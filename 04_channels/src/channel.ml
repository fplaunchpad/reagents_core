(** Composable swap channels as reagents.

    Unlike [04_channels] where [swap] was a standalone blocking function,
    here [swap] is a proper reagent: [('a, 'b) Reagent.t]. This means you
    can compose it with other reagents:

    {[
      (* Atomically receive from channel and push to stack: *)
      let recv_and_push ep s = Reagent.(Channel.swap ep >> push s)
    ]}

    The implementation follows the original reagents paper (Turon 2012).
    When thread A runs [swap >> k]:
    - A builds up Xt ops and reaches the swap
    - If no partner is waiting: A posts a message containing
      [(payload_A, k, xt_snapshot, offer_A)] and blocks
    - A partner thread B finds A's message:
      - B takes A's payload and builds a reagent [merged = k.seq (swap_k payload_A offer_A)]
      - This reagent, when run with B's payload, will:
        1. Run [k] (A's continuation) with B's payload, producing A's result
        2. Register a post-commit action to fulfill A's offer with A's result
        3. Commit atomically
      - B runs [merged] as its own continuation *)

(* ──────────────────────────────────────────────────────────────────────────
   Offers
   ────────────────────────────────────────────────────────────────────────── *)

(** An offer is a slot where a partner will deliver a value, plus a
    mechanism to wake the waiting thread. *)
type 'a offer = {
  mutable result : 'a option;
  mutex : Mutex.t;
  cond : Condition.t;
  mutable fulfilled : bool;
}

let make_offer () : 'a offer = {
  result = None;
  mutex = Mutex.create ();
  cond = Condition.create ();
  fulfilled = false;
}

(** Fulfill an offer with a value. Called atomically from a partner's
    commit. Returns [true] on first fulfillment, [false] if already done. *)
let fulfill (offer : 'a offer) (v : 'a) : bool =
  Mutex.lock offer.mutex;
  let ok =
    if offer.fulfilled then false
    else begin
      offer.result <- Some v;
      offer.fulfilled <- true;
      Condition.signal offer.cond;
      true
    end
  in
  Mutex.unlock offer.mutex;
  ok

let is_pending (offer : 'a offer) : bool =
  not offer.fulfilled

(** Block until the offer is fulfilled. *)
let await (offer : 'a offer) : 'a =
  Mutex.lock offer.mutex;
  while not offer.fulfilled do
    Condition.wait offer.cond offer.mutex
  done;
  Mutex.unlock offer.mutex;
  match offer.result with
  | Some v -> v
  | None -> failwith "Channel.await: fulfilled but no result"

(* ──────────────────────────────────────────────────────────────────────────
   Messages and endpoints
   ────────────────────────────────────────────────────────────────────────── *)

(** An existentially-typed message posted to an outgoing channel queue.

    Contains:
    - The payload being sent.
    - The sender's continuation ([k : ('b, 'final) Reagent.t]) — what the
      sender wants to do after receiving a ['b] from the partner.
    - The sender's offer — where the final result goes. *)
type ('a, 'b) message =
  | Message : 'a * ('b, 'final) Reagent.t * 'final offer -> ('a, 'b) message

(** A channel endpoint. [outgoing] is where we post; [incoming] is where
    we look for partners. *)
type ('a, 'b) endpoint = {
  outgoing : ('a, 'b) message Queue.t;
  incoming : ('b, 'a) message Queue.t;
  lock : Mutex.t;
}

let mk_chan () : ('a, 'b) endpoint * ('b, 'a) endpoint =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let lock = Mutex.create () in
  ( { outgoing = q1; incoming = q2; lock },
    { outgoing = q2; incoming = q1; lock } )

(** Remove non-pending (fulfilled) messages from the front of a queue. *)
let clean (q : ('a, 'b) message Queue.t) : unit =
  let rec loop () =
    match Queue.peek_opt q with
    | Some (Message (_, _, offer)) when not (is_pending offer) ->
      ignore (Queue.pop q); loop ()
    | _ -> ()
  in
  loop ()

(* ──────────────────────────────────────────────────────────────────────────
   Swap as a reagent
   ────────────────────────────────────────────────────────────────────────── *)

(** Helper: build a reagent that "delivers" a value to a partner's offer
    on commit, then continues with [k] applied to [dual_payload].

    When a partner B finds A's message, B wraps A's continuation:
    [merged = k_A.seq (swap_k payload_B offer_A)].
    Running [merged] with A's payload-for-A will:
    1. Run [k_A] with B's payload — that's what A expected to receive
    2. Take the output, schedule it for delivery to A's offer post-commit
    3. Continue with the enclosing reagent (typically terminating in commit) *)
let swap_k : type a b. a -> b offer -> (b, a) Reagent.t =
  fun dual_payload partner_offer ->
    Reagent.make_reagent (fun my_result xt ->
      (* [my_result] is the output of A's continuation k_A run with B's payload.
         That's A's final result — schedule delivery to A's offer. *)
      Xt.add_post_commit ~xt (fun () ->
        ignore (fulfill partner_offer my_result));
      (* And return [dual_payload] = A's payload = what B wants to receive. *)
      Reagent.Done dual_payload)

(** [swap ep] is a reagent: send value of type ['a], receive ['b].

    When run with input [a] (what we want to send):
    - Search the incoming queue for a matching partner.
    - If found: take partner's payload ['b], run the rest of our reagent
      (our continuation). The partner's final result was already posted
      for delivery via post-commit.
    - If not found: post a message containing [(a, my_continuation, my_offer)]
      and block until fulfilled. *)
let swap (ep : ('a, 'b) endpoint) : ('a, 'b) Reagent.t =
  (* We need to construct a reagent that, when [seq]'d with some continuation
     [k], can access [k] to pass it to the partner. The trick: we build
     the reagent's [try_react] to close over [k], and override [seq] to
     rebuild with a new [k]. *)
  let rec build : type final. ('b, final) Reagent.t -> ('a, final) Reagent.t =
    fun k -> {
      try_react = (fun payload xt ->
        Mutex.lock ep.lock;
        clean ep.incoming;
        match Queue.peek_opt ep.incoming with
        | Some (Message (partner_payload, partner_k, partner_offer)) ->
          (* Found a partner. Remove their message (we'll fulfill their offer). *)
          ignore (Queue.pop ep.incoming);
          Mutex.unlock ep.lock;
          (* Build the merged reagent: run partner's continuation with our
             payload, deliver partner's result to their offer, then run our
             continuation with partner's payload. *)
          let merged =
            Reagent.(partner_k >> swap_k partner_payload partner_offer >> k)
          in
          merged.try_react payload xt
        | None ->
          (* No partner. Post our message and block. *)
          let offer = make_offer () in
          let msg = Message (payload, k, offer) in
          Queue.push msg ep.outgoing;
          Mutex.unlock ep.lock;
          (* Commit pre-swap ops, then block. Tricky: we can't return Done
             without a value, and we can't commit yet because the partner
             needs to see our xt log. For simplicity, we block inside
             try_react — the thread waits, and when woken, we return Done.
             Note: this means ops *after* the swap only commit once the
             partner matches (they'll be run by the partner as our k). *)
          let result = await offer in
          Done result);
      seq = fun k' -> build (Reagent.(k >> k'));
    }
  in
  (* Seed with a "terminal" continuation that just returns its input. *)
  let terminal = Reagent.make_reagent (fun b _xt -> Reagent.Done b) in
  build terminal
