(** Composable swap channels as reagents.

    [swap] is a proper reagent of type [('a, 'b) Reagent.t] that composes
    with other reagents.

    {2 Two-phase protocol}

    When [run] calls [try_react ... None] (phase 1):
    - If a partner is in the incoming queue: match and proceed.
    - Otherwise: return [Block] without posting anything. This lets
      [+] try alternatives.

    When [run] calls [try_react ... (Some offer)] (phase 2):
    - If a partner is in the incoming queue: match and proceed.
    - Otherwise: post a message with the offer and our pre-swap xt
      snapshot, return [Block]. A future partner will take the message,
      run our continuation, and fulfill the offer — delivering the
      final result to the waiting thread. *)

(* ──────────────────────────────────────────────────────────────────────────
   Messages and endpoints
   ────────────────────────────────────────────────────────────────────────── *)

(** An existentially-typed message posted to a channel.

    Contains:
    - The payload being sent.
    - The sender's continuation (typed ['b -> 'final] as a reagent).
    - The sender's offer, where the final result will be delivered.
    - A snapshot of the sender's xt log (pre-swap ops), which the
      matching partner will merge into their own transaction. *)
type ('a, 'b) message =
  | Message :
      'a
      * ('b, 'final) Reagent.t
      * 'final Reagent.offer
      * Xt.snapshot
    -> ('a, 'b) message

(** No mutex needed — fibers run cooperatively on a single domain, so
    queue access is implicitly serialized. *)
type ('a, 'b) endpoint = {
  outgoing : ('a, 'b) message Queue.t;
  incoming : ('b, 'a) message Queue.t;
}

let mk_chan () : ('a, 'b) endpoint * ('b, 'a) endpoint =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  ( { outgoing = q1; incoming = q2 },
    { outgoing = q2; incoming = q1 } )

(** Remove messages whose offers have already been fulfilled. *)
let clean (q : ('a, 'b) message Queue.t) : unit =
  let rec loop () =
    match Queue.peek_opt q with
    | Some (Message (_, _, offer, _))
      when not (Reagent.is_offer_pending offer) ->
      ignore (Queue.pop q); loop ()
    | _ -> ()
  in
  loop ()

(* ──────────────────────────────────────────────────────────────────────────
   Swap as a reagent
   ────────────────────────────────────────────────────────────────────────── *)

(** [swap_k dual_payload partner_offer] builds a reagent that "delivers"
    its input to [partner_offer] on commit, then continues with
    [dual_payload].

    Used when B matches A's posted message: B wraps A's continuation
    with this so A's final result gets routed to A's offer. *)
let swap_k :
  type a b. a -> b Reagent.offer -> (b, a) Reagent.t =
  fun dual_payload partner_offer ->
    Reagent.make_reagent (fun my_result xt ->
      Xt.add_post_commit ~xt (fun () ->
        ignore (Reagent.fulfill partner_offer my_result));
      Reagent.Done dual_payload)

(** [swap ep] is the channel swap reagent. *)
let swap (ep : ('a, 'b) endpoint) : ('a, 'b) Reagent.t =
  let rec build : type final.
      ('b, final) Reagent.t -> ('a, final) Reagent.t =
    fun k -> {
      try_react = (fun payload xt offer ->
        clean ep.incoming;
        match Queue.peek_opt ep.incoming with
        | Some (Message (partner_payload, partner_k, partner_offer, partner_snap)) ->
          ignore (Queue.pop ep.incoming);
          Xt.merge ~xt partner_snap;
          let merged =
            Reagent.(partner_k >> swap_k partner_payload partner_offer >> k)
          in
          merged.try_react payload xt offer
        | None ->
          (match offer with
           | None -> Reagent.Block
           | Some my_offer ->
             let snap = Xt.snapshot ~xt in
             let msg = Message (payload, k, my_offer, snap) in
             Queue.push msg ep.outgoing;
             Reagent.Block));
      seq = (fun k' -> build (Reagent.(k >> k')));
    }
  in
  let terminal = Reagent.make_reagent (fun b _xt -> Reagent.Done b) in
  build terminal
