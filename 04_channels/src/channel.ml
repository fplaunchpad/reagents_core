(** Composable swap channels as reagents.

    [swap] is a proper reagent of type [('a, 'b) Reagent.t] that
    composes with other reagents.

    {2 Two-phase protocol}

    When [run] calls [try_react ... None] (phase 1):
    - If a partner is in the incoming queue: match and proceed.
    - Otherwise: return [Block] without posting. This lets [+] try
      alternatives.

    When [run] calls [try_react ... (Some offer)] (phase 2):
    - If a partner is in the incoming queue: match and proceed.
    - Otherwise: post a message with the offer and our pre-swap xt
      snapshot, return [Block]. A future partner will take the message,
      run our continuation, and CAS the offer to [Completed] as part of
      their commit's kCAS.

    {2 Atomic offer fulfilment}

    The partner's [swap_k] reads our offer's state loc into the xt and
    sets it to [Completed v]. At the partner's commit, this becomes a
    CAS Empty→Completed bundled into the same kCAS as the rest of the
    swap. So either the entire exchange commits and the offer is
    fulfilled, or nothing happens. No post-commit window. *)

(* ──────────────────────────────────────────────────────────────────────────
   Messages and endpoints
   ────────────────────────────────────────────────────────────────────────── *)

(** An existentially-typed message in a channel queue.

    Carries:
    - the payload being sent
    - the sender's continuation (typed ['b -> 'final] as a reagent)
    - the sender's offer loc (where the final result will land via CAS)
    - a snapshot of the sender's xt log (pre-swap reads/writes), which
      a matching partner merges into their own transaction. *)
type ('a, 'b) message =
  | Message :
      'a
      * ('b, 'final) Reagent.t
      * 'final Reagent.offer
      * Xt.snapshot
    -> ('a, 'b) message

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

(** Drop messages from the head whose offers are no longer Empty
    (someone fulfilled or rescinded them already). *)
let clean (q : ('a, 'b) message Queue.t) : unit =
  let rec loop () =
    match Queue.peek_opt q with
    | Some (Message (_, _, offer, _))
      when not (Reagent.is_offer_active offer) ->
      ignore (Queue.pop q); loop ()
    | _ -> ()
  in
  loop ()

(* ──────────────────────────────────────────────────────────────────────────
   Swap as a reagent
   ────────────────────────────────────────────────────────────────────────── *)

(** [swap_k dual_payload partner_offer] is the receiver-side wrapper:
    given the partner's continuation result [my_result], CAS the
    partner's offer Empty→Completed in the current transaction, then
    proceed with [dual_payload] to the receiver's own continuation.

    Reading the offer state loc records a CMP at the read value; setting
    it records a CAS to Completed. Both end up in the kCAS at commit. If
    the offer's state isn't Empty (rescinded by sender or fulfilled by
    another receiver), return [Retry] so the receiver retries cleanly. *)
let swap_k :
  type a b. a -> b Reagent.offer -> (b, a) Reagent.t =
  fun dual_payload partner_offer ->
    let loc = Reagent.offer_state_loc partner_offer in
    Reagent.make_reagent (fun my_result xt ->
      match Xt.get ~xt loc with
      | Reagent.Empty ->
        Xt.set ~xt loc (Reagent.Completed my_result);
        Reagent.Done dual_payload
      | Reagent.Completed _ | Reagent.Rescinded ->
        Reagent.Retry)

(** [swap ep] is the channel swap reagent. *)
let swap (ep : ('a, 'b) endpoint) : ('a, 'b) Reagent.t =
  let rec build : type final.
      ('b, final) Reagent.t -> ('a, final) Reagent.t =
    fun k -> {
      try_react = (fun payload xt offer ->
        Mutex.lock ep.lock;
        clean ep.incoming;
        match Queue.peek_opt ep.incoming with
        | Some (Message (partner_payload, partner_k, partner_offer, partner_snap)) ->
          ignore (Queue.pop ep.incoming);
          Mutex.unlock ep.lock;
          (* Merge partner's pre-swap xt entries into ours. *)
          Xt.merge ~xt partner_snap;
          (* Build the merged reagent: run partner's continuation with
             our payload, deliver partner's result to their offer (CAS
             in the kCAS), then run our continuation with partner's
             payload. *)
          let merged =
            Reagent.(partner_k >> swap_k partner_payload partner_offer >> k)
          in
          merged.try_react payload xt offer
        | None ->
          (* No partner. With an offer, post for a future partner.
             Without one, return Block so [+] can try alternatives. *)
          (match offer with
           | None ->
             Mutex.unlock ep.lock;
             Reagent.Block
           | Some my_offer ->
             let snap = Xt.snapshot ~xt in
             let msg = Message (payload, k, my_offer, snap) in
             Queue.push msg ep.outgoing;
             Mutex.unlock ep.lock;
             Reagent.Block));
      seq = (fun k' -> build (Reagent.(k >> k')));
      always_commits = false;
    }
  in
  let terminal = Reagent.make_reagent (fun b _xt -> Reagent.Done b) in
  build terminal
