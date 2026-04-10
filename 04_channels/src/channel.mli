(** Swap channels — synchronous rendezvous between two domains.

    A channel connects two endpoints. When domain A calls {!swap} on one
    endpoint and domain B calls {!swap} on the dual endpoint, they exchange
    values atomically. Blocking uses [Mutex]/[Condition] (domain-safe).

    {b Limitation:} {!swap} is a standalone blocking operation — not composable
    within an {!Xt} transaction. Atomically combining a channel swap with a CAS
    would require the CPS reagent structure from the original reagents paper.

    {2 Example}

    {[
      let ep1, ep2 = Channel.mk_chan () in

      let d = Domain.spawn (fun () -> Channel.swap ep1 42) in
      let got = Channel.swap ep2 99 in
      (* got = 42, Domain.join d = 99 *)
    ]} *)

type ('a, 'b) endpoint
(** One end of a swap channel. *)

val mk_chan : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
(** [mk_chan ()] creates a channel, returning dual endpoints. *)

val swap : ('a, 'b) endpoint -> 'a -> 'b
(** [swap ep v] sends [v] on [ep] and blocks until a partner calls [swap]
    on the dual endpoint. Returns the partner's value. *)
