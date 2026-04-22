(** Composable swap channels.

    Unlike [04_channels], [swap] here is a proper reagent that composes
    with other reagent operations:

    {[
      (* Atomically: receive from ep, push onto stack. If either side
         cannot proceed, the whole thing blocks or retries. *)
      let recv_and_push ep stack =
        Reagent.(Channel.swap ep >> Stack.push stack)
    ]}

    When one endpoint is empty, the swap blocks until a partner arrives.
    When a partner arrives, the two transactions (each side's pre-swap and
    post-swap ops) commit atomically as a single k-CAS. *)

type ('a, 'b) endpoint
(** One end of a swap channel. Sending ['a], receiving ['b]. *)

val mk_chan : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
(** [mk_chan ()] creates a channel with two dual endpoints. *)

val swap : ('a, 'b) endpoint -> ('a, 'b) Reagent.t
(** [swap ep] is a reagent: takes ['a] as input, produces ['b]. Blocks
    if no partner is waiting on the dual endpoint. *)
