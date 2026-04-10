(** Swap channels — synchronous rendezvous between two domains.

    Two domains call [swap] on dual endpoints of a channel to exchange values.
    Blocking uses Mutex/Condition (domain-safe).

    {b Limitation:} [swap] is standalone — not composable within an [Xt]
    transaction. See the module documentation for details. *)

type ('a, 'b) endpoint

val mk_chan : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
(** Create a channel. Returns dual endpoints. *)

val swap : ('a, 'b) endpoint -> 'a -> 'b
(** [swap ep v] sends [v] and blocks until a partner sends on the dual
    endpoint. Returns the partner's value. *)
