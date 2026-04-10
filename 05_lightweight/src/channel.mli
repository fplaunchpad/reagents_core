(** Swap channels using fiber-level suspension via Trigger.

    Must run within [Sched.run]. Uses [Trigger.await] to suspend the
    calling fiber instead of blocking the OS thread. *)

type ('a, 'b) endpoint

val mk_chan : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
val swap : ('a, 'b) endpoint -> 'a -> 'b
