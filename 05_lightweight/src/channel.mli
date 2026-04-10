(** Swap channels using fiber-level suspension via {!Trigger}.

    Same semantics as [04_channels/channel.ml], but blocking uses
    {!Trigger.await} instead of [Mutex]/[Condition]. Must run within
    {!Sched.run}.

    {2 Example}

    {[
      Sched.run (fun () ->
        let ep1, ep2 = Channel.mk_chan () in
        Sched.fork (fun () ->
          let got = Channel.swap ep1 42 in
          Printf.printf "fiber 1 got %d\n" got);
        let got = Channel.swap ep2 99 in
        Printf.printf "fiber 2 got %d\n" got)
    ]} *)

type ('a, 'b) endpoint
(** One end of a swap channel. *)

val mk_chan : unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
(** [mk_chan ()] creates a channel, returning dual endpoints.
    Sending ['a] on the first endpoint receives ['b] from the second,
    and vice versa. *)

val swap : ('a, 'b) endpoint -> 'a -> 'b
(** [swap ep v] sends [v] on [ep] and suspends the current fiber until a
    partner calls [swap] on the dual endpoint. Returns the partner's value.
    Must be called within {!Sched.run}. *)
