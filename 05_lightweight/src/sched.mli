(** Cooperative round-robin scheduler with {!Trigger} support.

    Runs fibers (lightweight threads) cooperatively on a single domain.
    Handles [Fork], [Yield], and [Trigger.Await] effects.

    {2 Example}

    {[
      Sched.run (fun () ->
        Sched.fork (fun () -> Printf.printf "fiber 1\n");
        Sched.fork (fun () -> Printf.printf "fiber 2\n");
        Printf.printf "main fiber\n")
    ]} *)

val fork : (unit -> unit) -> unit
(** [fork f] spawns [f] as a new fiber. The current fiber continues;
    [f] is added to the run queue. *)

val yield : unit -> unit
(** [yield ()] suspends the current fiber and switches to the next
    runnable fiber in the queue. *)

val run : (unit -> unit) -> unit
(** [run main] runs [main] and all forked fibers cooperatively.
    Returns when no more fibers are runnable. Handles {!Trigger.Await}
    by registering a callback that re-enqueues the blocked fiber. *)
