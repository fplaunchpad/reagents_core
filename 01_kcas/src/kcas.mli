(** Spinning k-CAS implementation (GKMZ algorithm).

    Implements the GKMZ algorithm from "Efficient Multi-word Compare and Swap"
    (Guerraoui, Kogan, Marathe, Zablotchi — arXiv:2008.02527, DISC 2020).

    Lock-free, linearizable, ABA-safe. Requires [k+1] single-word CAS
    operations for a [k]-word CAS in the uncontended case.

    {b Limitations:} No notification/awaiter mechanism. Callers that need to
    wait for state changes must busy-wait (spin). See [02_kcas_await] for
    an implementation with blocking support. *)

type 'a loc
type op = CAS : 'a loc * 'a * 'a -> op

val make : 'a -> 'a loc
val get : 'a loc -> 'a
val compare_and_set : 'a loc -> 'a -> 'a -> bool
val atomically : op list -> bool
