(** Spinning k-CAS implementation (GKMZ algorithm).

    Implements the GKMZ algorithm from "Efficient Multi-word Compare and Swap"
    (Guerraoui, Kogan, Marathe, Zablotchi — arXiv:2008.02527, DISC 2020).

    Lock-free, linearizable, ABA-safe. Requires [k+1] single-word CAS
    operations for a [k]-word CAS in the uncontended case.

    {b Limitations:} No notification/awaiter mechanism. Callers that need to
    wait for state changes must busy-wait (spin).

    {2 Example}

    {[
      let a = Kcas.make 1
      let b = Kcas.make 2

      (* Atomically swap a and b: *)
      let ok = Kcas.atomically [CAS (a, 1, 2); CAS (b, 2, 1)]
      (* ok = true, now a=2, b=1 *)
    ]} *)

(** {1 Shared memory locations} *)

type 'a loc
(** A shared memory location holding a value of type ['a]. Internally stores
    a state descriptor that the GKMZ algorithm uses for lock-free coordination. *)

(** {1 Operations} *)

type op = CAS : 'a loc * 'a * 'a -> op
(** A single compare-and-swap within a k-CAS operation.
    [CAS (loc, expected, desired)]: if [loc] holds [expected], write [desired]. *)

val make : 'a -> 'a loc
(** [make v] creates a new shared location initialized to [v]. *)

val get : 'a loc -> 'a
(** [get loc] reads the current logical value from [loc]. If [loc] is owned by
    an active k-CAS operation, [get] helps that operation complete first — this
    is the cooperative helping mechanism that ensures lock-freedom. *)

val compare_and_set : 'a loc -> 'a -> 'a -> bool
(** [compare_and_set loc expected desired] performs a single-word CAS.
    Returns [true] if [loc] held [expected] and was updated to [desired].
    Implemented as a 1-word k-CAS via {!atomically}. *)

val atomically : op list -> bool
(** [atomically ops] performs a multi-word compare-and-swap. Atomically: if
    every location in [ops] holds its expected value, update all of them to
    their desired values and return [true]. Otherwise, make no changes and
    return [false]. *)
