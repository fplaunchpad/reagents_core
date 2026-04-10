(** k-CAS with CMP extension (GKMZ algorithm).

    Extends [01_kcas] with read-only CMP (compare) operations.

    - CAS writes to the location (installs a descriptor during acquire).
    - CMP only asserts the location holds an expected value (no write).

    CMP operations are obstruction-free: they don't contend with each other.
    Two transactions that only read (CMP) overlapping locations can proceed
    in parallel. If a CMP location changes between the snapshot and the
    commit, the operation fails and can be retried.

    {2 Example}

    {[
      let a = Kcas.make 10
      let b = Kcas.make 0

      (* Atomically: assert a=10 (read-only), set b:=20 *)
      let ok = Kcas.atomically [CMP (a, 10); CAS (b, 0, 20)]
      (* ok = true, a=10 unchanged, b=20 *)
    ]} *)

(** {1 Shared memory locations} *)

type 'a loc
(** A shared memory location holding a value of type ['a]. *)

(** {1 Operations} *)

type op =
  | CAS : 'a loc * 'a * 'a -> op
  (** [CAS (loc, expected, desired)]: read-write. If [loc] holds [expected],
      write [desired]. Installs a descriptor during the acquire phase. *)
  | CMP : 'a loc * 'a -> op
  (** [CMP (loc, expected)]: read-only. Asserts [loc] holds [expected] at commit
      time. Never writes to the location — multiple CMPs on the same location
      from different operations proceed without contention. *)

val make : 'a -> 'a loc
(** [make v] creates a new shared location initialized to [v]. *)

val get : 'a loc -> 'a
(** [get loc] reads the current logical value from [loc]. Helps any active
    owning operation complete first (cooperative helping). *)

val compare_and_set : 'a loc -> 'a -> 'a -> bool
(** [compare_and_set loc expected desired] performs a single-word CAS. *)

val atomically : op list -> bool
(** [atomically ops] performs a multi-word compare-and-swap, optionally with
    read-only CMP assertions. Returns [true] if all CAS preconditions held and
    all CMP assertions were valid at commit time. *)
