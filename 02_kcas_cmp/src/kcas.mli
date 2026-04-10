(** k-CAS with CMP extension (GKMZ algorithm).

    Extends [01_kcas] with read-only CMP (compare) operations.

    - CAS writes to the location (installs a descriptor during acquire).
    - CMP only asserts the location holds an expected value (no write).

    CMP operations are obstruction-free: they don't contend with each other.
    Two transactions that only read (CMP) overlapping locations can proceed
    in parallel. If a CMP location changes between the snapshot and the
    commit, the operation fails and can be retried. *)

type 'a loc

type op =
  | CAS : 'a loc * 'a * 'a -> op
  | CMP : 'a loc * 'a -> op

val make : 'a -> 'a loc
val get : 'a loc -> 'a
val compare_and_set : 'a loc -> 'a -> 'a -> bool
val atomically : op list -> bool
