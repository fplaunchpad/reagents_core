(** k-CAS + CMP + awaiters.

    Extends [02_kcas_cmp] with an awaiter mechanism. After a successful
    {!atomically}, all awaiters registered on touched locations are fired.
    This is the foundation for {!Xt}'s [retry] blocking. *)

(** {1 Shared memory locations} *)

type 'a loc
(** A shared memory location with an associated awaiter list. *)

(** {1 Operations} *)

type op =
  | CAS : 'a loc * 'a * 'a -> op
  (** Read-write compare-and-swap. *)
  | CMP : 'a loc * 'a -> op
  (** Read-only compare (no write, no contention). *)

val make : 'a -> 'a loc
(** [make v] creates a new shared location initialized to [v]. *)

val get : 'a loc -> 'a
(** [get loc] reads the current logical value. Helps any active owning
    operation complete first. *)

val compare_and_set : 'a loc -> 'a -> 'a -> bool
(** Single-word compare-and-set. *)

val atomically : op list -> bool
(** Multi-word CAS with optional CMP assertions. On success, fires awaiters
    on all touched locations. *)

(** {1 Awaiters} *)

type awaiter = unit -> unit
(** A callback invoked when a location is modified by a successful {!atomically}. *)

val add_awaiter : 'a loc -> awaiter -> bool
(** [add_awaiter loc f] registers [f] to be called when [loc] is next modified.
    Returns [true] if registration succeeded, [false] if the awaiter list was
    concurrently modified (caller should retry). *)

val remove_awaiter : 'a loc -> awaiter -> unit
(** [remove_awaiter loc f] removes [f] from the awaiter list (physical identity). *)
