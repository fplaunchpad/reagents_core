(** k-CAS + CMP + awaiters. The foundation for the Xt transaction API. *)

type 'a loc
type awaiter = unit -> unit

type op =
  | CAS : 'a loc * 'a * 'a -> op
  | CMP : 'a loc * 'a -> op

val make : 'a -> 'a loc
val get : 'a loc -> 'a
val compare_and_set : 'a loc -> 'a -> 'a -> bool
val atomically : op list -> bool

val add_awaiter : 'a loc -> awaiter -> bool
val remove_awaiter : 'a loc -> awaiter -> unit
