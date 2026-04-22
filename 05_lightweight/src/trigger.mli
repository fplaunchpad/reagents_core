(** A one-shot signaling primitive for cooperative scheduling. *)

type t

val create : unit -> t
val signal : t -> bool
val on_signal : t -> (unit -> unit) -> bool
val await : t -> unit

type _ Effect.t += Await : t -> unit Effect.t
