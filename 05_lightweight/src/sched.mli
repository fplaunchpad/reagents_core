(** Cooperative round-robin scheduler with Trigger support. *)

val fork : (unit -> unit) -> unit
val yield : unit -> unit
val run : (unit -> unit) -> unit
