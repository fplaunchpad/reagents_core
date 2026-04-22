(** CPS-style reagent combinators with composable swap channels.

    Unlike [03_stm]'s simpler reagent type ['a -> Xt.t -> 'b], this
    reagent is a record that carries its continuation via the [seq]
    field. This enables composable channel [swap] — see {!Channel.swap}. *)

(** {1 Types} *)

type 'a result =
  | Done of 'a       (** Success. *)
  | Block            (** Permanent failure: precondition not met. *)
  | Retry            (** Transient failure: retry. *)

type ('a, 'b) t = {
  try_react : 'a -> Xt.t -> 'b result;
  seq : 'c. ('b, 'c) t -> ('a, 'c) t;
}
(** A reagent from ['a] to ['b].

    [try_react] runs the reagent, accumulating CAS/CMP ops in the [Xt] log.
    [seq k] composes the reagent with continuation [k] — this is what makes
    channel swap composable. *)

type 'a ref = 'a Kcas.loc

(** {1 Construction} *)

val make_reagent : ('a -> Xt.t -> 'b result) -> ('a, 'b) t
(** [make_reagent f] builds a reagent from a try function, generating
    the [seq] field automatically. *)

(** {1 Ref operations} *)

val ref : 'a -> 'a ref
val get : 'a ref -> 'a
val read : 'a ref -> (unit, 'a) t
val upd : 'a ref -> ('a -> 'b -> ('a * 'c) option) -> ('b, 'c) t
val cas : 'a ref -> 'a -> 'a -> (unit, unit) t
val modify : 'a ref -> ('a -> 'a) -> (unit, unit) t
val set : 'a ref -> 'a -> (unit, unit) t
val constant : 'b -> ('a, 'b) t
val lift : ('a -> 'b) -> ('a, 'b) t

(** {1 Combinators} *)

val (>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** Sequential composition via [seq]. *)

val (+) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
(** Choice: try first; on Block, try second. *)

val pair : ('a, 'c) t -> ('b, 'd) t -> ('a * 'b, 'c * 'd) t

(** {1 Execution} *)

val run : ('a, 'b) t -> 'a -> 'b
val run_opt : ('a, 'b) t -> 'a -> 'b option
