(** Reagent combinators — a functional wrapper over [Xt].

    A reagent is just [type ('a, 'b) t = 'a -> Xt.t -> 'b]. *)

type 'a ref = 'a Kcas.loc
type ('a, 'b) t = 'a -> Xt.t -> 'b

val ref : 'a -> 'a ref
val get : 'a ref -> 'a
val read : 'a ref -> (unit, 'a) t
val upd : 'a ref -> ('a -> 'b -> ('a * 'c) option) -> ('b, 'c) t
val cas : 'a ref -> 'a -> 'a -> (unit, unit) t
val modify : 'a ref -> ('a -> 'a) -> (unit, unit) t
val set : 'a ref -> 'a -> (unit, unit) t

val (>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val (+) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val constant : 'b -> ('a, 'b) t
val lift : ('a -> 'b) -> ('a, 'b) t
val pair : ('a, 'c) t -> ('b, 'd) t -> ('a * 'b, 'c * 'd) t

val run : ('a, 'b) t -> 'a -> 'b
val run_opt : ('a, 'b) t -> 'a -> 'b option
