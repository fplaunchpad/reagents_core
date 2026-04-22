(** CPS-style reagent combinators with composable swap channels.

    Uses a two-phase offer protocol so that [swap + alternative] works
    (if swap has no partner, fall through to alternative; only post an
    offer and block if all alternatives would block). *)

(** {1 Offers (shared slots for delivering results)} *)

type 'a offer
(** An offer is where a channel partner (or successful commit) will
    deliver the final result. Created automatically by [run]. *)

type 'a offer_state =
  | Pending
  | Fulfilled of 'a
  | Signaled

val make_offer : unit -> 'a offer
val fulfill : 'a offer -> 'a -> bool
val signal_offer : 'a offer -> unit
val is_offer_pending : 'a offer -> bool
val await_offer : 'a offer -> 'a
val await_offer_state : 'a offer -> 'a offer_state

(** {1 Reagent type} *)

type 'a result =
  | Done of 'a       (** Success. *)
  | Block            (** Permanent failure: precondition not met. *)
  | Retry            (** Transient failure: retry. *)

type ('a, 'b) t = {
  try_react : 'a -> Xt.t -> 'b offer option -> 'b result;
  seq : 'c. ('b, 'c) t -> ('a, 'c) t;
}
(** A reagent from ['a] to ['b]. [try_react] runs the reagent; the
    optional [offer] is where the final ['b] will be delivered if the
    reagent blocks (e.g., swap posts the offer to a channel queue). *)

type 'a ref = 'a Kcas.loc

(** {1 Construction} *)

val make_reagent : ('a -> Xt.t -> 'b result) -> ('a, 'b) t
(** Build a reagent from a try function. *)

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
val (+) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val pair : ('a, 'c) t -> ('b, 'd) t -> ('a * 'b, 'c * 'd) t

(** {1 Execution} *)

val run : ('a, 'b) t -> 'a -> 'b
(** Two-phase execution: try without offer, then with offer if the first
    attempt blocked. *)

val run_opt : ('a, 'b) t -> 'a -> 'b option
(** Phase-1 only: returns [None] if the reagent would block. *)
