(** CPS-style reagent combinators with composable swap channels.

    Algorithm: every primitive's chain ends in a first-class [commit]
    reagent (à la Turon, PLDI 2012). On a kCAS conflict, [commit]
    surfaces [Retry] from [try_react] so the [+] combinator can dispatch
    to the alternative branch. This is what makes the elimination-stack
    pattern [push s + swap elim] actually use elimination under contention.

    Run uses the two-phase offer protocol: phase 1 without offer (try
    synchronously); phase 2 with offer, posting it to channels that
    block. *)

(** {1 Offers} *)

(** An offer is a shared slot transitioning [Empty → Completed v] (by a
    channel partner's commit) or [Empty → Rescinded] (by us cancelling,
    or by a read-set awaiter signaling a precondition change).

    The state lives in a [Kcas.loc] so partner fulfilment is part of the
    same kCAS as the rest of the swap — atomic, lock-free. The waiting
    thread blocks on a Mutex+Condition wrapped around the loc, woken by
    a kCAS awaiter installed on the loc. *)
type 'a offer_state =
  | Empty
  | Completed of 'a
  | Rescinded

type 'a offer
(** Abstract; create via [make_offer]. *)

val make_offer : unit -> 'a offer
val offer_state_loc : 'a offer -> 'a offer_state Kcas.loc
(** Exposed so channels can include the offer-completion CAS in a
    transaction. *)

val is_offer_active : 'a offer -> bool
(** True iff the offer is still [Empty] (eligible to be matched). *)

val signal_offer : 'a offer -> unit
(** Try to rescind the offer (CAS Empty→Rescinded). No-op if already
    Completed/Rescinded. Used as a read-set awaiter callback so any
    blocked precondition change wakes the offer. *)

(** {1 Reagent type} *)

type 'a result =
  | Done of 'a
  | Block            (** Permanent failure: precondition not met. *)
  | Retry            (** Transient failure: kCAS lost or interfered. *)
  | BlockAndRetry    (** Mixed: one branch blocks, another retries. *)

type ('a, 'b) t = {
  try_react : 'a -> Xt.t -> 'b offer option -> 'b result;
  seq : 'c. ('b, 'c) t -> ('a, 'c) t;
  always_commits : bool;
}

type 'a ref = 'a Kcas.loc

(** {1 Construction} *)

val make_reagent : ('a -> Xt.t -> 'b result) -> ('a, 'b) t
(** Build a reagent from a body function. The resulting reagent's chain
    ends in a [commit] primitive at construction time. The body should
    only return [Done]/[Block]/[Retry] (never [BlockAndRetry]; that's
    only producible by [+]). *)

val commit : ('a, 'a) t
(** The commit primitive. Exposed for advanced cases (e.g., custom
    reagents like [swap] that build their own [try_react] without going
    through [make_reagent]). [commit.seq next = next] (it absorbs
    itself), so the chain still ends in exactly one commit at the
    rightmost primitive. *)

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
(** Choice. Snapshots [Xt] before [r1] and rolls back on
    [Block]/[Retry]/[BlockAndRetry] before trying [r2]. Combines results
    per the reference's table — in particular [(Block,Retry)] and
    [(Retry,Block)] both yield [BlockAndRetry], which the run loop uses
    to enter phase 2.

    Invariant: composition distributes via [seq]: [(r1 + r2).seq k =
    (r1.seq k) + (r2.seq k)]. This guarantees each branch ends in its
    own commit at its own tail, so the snapshot/restore covers a
    complete branch. Any new combinator nested under [+] must preserve
    this distribution. *)

(** Note: [pair] / [<*>] is intentionally omitted. The current
    snapshot-before-each-branch-with-tail-commit design is incompatible
    with running two reagents independently and combining their results;
    a faithful CPS [pair] would require [first]/[second] combinators
    (cf. reference [core.ml]'s [<*>]). Deferred. *)

(** {1 Execution} *)

val run : ('a, 'b) t -> 'a -> 'b
(** Two-phase execution: try without offer; on Block/BlockAndRetry,
    enter phase 2 with a fresh offer; on Block in phase 2, install
    read-set awaiters and wait. *)

val run_opt : ('a, 'b) t -> 'a -> 'b option
(** Phase-1 only: returns [None] if the reagent would block. *)
