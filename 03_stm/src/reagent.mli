(** Reagent combinators — a functional wrapper over {!Xt}.

    A reagent [('a, 'b) t] is just a function [fun (a : 'a) (xt : Xt.t) -> ... : 'b].
    Sequencing is function composition. Choice is {!Xt.or_else}. Blocking is
    {!Xt.retry}.

    {2 Example}

    {[
      let push s = Reagent.upd s (fun xs x -> Some (x :: xs, ()))
      let try_pop s = Reagent.upd s (fun xs () ->
        match xs with [] -> None | x :: xs' -> Some (xs', x))

      (* Atomically pop from s1 and push to s2: *)
      let transfer s1 s2 = Reagent.(try_pop s1 >> push s2)
      let () = Reagent.run (transfer s1 s2) ()
    ]} *)

(** {1 Types} *)

type 'a ref = 'a Kcas.loc
(** A mutable shared reference cell, backed by a k-CAS location. *)

type ('a, 'b) t = 'a -> Xt.t -> 'b
(** A reagent: takes an input of type ['a] and a transaction log, produces ['b].
    All reads and writes go through the log and are committed atomically. *)

(** {1 Ref cell operations} *)

val ref : 'a -> 'a ref
(** [ref v] creates a new reference cell initialized to [v]. *)

val get : 'a ref -> 'a
(** [get r] reads the current value of [r] directly (outside a transaction).
    Not validated at commit time — use {!read} inside a reagent. *)

val read : 'a ref -> (unit, 'a) t
(** [read r] reads [r] within a transaction (validated at commit via CMP). *)

val upd : 'a ref -> ('a -> 'b -> ('a * 'c) option) -> ('b, 'c) t
(** [upd r f] is the fundamental update reagent. Reads current value [a] from
    [r], calls [f a b] where [b] is the input. If [f] returns [Some (a', c)],
    sets [r] to [a'] and produces [c]. If [None], calls {!Xt.retry}. *)

val cas : 'a ref -> 'a -> 'a -> (unit, unit) t
(** [cas r expected desired] sets [r] to [desired] if it holds [expected],
    otherwise retries. *)

val modify : 'a ref -> ('a -> 'a) -> (unit, unit) t
(** [modify r f] atomically applies [f] to the value in [r]. *)

val set : 'a ref -> 'a -> (unit, unit) t
(** [set r v] atomically sets [r] to [v]. *)

(** {1 Combinators} *)

val (>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** [r1 >> r2] sequences two reagents within the same atomic transaction.
    The output of [r1] becomes the input to [r2]. *)

val (+) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
(** [r1 + r2] chooses between two reagents. Tries [r1]; if it retries,
    rolls back and tries [r2]. Corresponds to {!Xt.or_else}. *)

val constant : 'b -> ('a, 'b) t
(** [constant v] ignores its input and always produces [v]. *)

val lift : ('a -> 'b) -> ('a, 'b) t
(** [lift f] applies pure function [f] to the input. No transactional ops. *)

val pair : ('a, 'c) t -> ('b, 'd) t -> ('a * 'b, 'c * 'd) t
(** [pair r1 r2] runs both reagents on a pair input, producing a pair output. *)

(** {1 Execution} *)

val run : ('a, 'b) t -> 'a -> 'b
(** [run r a] executes reagent [r] with input [a]. Commits atomically via
    k-CAS. On transient failure, retries. On {!Xt.retry}, blocks until a
    read-set location changes. *)

val run_opt : ('a, 'b) t -> 'a -> 'b option
(** [run_opt r a] executes [r]. Returns [Some b] on success, [None] if the
    reagent retries (i.e., its precondition is not met). *)
