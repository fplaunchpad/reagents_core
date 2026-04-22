(** Explicit transaction log with retry and orElse.

    Provides an imperative STM API on top of k-CAS. Within a transaction,
    reads and writes are logged. On commit, the log is atomically applied.

    {2 Example}

    {[
      let a = Kcas.make 10
      let b = Kcas.make 20

      (* Atomically read both and write their sum to a third location *)
      let c = Kcas.make 0
      let () = Xt.commit (fun ~xt ->
        let va = Xt.get ~xt a in
        let vb = Xt.get ~xt b in
        Xt.set ~xt c (va + vb))
    ]}

    {2 retry and or_else}

    {[
      (* Block until a > 0, then decrement *)
      let decr_when_positive loc = Xt.commit (fun ~xt ->
        let v = Xt.get ~xt loc in
        if v <= 0 then Xt.retry ();
        Xt.set ~xt loc (v - 1))

      (* Try to pop from s1; if empty, pop from s2 *)
      let pop_either s1 s2 = Xt.commit (fun ~xt ->
        Xt.or_else
          (fun ~xt -> let v = Xt.get ~xt s1 in ...)
          (fun ~xt -> let v = Xt.get ~xt s2 in ...)
          ~xt)
    ]} *)

type t
(** A transaction log. *)

val get : xt:t -> 'a Kcas.loc -> 'a
(** Read a location within the transaction (validated at commit). *)

val set : xt:t -> 'a Kcas.loc -> 'a -> unit
(** Write a location within the transaction. *)

val modify : xt:t -> 'a Kcas.loc -> ('a -> 'a) -> unit
(** [modify ~xt loc f] applies [f] to the value in [loc]. *)

val update : xt:t -> 'a Kcas.loc -> ('a -> 'a) -> 'a
(** [update ~xt loc f] applies [f] and returns the old value. *)

val retry : unit -> 'a
(** Abort and block until a read-set location changes. *)

val or_else : (xt:t -> 'a) -> (xt:t -> 'a) -> xt:t -> 'a
(** [or_else tx1 tx2 ~xt]: run [tx1]; if it retries, roll back and run [tx2]. *)

val commit : (xt:t -> 'a) -> 'a
(** Run a transaction and commit atomically. Retries on failure. *)

val add_post_commit : xt:t -> (unit -> unit) -> unit
(** [add_post_commit ~xt f] schedules [f ()] to run after the transaction
    successfully commits. If the transaction retries or or_else rolls back,
    [f] is discarded. Used by channels to deliver swapped values atomically. *)
