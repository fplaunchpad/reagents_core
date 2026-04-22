(** Explicit transaction log for composable atomic operations.

    The Xt module provides an imperative transaction API on top of k-CAS.
    Within a transaction, reads and writes are logged. On commit, the log
    is converted to a k-CAS operation list (CMP for reads, CAS for writes)
    and committed atomically.

    Supports [retry] (block until a read-set location changes) and
    [or_else] (try an alternative transaction if the first retries).

    Inspired by GHC's STM: [retry] corresponds to Haskell's [retry],
    and [or_else] corresponds to Haskell's [orElse]. *)

(* ──────────────────────────────────────────────────────────────────────────
   Internal types
   ────────────────────────────────────────────────────────────────────────── *)

(** A single entry in the transaction log.

    Records the location, the value when first accessed (initial), and the
    current value within the transaction. On commit:
    - If initial = current → CMP (read-only, no write needed)
    - If initial ≠ current → CAS (loc, initial, current) *)
type entry = Entry : 'a Kcas.loc * 'a * 'a ref -> entry

(** The transaction log.

    [log]: CAS/CMP entries committed via k-CAS.
    [post_commit]: actions (e.g., offer fulfillments) to run after successful
    commit. Used by channels to deliver swapped values atomically with the
    transaction. *)
type t = {
  mutable log : entry list;
  mutable post_commit : (unit -> unit) list;
}

(** Raised by [retry] to signal "block until a read-set location changes." *)
exception Retry

(** [post_commit ~xt f] schedules [f ()] to run after the transaction
    successfully commits. If the transaction retries or or_else rolls back,
    [f] is discarded. *)
let add_post_commit ~(xt : t) (f : unit -> unit) : unit =
  xt.post_commit <- f :: xt.post_commit

(** An opaque snapshot of a transaction's log. Used by channel swap to
    transfer pre-swap ops from one thread to a matching partner. *)
type snapshot = {
  snap_log : entry list;
  snap_post_commit : (unit -> unit) list;
}

let snapshot ~(xt : t) : snapshot =
  { snap_log = xt.log; snap_post_commit = xt.post_commit }

(** [merge ~xt snap] prepends the snapshotted log entries into [xt].
    Used when a partner arrives at a channel — they absorb the other
    side's pre-swap ops into their own transaction. *)
let merge ~(xt : t) (snap : snapshot) : unit =
  xt.log <- snap.snap_log @ xt.log;
  xt.post_commit <- snap.snap_post_commit @ xt.post_commit

(** [install_awaiters snap cb] registers [cb] as an awaiter on every
    location in the snapshot's log. Used by reagent [run] to wake up when
    any read-set location changes. *)
let install_awaiters (snap : snapshot) (cb : unit -> unit) : unit =
  List.iter (fun (Entry (loc, _, _)) ->
    let rec try_add () =
      if not (Kcas.add_awaiter loc cb) then try_add ()
    in
    try_add ()
  ) snap.snap_log

(* ──────────────────────────────────────────────────────────────────────────
   Transaction operations
   ────────────────────────────────────────────────────────────────────────── *)

(** [get ~xt loc] reads the current transactional value of [loc].

    If [loc] was previously accessed in this transaction, returns the
    current (possibly written) value. Otherwise, reads from the location,
    records the initial value in the log, and returns it.

    The read is validated at commit time via CMP (or CAS if also written). *)
let get ~(xt : t) (loc : 'a Kcas.loc) : 'a =
  (* Search the log for an existing entry for this location. *)
  let rec find = function
    | [] ->
      (* First access — read the current value and log it. *)
      let v = Kcas.get loc in
      let current = ref v in
      xt.log <- Entry (loc, v, current) :: xt.log;
      v
    | Entry (loc', _, current') :: rest ->
      (* Compare locations by physical identity. The existential type
         means we need Obj.magic to recover the type — this is safe
         because we're comparing the same location. *)
      if Obj.repr loc == Obj.repr loc' then
        Obj.magic !current'
      else
        find rest
  in
  find xt.log

(** [set ~xt loc v] writes [v] to [loc] within the transaction.

    If [loc] was previously accessed, updates the current value in the log.
    Otherwise, reads the initial value first, then sets the current to [v].

    The write becomes a CAS(loc, initial, v) at commit time. *)
let set ~(xt : t) (loc : 'a Kcas.loc) (v : 'a) : unit =
  let rec find = function
    | [] ->
      (* First access — read initial, set current to v. *)
      let initial = Kcas.get loc in
      let current = ref v in
      xt.log <- Entry (loc, initial, current) :: xt.log
    | Entry (loc', _, current') :: rest ->
      if Obj.repr loc == Obj.repr loc' then
        (Obj.magic current') := v
      else
        find rest
  in
  find xt.log

(** [modify ~xt loc f] atomically applies [f] to the value in [loc]. *)
let modify ~xt loc f =
  let v = get ~xt loc in
  set ~xt loc (f v)

(** [update ~xt loc f] applies [f] and returns the old value. *)
let update ~xt loc f =
  let v = get ~xt loc in
  set ~xt loc (f v);
  v

(** [retry ()] aborts the current transaction and blocks until a read-set
    location changes. Caught by [commit], which registers awaiters and
    blocks the calling domain. *)
let retry () = raise Retry

(** [or_else tx1 tx2 ~xt] runs [tx1]. If it calls [retry], rolls back the
    log entries added by [tx1] and runs [tx2]. If both retry, the [Retry]
    propagates to [commit], which blocks on the entries from the prefix and
    [tx2] (entries from [tx1] were rolled back). *)
let or_else (tx1 : xt:t -> 'a) (tx2 : xt:t -> 'a) ~(xt : t) : 'a =
  let snap_log = xt.log in
  let snap_post = xt.post_commit in
  match tx1 ~xt with
  | v -> v
  | exception Retry ->
    xt.log <- snap_log;
    xt.post_commit <- snap_post;
    tx2 ~xt

(* ──────────────────────────────────────────────────────────────────────────
   Commit
   ────────────────────────────────────────────────────────────────────────── *)

(** Convert the transaction log to a k-CAS op list.
    - Entries where value unchanged → CMP (read-only validation)
    - Entries where value changed → CAS (read-write) *)
let ops_of_log (log : entry list) : Kcas.op list =
  List.map (fun (Entry (loc, initial, current)) ->
    if !current == initial then
      Kcas.CMP (loc, initial)
    else
      Kcas.CAS (loc, initial, !current)
  ) log

(** Build awaiter-registration functions from the log entries. *)
let locs_of_log (log : entry list) :
    (Kcas.awaiter -> bool) list =
  List.map (fun (Entry (loc, _, _)) ->
    fun awaiter -> Kcas.add_awaiter loc awaiter
  ) log

(** Block the current fiber until an awaiter fires on one of the logged
    locations, then return. Uses [Trigger.await] instead of Mutex/Condition. *)
let block_on_log (log : entry list) : unit =
  if log = [] then ()
  else begin
    let trigger = Trigger.create () in
    let awaiter () = ignore (Trigger.signal trigger) in
    let adders = locs_of_log log in
    List.iter (fun add ->
      let rec try_add () =
        if not (add awaiter) then try_add ()
      in
      try_add ()
    ) adders;
    Trigger.await trigger
  end

(** [commit tx] runs transaction [tx], then commits atomically via k-CAS.

    - On success: returns the result.
    - On transient failure (k-CAS interference): retries immediately.
    - On [retry]: registers awaiters on all read-set locations, blocks
      until one changes, then retries. *)
let commit (tx : xt:t -> 'a) : 'a =
  let rec loop () =
    let xt = { log = []; post_commit = [] } in
    match tx ~xt with
    | result ->
      let ops = ops_of_log xt.log in
      if Kcas.atomically ops then begin
        (* Run post-commit actions (e.g., channel offer fulfillments). *)
        List.iter (fun f -> f ()) xt.post_commit;
        result
      end
      else loop ()  (* transient failure, retry *)
    | exception Retry ->
      (* Block until a read-set location changes, then retry. *)
      block_on_log xt.log;
      loop ()
  in
  loop ()
