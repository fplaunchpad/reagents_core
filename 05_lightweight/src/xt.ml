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

(** The transaction log. *)
type t = {
  mutable log : entry list;
}

(** Raised by [retry] to signal "block until a read-set location changes." *)
exception Retry

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
    exception propagates to [commit], which blocks on the union of both
    read sets. *)
let or_else (tx1 : xt:t -> 'a) (tx2 : xt:t -> 'a) ~(xt : t) : 'a =
  let snap = xt.log in
  match tx1 ~xt with
  | v -> v
  | exception Retry ->
    xt.log <- snap;
    tx2 ~xt

(* ──────────────────────────────────────────────────────────────────────────
   Commit
   ────────────────────────────────────────────────────────────────────────── *)

(** Convert the transaction log to a k-CAS op list.
    - Entries where value unchanged → CMP (read-only validation)
    - Entries where value changed → CAS (read-write) *)
let ops_of_log (log : entry list) : Kcas.op list =
  List.filter_map (fun (Entry (loc, initial, current)) ->
    if !current == initial then
      Some (Kcas.CMP (loc, initial))
    else
      Some (Kcas.CAS (loc, initial, !current))
  ) log

(** Collect all locations from the log (for registering awaiters on retry). *)
let locs_of_log (log : entry list) :
    (Kcas.awaiter -> bool) list =
  List.map (fun (Entry (loc, _, _)) ->
    fun awaiter -> Kcas.add_awaiter loc awaiter
  ) log

(** Block until one of the logged locations changes, then return.

    Uses [Trigger.await] to suspend the current fiber instead of
    blocking the OS thread with Mutex/Condition. Must run within
    [Sched.run]. *)
let block_on_log (log : entry list) : unit =
  if log = [] then ()
  else begin
    let trigger = Trigger.create () in
    let awaiter () = Trigger.signal trigger |> ignore in
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
    let xt = { log = [] } in
    match tx ~xt with
    | result ->
      let ops = ops_of_log xt.log in
      if Kcas.atomically ops then result
      else loop ()  (* transient failure, retry *)
    | exception Retry ->
      (* Block until a read-set location changes, then retry. *)
      block_on_log xt.log;
      loop ()
  in
  loop ()
