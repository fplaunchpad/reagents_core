(** Readable implementation of the GKMZ k-CAS algorithm with CMP extension.

    Reference: "Efficient Multi-word Compare and Swap"
    by Guerraoui, Kogan, Marathe, Zablotchi (arXiv:2008.02527, DISC 2020).

    CMP extension from: kcas documentation (gkmz-with-read-only-cmp-ops.md).

    This extends [01_kcas] with read-only CMP operations. A CMP asserts that
    a location holds an expected value without writing to it. This allows
    transactions that only read overlapping locations to proceed in parallel,
    since CMP never installs a descriptor into the location.

    The algorithm proceeds in two phases:

    Phase 1 (Acquire): Walk the list of word descriptors and attempt to install
    our state descriptor into each target location via CAS. If we encounter a
    location owned by another active operation, we help that operation complete
    first (cooperative helping — this is what ensures lock-freedom).

    Phase 2 (Finalize): Once all locations are acquired (or a mismatch is found),
    atomically set the operation status to [After] (success) or [Before] (failure)
    via a single CAS on the status word.

    Key correctness properties:
    - Lock-free: at least one thread always makes progress (via helping).
    - Linearizable: the operation appears to take effect atomically at the
      point when the status word is finalized.
    - ABA-safe: every [atomically] call allocates fresh [state] and [mcas_desc]
      values, so physical identity comparison is safe. *)

(* ──────────────────────────────────────────────────────────────────────────
   Types
   ────────────────────────────────────────────────────────────────────────── *)

(** A shared memory location holding a value of type ['a].
    Corresponds to a "target address" in the paper. *)
type 'a loc = 'a state Atomic.t

(** Internal state stored in a location.

    Corresponds to [WordDescriptor] in the paper (the address field is implicit
    — it is the [loc] that contains this state).

    - [before]: the expected (old) value
    - [after]:  the desired (new) value
    - [mcas_desc]:   back-pointer to the owning [MCASDescriptor]'s status

    Fresh [state] records are allocated per-operation to avoid ABA problems. *)
and 'a state = {
  before : 'a;
  after : 'a;
  mcas_desc : mcas_desc;
}

(** An existentially-typed word descriptor paired with its target location.

    The existential hides the value type ['a], allowing a heterogeneous list of
    word descriptors (different locations may hold different types). *)
and word_desc = Word_desc : 'a loc * 'a state -> word_desc

(** Operation descriptor — an atomic cell holding the operation's status.

    Corresponds to [MCASDescriptor.status] in the paper.

    The data structure is cyclic: [mcas_desc] contains the list of [word_desc] descriptors,
    each [word_desc] contains a [state], and each [state] points back to the [mcas_desc].
    This allows traversal from any location's state back to the operation status,
    which is essential for the helping mechanism. *)
and mcas_desc = status Atomic.t

(** Status of a k-CAS operation.

    Corresponds to [StatusType] in the paper:
    - [Undetermined] = [ACTIVE]   — operation is still in progress
    - [After]        = [SUCCESSFUL] — all locations were successfully updated
    - [Before]       = [FAILED]     — at least one location had a mismatched value *)
and status =
  | Undetermined of word_desc list
  | After
  | Before

(** A single operation within a k-CAS (the user-facing type).

    - [CAS (loc, expected, desired)]: if [loc] holds [expected], write [desired].
    - [CMP (loc, expected)]: assert [loc] holds [expected] (read-only, no write).

    CMP never writes to the location, so multiple CMPs on the same location
    from different operations can proceed in parallel without contention. *)
type op =
  | CAS : 'a loc * 'a * 'a -> op
  | CMP : 'a loc * 'a -> op

(* ──────────────────────────────────────────────────────────────────────────
   Algorithm
   ────────────────────────────────────────────────────────────────────────── *)

(** [is_cmp mcas_desc state] returns [true] if [state] is a read-only CMP descriptor.

    CMP descriptors reuse the existing state from the location (which has a
    different [mcas_desc] than the current operation). CAS descriptors have freshly
    allocated states with the current operation's [mcas_desc]. So comparing the
    [mcas_desc] pointers distinguishes them. *)
let is_cmp mcas_desc state = state.mcas_desc != mcas_desc

(** [finish mcas_desc desired] attempts to finalize the operation.

    Before declaring success, verifies that all CMP locations still hold
    their original states. If any CMP location has changed, the operation
    must fail (the read-only assertion was violated). *)
let finish mcas_desc desired =
  match Atomic.get mcas_desc with
  | After -> true
  | Before -> false
  | Undetermined word_desc as current ->
    (* Before declaring success, verify all CMP locations are unchanged. *)
    let desired =
      if desired == After
         && List.exists (fun (Word_desc (loc, state)) ->
              is_cmp mcas_desc state && Atomic.get loc != state) word_desc
      then Before
      else desired
    in
    Atomic.compare_and_set mcas_desc current desired |> ignore;
    Atomic.get mcas_desc == After

(** [gkmz mcas_desc word_descs] runs Phase 1 (acquire) of the GKMZ algorithm.

    Corresponds to the for-loop on lines 12–21 of Listing 3 in the paper.

    Walks [word_descs] and, for each word descriptor [Word_desc (loc, desired)]:

    1. Read the current state from [loc].
    2. If [desired == current] (physical identity): our descriptor is already
       installed (perhaps by a helper), move on to the next word.
    3. Otherwise, determine the logical value at [loc]:
       - Call [is_after current.mcas_desc] to check whether the previous operation
         on this location succeeded. This may recursively help that operation.
       - The logical value is [current.after] if it succeeded, [current.before]
         if it failed.
    4. If the logical value does not match [desired.before]: the precondition
       for this word fails, so the entire k-CAS fails.
    5. Check that our own operation is still active. A helper thread may have
       already finalized us — if so, return the outcome.
    6. Attempt [CAS(loc, current, desired)] to install our descriptor.
       - On success: proceed to the next word.
       - On failure: retry this word. The failure may be due to a helper
         installing our descriptor, or a concurrent operation — either way,
         re-reading will sort it out.

    [is_after mcas_desc] resolves the outcome of a (possibly still-active) operation.

    Corresponds to [readInternal] in Listing 2 of the paper, combined with the
    helping call to [MCAS] on line 7. When the operation is still [Undetermined],
    we help it complete by recursively calling [gkmz]. This mutual recursion
    between [gkmz] and [is_after] is the helping mechanism that ensures
    lock-freedom. *)
let rec gkmz mcas_desc = function
  | [] ->
    (* All words acquired successfully — try to finalize as success. *)
    finish mcas_desc After
  | (Word_desc (loc, desired) :: continue) as retry ->
    let current = Atomic.get loc in
    if desired == current then
      (* Already installed (by us or a helper). Move on. *)
      gkmz mcas_desc continue
    else if is_cmp mcas_desc desired then
      (* This is a CMP (read-only) descriptor, and the location's state
         has changed since we took the snapshot. The assertion fails. *)
      finish mcas_desc Before
    else
      (* Determine the logical value at this location.
         If the location is owned by another operation, [is_after] will
         help that operation complete first. *)
      let current_value =
        if is_after current.mcas_desc then
          current.after
        else
          current.before
      in
      if current_value != desired.before then
        (* Precondition mismatch — this k-CAS must fail. *)
        finish mcas_desc Before
      else
        (* Value matches. Check we're still active before attempting to
           install our descriptor. (A helper may have already finalized us.) *)
        match Atomic.get mcas_desc with
        | Undetermined _ ->
          (* Still active — try to take ownership of this location. *)
          if Atomic.compare_and_set loc current desired then
            gkmz mcas_desc continue
          else
            (* CAS failed — a concurrent thread touched this location.
               Retry: re-read and try again. *)
            gkmz mcas_desc retry
        | After -> true
        | Before -> false

and is_after mcas_desc =
  match Atomic.get mcas_desc with
  | Undetermined word_desc ->
    (* The operation is still active — help it complete. *)
    gkmz mcas_desc word_desc
  | After -> true
  | Before -> false

(* ──────────────────────────────────────────────────────────────────────────
   Public API
   ────────────────────────────────────────────────────────────────────────── *)

(** [make v] creates a new shared memory location initialized to [v].

    The initial state uses [After] as its [mcas_desc] status, representing a
    "completed" dummy operation. This ensures [get] will read [state.after],
    which is [v]. *)
let make (v : 'a) : 'a loc =
  let mcas_desc = Atomic.make After in
  Atomic.make { before = v; after = v; mcas_desc }

(** [get loc] reads the current logical value from [loc].

    Corresponds to the [read] function on lines 8–11 of Listing 3.

    The location contains a [state] record. To determine the logical value,
    we check whether the operation that installed this state succeeded
    ([is_after] returns [true] → use [after]) or failed (→ use [before]). *)
let get (loc : 'a loc) : 'a =
  let state = Atomic.get loc in
  if is_after state.mcas_desc then
    state.after
  else
    state.before

(** [atomically ops] performs a k-word compare-and-swap with optional CMPs.

    For CAS operations: creates fresh state descriptors (with the current [mcas_desc]).
    For CMP operations: snapshots the existing state from the location (reuses
    it, so [is_cmp] will detect it by the differing [mcas_desc]). If the snapshot
    fails (value already wrong), fails immediately via [Exit]. *)
let atomically (ops : op list) : bool =
  let mcas_desc = Atomic.make After in
  let word_desc =
    ops
    |> List.map (function
      | CAS (loc, before, after) -> Word_desc (loc, { before; after; mcas_desc })
      | CMP (loc, expected) ->
        let current = Atomic.get loc in
        if get loc != expected || Atomic.get loc != current then
          raise Exit
        else
          Word_desc (loc, current))
  in
  Atomic.set mcas_desc (Undetermined word_desc);
  gkmz mcas_desc word_desc

let atomically ops =
  try atomically ops with Exit -> false

(** [compare_and_set loc expected desired] performs a single-word CAS.

    Implemented as a 1-word k-CAS via [atomically]. *)
let compare_and_set (loc : 'a loc) (expected : 'a) (desired : 'a) : bool =
  atomically [CAS (loc, expected, desired)]
