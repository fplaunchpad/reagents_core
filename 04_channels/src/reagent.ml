(** CPS reagents with first-class commit and a two-phase offer protocol.

    Three load-bearing ideas, all from Turon (PLDI 2012):

    1. [commit] is itself a reagent appended to every primitive's chain.
       A failed kCAS surfaces as [Retry] from [try_react] so [+] can
       dispatch to the alternative. Without this, push-via-[upd] would
       always return [Done] from the body and the [+]-elimination
       pattern would never kick in under contention.

    2. [BlockAndRetry] distinguishes "one branch blocks, another has
       transient failure" from pure block or pure retry. The run loop
       uses this to decide whether to enter phase 2 (with offer).

    3. Offers live in [Kcas.loc]s so partner fulfilment is part of the
       same kCAS as the rest of the swap. Lock-free; matches reference. *)

(* ──────────────────────────────────────────────────────────────────────────
   Offers
   ────────────────────────────────────────────────────────────────────────── *)

type 'a offer_state =
  | Empty
  | Completed of 'a
  | Rescinded

type 'a offer = {
  state : 'a offer_state Kcas.loc;
  mutex : Mutex.t;
  cond : Condition.t;
}

let make_offer () : 'a offer = {
  state = Kcas.make Empty;
  mutex = Mutex.create ();
  cond = Condition.create ();
}

let offer_state_loc o = o.state

let is_offer_active o =
  match Kcas.get o.state with Empty -> true | _ -> false

(** Try to rescind: CAS Empty→Rescinded. Used by both the read-set
    awaiter callback (precondition changed, abandon the offer) and by
    the [commit] reagent (cancel the offer just before our own kCAS).
    No-op if the offer is already non-Empty. *)
let signal_offer (o : 'a offer) : unit =
  ignore (Kcas.compare_and_set o.state Empty Rescinded)

(** Wake any thread parked in [await_offer] for this offer. Awaiter
    callback installed on the offer's loc. *)
let wake_offer (o : 'a offer) () : unit =
  Mutex.lock o.mutex;
  Condition.signal o.cond;
  Mutex.unlock o.mutex

(** Park until the offer transitions out of [Empty]. The kCAS that
    transitions the offer fires our awaiter, signaling the cond. We
    re-read the loc state inside the lock to handle the case where the
    transition happened before our awaiter was installed. *)
let await_offer (o : 'a offer) : 'a offer_state =
  let cb = wake_offer o in
  let rec install () =
    if not (Kcas.add_awaiter o.state cb) then install ()
  in
  install ();
  Mutex.lock o.mutex;
  let rec wait_loop () =
    match Kcas.get o.state with
    | Empty -> Condition.wait o.cond o.mutex; wait_loop ()
    | st -> st
  in
  let final = wait_loop () in
  Mutex.unlock o.mutex;
  final

(** [try_rescind o] = same CAS as [signal_offer], but reports the
    outcome: [None] if we successfully rescinded (or it was already
    Rescinded), [Some v] if a partner had completed it before us. *)
let try_rescind (o : 'a offer) : 'a option =
  if Kcas.compare_and_set o.state Empty Rescinded then None
  else match Kcas.get o.state with
    | Completed v -> Some v
    | Rescinded -> None
    | Empty -> assert false

(* ──────────────────────────────────────────────────────────────────────────
   Reagent type
   ────────────────────────────────────────────────────────────────────────── *)

type 'a result =
  | Done of 'a
  | Block
  | Retry
  | BlockAndRetry

type ('a, 'b) t = {
  try_react : 'a -> Xt.t -> 'b offer option -> 'b result;
  seq : 'c. ('b, 'c) t -> ('a, 'c) t;
  always_commits : bool;
}

type 'a ref = 'a Kcas.loc

(* ──────────────────────────────────────────────────────────────────────────
   The [commit] primitive — the core of the algorithm.

   When called with no offer: try the kCAS. If it succeeds, [Done];
   else [Retry] so an enclosing [+] can take the alternative.

   When called with an offer: first rescind it (so future channel
   partners can't fulfil an offer we're about to commit past). If
   rescind found we'd already been completed by a partner, return that
   value (the partner's swap won the race; we honour their result).
   Otherwise commit our kCAS as usual.

   [commit.seq next = next] absorbs itself, so the chain still ends in
   exactly one commit (at the rightmost primitive).
   ────────────────────────────────────────────────────────────────────────── *)

let commit : type a. (a, a) t = {
  try_react = (fun a xt offer ->
    match offer with
    | None ->
      if Xt.try_commit_log xt then Done a else Retry
    | Some o ->
      match try_rescind o with
      | Some v -> Done v
      | None ->
        if Xt.try_commit_log xt then Done a else Retry);
  seq = (fun (type c) (next : (a, c) t) -> next);
  always_commits = true;
}

(* ──────────────────────────────────────────────────────────────────────────
   Primitive construction.

   [mk_reagent body k] composes a body function with a continuation [k].
   The body returns [Done b]/[Block]/[Retry]; on [Done], its output
   feeds [k]. The body does not see the offer (only [commit] and
   custom reagents like [swap] need it).

   [make_reagent body] = [mk_reagent body commit] — the standard pattern
   for primitives that just want to run a body and then commit at the
   tail.
   ────────────────────────────────────────────────────────────────────────── *)

let rec mk_reagent : type a b c.
    (a -> Xt.t -> b result) -> (b, c) t -> (a, c) t =
  fun body k -> {
    try_react = (fun a xt offer ->
      match body a xt with
      | Done b -> k.try_react b xt offer
      | Block -> Block
      | Retry -> Retry
      | BlockAndRetry -> BlockAndRetry);
    seq = (fun next -> mk_reagent body (k.seq next));
    always_commits = k.always_commits;
  }

let make_reagent body = mk_reagent body commit

(* ──────────────────────────────────────────────────────────────────────────
   Ref operations
   ────────────────────────────────────────────────────────────────────────── *)

let ref v = Kcas.make v
let get r = Kcas.get r

let read r = make_reagent (fun () xt -> Done (Xt.get ~xt r))

let upd r f = make_reagent (fun b xt ->
  let v = Xt.get ~xt r in
  match f v b with
  | Some (v', c) -> Xt.set ~xt r v'; Done c
  | None -> Block)

let cas r expected desired = make_reagent (fun () xt ->
  let v = Xt.get ~xt r in
  if v = expected then (Xt.set ~xt r desired; Done ())
  else Block)

let modify r f = make_reagent (fun () xt -> Xt.modify ~xt r f; Done ())
let set r v = make_reagent (fun () xt -> Xt.set ~xt r v; Done ())

let constant v = make_reagent (fun _ _ -> Done v)
let lift f = make_reagent (fun a _ -> Done (f a))

(* ──────────────────────────────────────────────────────────────────────────
   Combinators
   ────────────────────────────────────────────────────────────────────────── *)

let (>>) r1 r2 = r1.seq r2

(** Choice. Snapshots [xt] before [r1], runs it; on any non-[Done],
    rolls [xt] back and runs [r2]. The result table mirrors the
    reference exactly so [run] can dispatch correctly:

    | r1            | r2          | result        |
    |---------------|-------------|---------------|
    | Done          | (not run)   | Done          |
    | Block         | Done        | Done          |
    | Block         | Block       | Block         |
    | Block         | Retry       | BlockAndRetry |
    | Block         | BlockAndR.  | BlockAndR.    |
    | Retry         | Done        | Done          |
    | Retry         | Block       | BlockAndRetry |
    | Retry         | Retry       | Retry         |
    | Retry         | BlockAndR.  | BlockAndR.    |
    | BlockAndRetry | anything≠Done | BlockAndR.  |
*)
let rec (+) : type a b. (a, b) t -> (a, b) t -> (a, b) t =
  fun r1 r2 -> {
    try_react = (fun a xt offer ->
      let cp = Xt.checkpoint xt in
      match r1.try_react a xt offer with
      | Done _ as v -> v
      | Block ->
        Xt.rollback xt cp;
        (match r2.try_react a xt offer with
         | Retry -> BlockAndRetry
         | v -> v)
      | Retry ->
        Xt.rollback xt cp;
        (match r2.try_react a xt offer with
         | Block -> BlockAndRetry
         | v -> v)
      | BlockAndRetry ->
        Xt.rollback xt cp;
        (match r2.try_react a xt offer with
         | Block | Retry -> BlockAndRetry
         | v -> v));
    seq = (fun k -> (r1.seq k) + (r2.seq k));
    always_commits = r1.always_commits && r2.always_commits;
  }

(* ──────────────────────────────────────────────────────────────────────────
   Execution (two-phase)

   Phase 1 (no offer): try synchronously. On [Done], return. On [Retry],
   pause and re-try phase 1. On [Block]/[BlockAndRetry], enter phase 2.

   Phase 2 (with fresh offer): try once. On [Done], return. On [Block],
   the reagent has either posted the offer to a channel or returned
   without finding a precondition; install awaiters on the read-set so
   any change to a watched location signals the offer, then park. On
   [Retry]/[BlockAndRetry], pause briefly and try to rescind the offer:
   if a partner had completed it concurrently, take their value; else
   loop with a fresh offer.
   ────────────────────────────────────────────────────────────────────────── *)

let run r a =
  let rec without_offer () =
    let xt = Xt.fresh () in
    match r.try_react a xt None with
    | Done v -> v
    | Retry -> Domain.cpu_relax (); without_offer ()
    | Block | BlockAndRetry -> with_offer ()

  and with_offer () =
    let offer = make_offer () in
    let xt = Xt.fresh () in
    match r.try_react a xt (Some offer) with
    | Done v -> v
    | Block ->
      let snap = Xt.snapshot ~xt in
      Xt.install_awaiters snap (fun () -> signal_offer offer);
      (match await_offer offer with
       | Completed v -> v
       | Rescinded -> without_offer ()
       | Empty -> assert false)
    | Retry | BlockAndRetry ->
      Domain.cpu_relax ();
      (match try_rescind offer with
       | Some v -> v
       | None -> with_offer ())
  in
  without_offer ()

let run_opt r a =
  (* Phase-1 only: never posts an offer, never installs awaiters.
     But transient kCAS conflicts ([Retry] from [commit]) are not a
     "would block" condition — we retry until we either succeed or hit
     a real precondition failure ([Block]/[BlockAndRetry]). This mirrors
     the linearizability semantics of [Xt.commit]'s internal retry. *)
  let rec loop () =
    let xt = Xt.fresh () in
    match r.try_react a xt None with
    | Done v -> Some v
    | Retry -> Domain.cpu_relax (); loop ()
    | Block | BlockAndRetry -> None
  in
  loop ()
