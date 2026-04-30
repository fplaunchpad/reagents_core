(** Regression tests for the commit-as-reagent / [+]-dispatch fix.

    The bug: in the previous design, [Reagent.upd] always returned
    [Done] from [try_react] when the body's [f] returned [Some], even
    if the kCAS at commit time would conflict. The old [Xt.commit]
    silently retried the body. So the [+] combinator never saw the
    conflict and never dispatched to its alternative — making patterns
    like [push s + swap elim] dead code on the contended path.

    The fix: [commit] is now a first-class reagent at the tail of every
    primitive's chain. A failed kCAS surfaces as [Retry] from
    [try_react]; [+] catches it and tries the alternative.

    {1 Tests}

    - [test_commit_failure_dispatches]: deterministic single-domain test.
      A body deliberately constructed to always fail at commit. With the
      old algorithm this would hang in [Xt.commit]'s retry loop; with
      the new algorithm [+] dispatches to the constant alternative.

    - [test_elimination_stack_smoke]: stress test of an elimination
      stack with N domains pushing and popping under contention. Mostly
      a smoke test for "doesn't deadlock, preserves data". *)

let assert_eq msg expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %d, got %d" msg expected actual

let run_test name f =
  Printf.printf "  %s... %!" name;
  (try f (); Printf.printf "OK\n%!"
   with e -> Printf.printf "FAIL: %s\n%!" (Printexc.to_string e); exit 1)

(** Run [f] in a fresh domain, fail if it doesn't finish within [timeout].
    Used to surface the old-algorithm hang as a test failure rather than
    a hung test runner. *)
let with_timeout ~timeout f =
  let result = Atomic.make None in
  let d = Domain.spawn (fun () ->
    let v = f () in
    Atomic.set result (Some v))
  in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait () =
    match Atomic.get result with
    | Some v -> v
    | None ->
      if Unix.gettimeofday () > deadline then
        failwith (Printf.sprintf "test timed out after %.1fs" timeout)
      else (Unix.sleepf 0.01; wait ())
  in
  let v = wait () in
  Domain.join d;
  v

(* ──────────────────────────────────────────────────────────────────────────
   Deterministic regression test for the algorithmic fix.

   We construct a body that always fails at commit: it reads a location
   into the xt log, then mutates the same location *externally* (outside
   the transaction). The xt log records the pre-mutation value as the
   initial; commit's kCAS asserts the location still has that value;
   the assertion fails; commit returns [Retry].

   Pre-fix algorithm: the body returns [Done], the wrapping [Xt.commit]
   in [run] catches the kCAS failure and retries the whole body — which
   re-runs the external mutation, ad infinitum. The [+] combinator never
   sees a [Retry], never dispatches to the alternative.

   Post-fix algorithm: the body returns [Done] but the chain ends in the
   [commit] reagent, which calls [Xt.try_commit_log]. That returns
   [false]; [commit] returns [Retry]; [+] dispatches to [constant];
   [run] returns the constant.
   ────────────────────────────────────────────────────────────────────────── *)

let test_commit_failure_dispatches () =
  let loc = Reagent.ref 0 in
  let always_fails_commit = Reagent.make_reagent (fun () xt ->
    let v = Xt.get ~xt loc in
    (* Externally mutate the location so the kCAS at commit fails. *)
    let _ = Kcas.compare_and_set loc v (v + 1) in
    Reagent.Done ())
  in
  let took_alternative = ref false in
  let alternative = Reagent.lift (fun () -> took_alternative := true) in
  let r = Reagent.(always_fails_commit + alternative) in
  let () = with_timeout ~timeout:2.0 (fun () -> Reagent.run r ()) in
  assert_eq "+ dispatched to alternative on commit-time Retry"
    1 (if !took_alternative then 1 else 0)

(* ──────────────────────────────────────────────────────────────────────────
   Elimination stack tests.

   Active offers in a channel queue come from reagents whose body returns
   [Block] before reaching [commit]: only [commit] rescinds, so a body
   that fails its precondition leaves the offer [Empty]. The canonical
   case: a blocking [pop] on an empty stack. That [Empty] offer in
   [elim_pop.outgoing] is what producers can rendezvous with on the
   contended path — this is when elimination engages.
   ────────────────────────────────────────────────────────────────────────── *)

module Treiber_stack = struct
  type 'a t = 'a list Reagent.ref [@@warning "-34"]
  let create () = Reagent.ref []
  let push s = Reagent.upd s (fun xs x -> Some (x :: xs, ()))
  let try_pop s = Reagent.upd s (fun xs () ->
    match xs with [] -> Some ([], None) | x :: xs' -> Some (xs', Some x))
  let pop s = Reagent.upd s (fun xs () ->
    match xs with [] -> None | x :: xs' -> Some (xs', x))
end

(** Elimination stack with instrumented elim branches. The push/pop
    elim sides each thread an [upd] on a counter loc into the merged
    chain — that increment is part of the rendezvous kCAS, so it only
    fires when a swap actually completes. *)
module Elimination_stack = struct
  type 'a t = {
    stack : 'a Treiber_stack.t;
    elim_push : ('a, unit) Channel.endpoint;
    elim_pop : (unit, 'a) Channel.endpoint;
    pushes_via_elim : int Reagent.ref;
    pops_via_elim : int Reagent.ref;
  }
  let create () =
    let elim_push, elim_pop = Channel.mk_chan () in
    { stack = Treiber_stack.create ();
      elim_push; elim_pop;
      pushes_via_elim = Reagent.ref 0;
      pops_via_elim = Reagent.ref 0 }

  let bump c =
    Reagent.upd c (fun n () -> Some (Stdlib.(n + 1), ()))

  let push r =
    Reagent.(Treiber_stack.push r.stack
             + (Channel.swap r.elim_push >> bump r.pushes_via_elim))

  let pop r =
    Reagent.(Treiber_stack.pop r.stack
             + (Channel.swap r.elim_pop >> lift (fun x -> x)
                >> upd r.pops_via_elim (fun n x -> Some (Stdlib.(n + 1), x))))
end

let test_elimination_stack_smoke () =
  let s = Elimination_stack.create () in
  let n_domains = 4 in
  let n_per = 250 in
  let total = n_domains * n_per in
  let producers = List.init n_domains (fun id ->
    Domain.spawn (fun () ->
      for i = 0 to n_per - 1 do
        Reagent.run (Elimination_stack.push s) (id * n_per + i)
      done))
  in
  let consumed = Atomic.make 0 in
  let consumers = List.init n_domains (fun _ ->
    Domain.spawn (fun () ->
      let rec loop () =
        if Atomic.get consumed >= total then ()
        else match Reagent.run (Treiber_stack.try_pop s.stack) () with
          | Some _ -> Atomic.incr consumed; loop ()
          | None -> Domain.cpu_relax (); loop ()
      in
      loop ()))
  in
  List.iter Domain.join producers;
  List.iter Domain.join consumers;
  assert_eq "all values consumed" total (Atomic.get consumed)

(* Hostile-to-Treiber scenario: many consumers sit on an empty stack
   running the BLOCKING [pop] reagent, posting [Empty] offers to
   [elim_pop.outgoing]. Producers then push, contending on the stack
   head with each other. When a producer's commit fails, [+] falls
   through to [swap elim_push], which scans [elim_push.incoming] (=
   [elim_pop.outgoing]) and finds the consumers' active offers. *)
let test_elimination_engages_under_contention () =
  let s = Elimination_stack.create () in
  let n_consumers = 4 in
  let n_producers = 4 in
  let n_per = 200 in
  let _total = n_consumers * n_per in
  let consumed_sum = Atomic.make 0 in

  (* Start consumers first so they post Empty offers on the empty stack. *)
  let consumers = List.init n_consumers (fun _ ->
    Domain.spawn (fun () ->
      let local = ref 0 in
      for _ = 1 to n_per do
        local := !local + Reagent.run (Elimination_stack.pop s) ()
      done;
      Atomic.fetch_and_add consumed_sum !local |> ignore))
  in

  (* Brief delay to give consumers time to post offers before producers
     start. Not necessary for correctness, but makes elimination more
     likely. *)
  Unix.sleepf 0.05;

  let pushed_sum = ref 0 in
  let producers = List.init n_producers (fun id ->
    let base = id * n_per in
    Domain.spawn (fun () ->
      let local = ref 0 in
      for i = 0 to n_per - 1 do
        let v = base + i + 1 in    (* +1 so values are non-zero *)
        local := !local + v;
        Reagent.run (Elimination_stack.push s) v
      done;
      !local))
  in
  List.iter (fun d -> pushed_sum := !pushed_sum + Domain.join d) producers;
  List.iter Domain.join consumers;

  (* Conservation: consumers' total should equal producers' total. *)
  assert_eq "sum conserved" !pushed_sum (Atomic.get consumed_sum);

  let pe = Reagent.get s.pushes_via_elim in
  let po = Reagent.get s.pops_via_elim in
  Printf.printf "(pushes_via_elim=%d, pops_via_elim=%d) %!" pe po;
  if pe <> po then
    failwith (Printf.sprintf "elim push/pop counts disagree: %d vs %d" pe po);
  if pe = 0 then
    failwith "elimination never engaged — algorithm regression"

(* ── Run ────────────────────────────────────────────────────────────────── *)

let () =
  Printf.printf "Commit-as-reagent dispatch:\n%!";
  run_test "commit_failure_dispatches" test_commit_failure_dispatches;
  Printf.printf "\nElimination stack:\n%!";
  run_test "elimination_stack_smoke" test_elimination_stack_smoke;
  run_test "elimination_engages_under_contention"
    test_elimination_engages_under_contention;
  Printf.printf "\nAll tests passed.\n%!"
