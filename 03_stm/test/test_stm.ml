let assert_eq msg expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %d, got %d" msg expected actual
let _assert_true msg b = if not b then failwith (msg ^ ": expected true")
let run_test name f =
  Printf.printf "  %s... %!" name;
  (try f (); Printf.printf "OK\n%!"
   with e -> Printf.printf "FAIL: %s\n%!" (Printexc.to_string e); exit 1)

(* ── Basic Xt transactions ──────────────────────────────────────────────── *)

let test_single_read () =
  let a = Kcas.make 42 in
  let v = Xt.commit (fun ~xt -> Xt.get ~xt a) in
  assert_eq "read" 42 v

let test_single_write () =
  let a = Kcas.make 0 in
  Xt.commit (fun ~xt -> Xt.set ~xt a 99);
  assert_eq "write" 99 (Kcas.get a)

let test_read_write () =
  let a = Kcas.make 10 in
  let b = Kcas.make 20 in
  let c = Kcas.make 0 in
  Xt.commit (fun ~xt ->
    let va = Xt.get ~xt a in
    let vb = Xt.get ~xt b in
    Xt.set ~xt c (va + vb));
  assert_eq "c = a + b" 30 (Kcas.get c)

let test_modify () =
  let a = Kcas.make 5 in
  Xt.commit (fun ~xt -> Xt.modify ~xt a (fun x -> x * 2));
  assert_eq "modify" 10 (Kcas.get a)

let test_update () =
  let a = Kcas.make 5 in
  let old = Xt.commit (fun ~xt -> Xt.update ~xt a (fun x -> x + 1)) in
  assert_eq "update old" 5 old;
  assert_eq "update new" 6 (Kcas.get a)

let test_read_own_write () =
  let a = Kcas.make 0 in
  let v = Xt.commit (fun ~xt ->
    Xt.set ~xt a 42;
    Xt.get ~xt a) in
  assert_eq "read own write" 42 v

let test_multi_location () =
  let a = Kcas.make 1 in
  let b = Kcas.make 2 in
  let c = Kcas.make 3 in
  Xt.commit (fun ~xt ->
    let va = Xt.get ~xt a in
    let vb = Xt.get ~xt b in
    let vc = Xt.get ~xt c in
    Xt.set ~xt a (va * 10);
    Xt.set ~xt b (vb * 10);
    Xt.set ~xt c (vc * 10));
  assert_eq "a" 10 (Kcas.get a);
  assert_eq "b" 20 (Kcas.get b);
  assert_eq "c" 30 (Kcas.get c)

(* ── retry ──────────────────────────────────────────────────────────────── *)

let test_retry () =
  (* One domain waits for a location to become positive, another sets it. *)
  let a = Kcas.make 0 in
  let d = Domain.spawn (fun () ->
    Xt.commit (fun ~xt ->
      let v = Xt.get ~xt a in
      if v <= 0 then Xt.retry ();
      v))
  in
  Unix.sleepf 0.01;
  Kcas.compare_and_set a 0 42 |> ignore;
  let result = Domain.join d in
  assert_eq "retry got value" 42 result

(* ── or_else ────────────────────────────────────────────────────────────── *)

let test_or_else_first_succeeds () =
  let a = Kcas.make 10 in
  let v = Xt.commit (fun ~xt ->
    Xt.or_else
      (fun ~xt -> Xt.get ~xt a)
      (fun ~xt -> ignore (Xt.get ~xt a); 999)
      ~xt) in
  assert_eq "first" 10 v

let test_or_else_fallthrough () =
  let a = Kcas.make 0 in
  let b = Kcas.make 42 in
  let v = Xt.commit (fun ~xt ->
    Xt.or_else
      (fun ~xt ->
        let v = Xt.get ~xt a in
        if v = 0 then Xt.retry ();
        v)
      (fun ~xt -> Xt.get ~xt b)
      ~xt) in
  assert_eq "fallthrough to b" 42 v

let test_or_else_both_retry () =
  (* Both alternatives retry. The combined transaction should block
     until either a or b changes. *)
  let a = Kcas.make 0 in
  let b = Kcas.make 0 in
  let d = Domain.spawn (fun () ->
    Xt.commit (fun ~xt ->
      Xt.or_else
        (fun ~xt ->
          let v = Xt.get ~xt a in
          if v = 0 then Xt.retry (); v)
        (fun ~xt ->
          let v = Xt.get ~xt b in
          if v = 0 then Xt.retry (); v)
        ~xt))
  in
  Unix.sleepf 0.01;
  (* Change b — should wake the blocked transaction *)
  Kcas.compare_and_set b 0 77 |> ignore;
  let result = Domain.join d in
  assert_eq "or_else both retry" 77 result

(* ── Concurrent ─────────────────────────────────────────────────────────── *)

let test_concurrent_transfers () =
  let a = Kcas.make 50 in
  let b = Kcas.make 50 in
  let domains = List.init 4 (fun _ ->
    Domain.spawn (fun () ->
      for _ = 1 to 2_000 do
        Xt.commit (fun ~xt ->
          let va = Xt.get ~xt a in
          let vb = Xt.get ~xt b in
          Xt.set ~xt a (va - 1);
          Xt.set ~xt b (vb + 1))
        |> ignore
      done))
  in
  List.iter Domain.join domains;
  assert_eq "invariant" 100 (Kcas.get a + Kcas.get b)

let test_concurrent_read_only () =
  (* Concurrent read-only transactions should not block each other
     (they use CMP, not CAS). *)
  let a = Kcas.make 42 in
  let b = Kcas.make 58 in
  let successes = Atomic.make 0 in
  let domains = List.init 4 (fun _ ->
    Domain.spawn (fun () ->
      for _ = 1 to 5_000 do
        let sum = Xt.commit (fun ~xt ->
          Xt.get ~xt a + Xt.get ~xt b) in
        if sum = 100 then Atomic.incr successes
      done))
  in
  List.iter Domain.join domains;
  assert_eq "all reads consistent" (4 * 5_000) (Atomic.get successes)

(* No-skew test (from reference kcas test suite):
   One domain atomically toggles a1 and a2 between (0,0) and (1,1).
   Other domains read both — they should NEVER see (0,1) or (1,0). *)
let test_no_skew () =
  let a1 = Kcas.make 0 in
  let a2 = Kcas.make 0 in
  let test_finished = Atomic.make false in
  let skew_detected = Atomic.make false in
  let writer = Domain.spawn (fun () ->
    for _ = 1 to 5_000 do
      Xt.commit (fun ~xt ->
        Xt.set ~xt a1 1;
        Xt.set ~xt a2 1);
      Xt.commit (fun ~xt ->
        Xt.set ~xt a1 0;
        Xt.set ~xt a2 0)
    done;
    Atomic.set test_finished true) in
  let readers = List.init 2 (fun _ ->
    Domain.spawn (fun () ->
      while not (Atomic.get test_finished) do
        let v1, v2 = Xt.commit (fun ~xt ->
          (Xt.get ~xt a1, Xt.get ~xt a2)) in
        if v1 <> v2 then Atomic.set skew_detected true
      done)) in
  Domain.join writer;
  List.iter Domain.join readers;
  assert_eq "no skew" 0 (if Atomic.get skew_detected then 1 else 0)

(* ── Thesis examples (Turon, "Reagents", 2012) ──────────────────────────── *)

(* Treiber stack via Xt (Figure 10.2) *)
module Xt_stack = struct
  let create () = Kcas.make []

  let push ~xt s x =
    let xs = Xt.get ~xt s in
    Xt.set ~xt s (x :: xs)

  let try_pop ~xt s =
    match Xt.get ~xt s with
    | [] -> None
    | x :: xs -> Xt.set ~xt s xs; Some x
end

let test_xt_stack () =
  let s = Xt_stack.create () in
  Xt.commit (fun ~xt -> Xt_stack.push ~xt s 1);
  Xt.commit (fun ~xt -> Xt_stack.push ~xt s 2);
  Xt.commit (fun ~xt -> Xt_stack.push ~xt s 3);
  assert_eq "pop" 3
    (Xt.commit (fun ~xt ->
      match Xt_stack.try_pop ~xt s with Some v -> v | None -> -1))

(* Atomic transfer between stacks (thesis p.184) *)
let test_xt_stack_transfer () =
  let s1 = Xt_stack.create () in
  let s2 = Xt_stack.create () in
  Xt.commit (fun ~xt -> Xt_stack.push ~xt s1 42);
  (* Atomically pop from s1 and push to s2 *)
  Xt.commit (fun ~xt ->
    match Xt_stack.try_pop ~xt s1 with
    | Some v -> Xt_stack.push ~xt s2 v
    | None -> Xt.retry ());
  assert_eq "s2 got" 42
    (Xt.commit (fun ~xt ->
      match Xt_stack.try_pop ~xt s2 with Some v -> v | None -> -1));
  assert_eq "s1 empty" (-1)
    (Xt.commit (fun ~xt ->
      match Xt_stack.try_pop ~xt s1 with Some v -> v | None -> -1))

(* Counter with blocking decrement (thesis p.180) *)
module Counter = struct
  let create n = Kcas.make n

  let inc ~xt c =
    Xt.modify ~xt c (fun i -> i + 1)

  let dec ~xt c =
    let i = Xt.get ~xt c in
    if i <= 0 then Xt.retry ();
    Xt.set ~xt c (i - 1)

  let try_dec ~xt c =
    let i = Xt.get ~xt c in
    if i <= 0 then None
    else (Xt.set ~xt c (i - 1); Some i)

  let value c = Kcas.get c
end

let test_counter_inc_dec () =
  let c = Counter.create 0 in
  Xt.commit (fun ~xt -> Counter.inc ~xt c);
  Xt.commit (fun ~xt -> Counter.inc ~xt c);
  Xt.commit (fun ~xt -> Counter.inc ~xt c);
  assert_eq "after 3 inc" 3 (Counter.value c);
  Xt.commit (fun ~xt -> Counter.dec ~xt c);
  assert_eq "after dec" 2 (Counter.value c)

let test_counter_try_dec () =
  let c = Counter.create 0 in
  let r = Xt.commit (fun ~xt -> Counter.try_dec ~xt c) in
  assert_eq "try_dec empty" 1 (if r = None then 1 else 0);
  Xt.commit (fun ~xt -> Counter.inc ~xt c);
  let r = Xt.commit (fun ~xt -> Counter.try_dec ~xt c) in
  assert_eq "try_dec got" 1 (match r with Some 1 -> 1 | _ -> 0)

let test_counter_blocking_dec () =
  (* dec blocks until another domain increments *)
  let c = Counter.create 0 in
  let d = Domain.spawn (fun () ->
    Xt.commit (fun ~xt -> Counter.dec ~xt c);
    Counter.value c) in
  Unix.sleepf 0.01;
  Xt.commit (fun ~xt -> Counter.inc ~xt c);
  let _ = Domain.join d in
  assert_eq "after blocking dec" 0 (Counter.value c)

(* Michael-Scott queue via Xt (thesis Figure 10.4, simplified) *)
module Xt_queue = struct
  type 'a t = {
    head : 'a list Kcas.loc;
    tail : 'a list Kcas.loc;
  }

  let create () = { head = Kcas.make []; tail = Kcas.make [] }

  let enq ~xt q x =
    let t = Xt.get ~xt q.tail in
    Xt.set ~xt q.tail (x :: t)

  let try_deq ~xt q =
    match Xt.get ~xt q.head with
    | x :: rest -> Xt.set ~xt q.head rest; Some x
    | [] ->
      (* Head empty — reverse the tail and move it to head *)
      match Xt.get ~xt q.tail with
      | [] -> None
      | t ->
        Xt.set ~xt q.tail [];
        let h = List.rev t in
        (match h with
         | x :: rest -> Xt.set ~xt q.head rest; Some x
         | [] -> None)
end

let test_queue () =
  let q = Xt_queue.create () in
  Xt.commit (fun ~xt -> Xt_queue.enq ~xt q 1);
  Xt.commit (fun ~xt -> Xt_queue.enq ~xt q 2);
  Xt.commit (fun ~xt -> Xt_queue.enq ~xt q 3);
  assert_eq "deq 1" 1
    (Xt.commit (fun ~xt ->
      match Xt_queue.try_deq ~xt q with Some v -> v | None -> -1));
  assert_eq "deq 2" 2
    (Xt.commit (fun ~xt ->
      match Xt_queue.try_deq ~xt q with Some v -> v | None -> -1));
  assert_eq "deq 3" 3
    (Xt.commit (fun ~xt ->
      match Xt_queue.try_deq ~xt q with Some v -> v | None -> -1))

let test_queue_empty () =
  let q = Xt_queue.create () in
  let r = Xt.commit (fun ~xt -> Xt_queue.try_deq ~xt q) in
  assert_eq "deq empty" 1 (if r = None then 1 else 0)

(* Concurrent queue: multiple producers/consumers maintaining FIFO order *)
let test_concurrent_queue () =
  let q = Xt_queue.create () in
  let n = 1_000 in
  let producers = List.init 2 (fun id ->
    Domain.spawn (fun () ->
      for i = 0 to n - 1 do
        Xt.commit (fun ~xt -> Xt_queue.enq ~xt q (id * n + i))
      done)) in
  List.iter Domain.join producers;
  let count = Atomic.make 0 in
  let consumers = List.init 2 (fun _ ->
    Domain.spawn (fun () ->
      let c = Stdlib.ref 0 in
      let go = Stdlib.ref true in
      while !go do
        let r = Xt.commit (fun ~xt -> Xt_queue.try_deq ~xt q) in
        match r with Some _ -> incr c | None -> go := false
      done;
      Atomic.fetch_and_add count !c |> ignore)) in
  List.iter Domain.join consumers;
  assert_eq "queue count" (2 * n) (Atomic.get count)

(* Reagent-style examples using the Reagent wrapper *)

module Treiber_stack = struct
  type 'a t = 'a list Reagent.ref [@@warning "-34"]
  let create () = Reagent.ref []
  let push s = Reagent.upd s (fun xs x -> Some (x :: xs, ()))
  (* Blocking pop: retries when empty *)
  let pop s = Reagent.upd s (fun xs () ->
    match xs with [] -> None | x :: xs' -> Some (xs', x))
  (* Non-blocking pop: returns option *)
  let try_pop s = Reagent.upd s (fun xs () ->
    match xs with [] -> Some ([], None) | x :: xs' -> Some (xs', Some x))
end

let test_reagent_treiber_stack () =
  let s = Treiber_stack.create () in
  Reagent.run (Treiber_stack.push s) 10;
  Reagent.run (Treiber_stack.push s) 20;
  Reagent.run (Treiber_stack.push s) 30;
  assert_eq "pop" 30 (Reagent.run (Treiber_stack.pop s) ())

let test_reagent_transfer () =
  let s1 = Treiber_stack.create () in
  let s2 = Treiber_stack.create () in
  Reagent.run (Treiber_stack.push s1) 42;
  (* Atomic pop-and-push via >> *)
  Reagent.run
    Reagent.(Treiber_stack.pop s1 >> Treiber_stack.push s2) ();
  assert_eq "s2" 42 (Reagent.run (Treiber_stack.pop s2) ())

let test_reagent_choice () =
  (* Pop from s1; if empty, pop from s2 (thesis elimination stack pattern) *)
  let s1 = Treiber_stack.create () in
  let s2 = Treiber_stack.create () in
  Reagent.run (Treiber_stack.push s2) 99;
  let pop_either =
    Reagent.(+) (Treiber_stack.pop s1) (Treiber_stack.pop s2)
  in
  assert_eq "choice pops from s2" 99 (Reagent.run pop_either ())

(* LIFO ordering: push 1,2,3 → pop 3,2,1 *)
let test_reagent_lifo () =
  let s = Treiber_stack.create () in
  List.iter (Reagent.run (Treiber_stack.push s)) [1; 2; 3];
  assert_eq "lifo 1" 3 (Reagent.run (Treiber_stack.pop s) ());
  assert_eq "lifo 2" 2 (Reagent.run (Treiber_stack.pop s) ());
  assert_eq "lifo 3" 1 (Reagent.run (Treiber_stack.pop s) ())

(* Non-blocking try_pop: returns None on empty, Some on non-empty *)
let test_reagent_try_pop () =
  let s = Treiber_stack.create () in
  let r = Reagent.run (Treiber_stack.try_pop s) () in
  assert_eq "try_pop empty" 1 (if r = None then 1 else 0);
  Reagent.run (Treiber_stack.push s) 42;
  let r = Reagent.run (Treiber_stack.try_pop s) () in
  assert_eq "try_pop got" 42 (match r with Some v -> v | None -> -1)

(* run_opt: returns None when the reagent retries *)
let test_reagent_run_opt () =
  let s = Treiber_stack.create () in
  let r = Reagent.run_opt (Treiber_stack.pop s) () in
  assert_eq "run_opt empty" 1 (if r = None then 1 else 0);
  Reagent.run (Treiber_stack.push s) 7;
  let r = Reagent.run_opt (Treiber_stack.pop s) () in
  assert_eq "run_opt got" 7 (match r with Some v -> v | None -> -1)

(* Blocking pop: one domain pops from empty stack, another pushes *)
let test_reagent_blocking_pop () =
  let s = Treiber_stack.create () in
  let d = Domain.spawn (fun () ->
    Reagent.run (Treiber_stack.pop s) ()) in
  Unix.sleepf 0.01;
  Reagent.run (Treiber_stack.push s) 99;
  assert_eq "blocking pop" 99 (Domain.join d)

(* Lift: pop and transform the result *)
let test_reagent_lift () =
  let s = Treiber_stack.create () in
  Reagent.run (Treiber_stack.push s) 5;
  let pop_doubled = Reagent.(Treiber_stack.pop s >> lift (fun x -> x * 2)) in
  assert_eq "lift" 10 (Reagent.run pop_doubled ())

(* Pair: push to two stacks atomically *)
let test_reagent_pair () =
  let s1 = Treiber_stack.create () in
  let s2 = Treiber_stack.create () in
  let push_both = Reagent.pair (Treiber_stack.push s1) (Treiber_stack.push s2) in
  Reagent.run push_both (10, 20) |> ignore;
  assert_eq "pair s1" 10 (Reagent.run (Treiber_stack.pop s1) ());
  assert_eq "pair s2" 20 (Reagent.run (Treiber_stack.pop s2) ())

(* Constant: ignore pop result and produce a fixed value *)
let test_reagent_constant () =
  let s = Treiber_stack.create () in
  Reagent.run (Treiber_stack.push s) 42;
  let pop_then_zero = Reagent.(Treiber_stack.pop s >> constant 0) in
  assert_eq "constant" 0 (Reagent.run pop_then_zero ());
  (* Stack should be empty after pop *)
  assert_eq "popped" 1
    (if Reagent.run (Treiber_stack.try_pop s) () = None then 1 else 0)

(* Concurrent push/pop: multiple domains push and pop, total count preserved *)
let test_reagent_concurrent () =
  let s = Treiber_stack.create () in
  let n = 1_000 in
  (* 4 domains each push n items *)
  let pushers = List.init 4 (fun id ->
    Domain.spawn (fun () ->
      for i = 0 to n - 1 do
        Reagent.run (Treiber_stack.push s) (id * n + i)
      done)) in
  List.iter Domain.join pushers;
  (* Pop everything and count *)
  let count = Stdlib.ref 0 in
  let go = Stdlib.ref true in
  while !go do
    match Reagent.run (Treiber_stack.try_pop s) () with
    | Some _ -> incr count
    | None -> go := false
  done;
  assert_eq "concurrent count" (4 * n) !count

(* Atomic transfer chain: s1 → s2 → s3 via >> *)
let test_reagent_transfer_chain () =
  let s1 = Treiber_stack.create () in
  let s2 = Treiber_stack.create () in
  let s3 = Treiber_stack.create () in
  Reagent.run (Treiber_stack.push s1) 77;
  (* s1 → s2 *)
  Reagent.run Reagent.(Treiber_stack.pop s1 >> Treiber_stack.push s2) ();
  (* s2 → s3 *)
  Reagent.run Reagent.(Treiber_stack.pop s2 >> Treiber_stack.push s3) ();
  assert_eq "chain s3" 77 (Reagent.run (Treiber_stack.pop s3) ());
  assert_eq "chain s1 empty" 1
    (if Reagent.run (Treiber_stack.try_pop s1) () = None then 1 else 0);
  assert_eq "chain s2 empty" 1
    (if Reagent.run (Treiber_stack.try_pop s2) () = None then 1 else 0)

(* ── Run ────────────────────────────────────────────────────────────────── *)

let () =
  Printf.printf "Basic Xt transactions:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["single_read", test_single_read;
     "single_write", test_single_write;
     "read_write", test_read_write;
     "modify", test_modify;
     "update", test_update;
     "read_own_write", test_read_own_write;
     "multi_location", test_multi_location];
  Printf.printf "\nretry:\n%!";
  run_test "retry" test_retry;
  Printf.printf "\nor_else:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["first_succeeds", test_or_else_first_succeeds;
     "fallthrough", test_or_else_fallthrough;
     "both_retry", test_or_else_both_retry];
  Printf.printf "\nConcurrent:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["concurrent_transfers", test_concurrent_transfers;
     "concurrent_read_only", test_concurrent_read_only;
     "no_skew", test_no_skew];
  Printf.printf "\nThesis examples — Xt stack:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["xt_stack", test_xt_stack;
     "xt_stack_transfer", test_xt_stack_transfer];
  Printf.printf "\nThesis examples — Counter:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["counter_inc_dec", test_counter_inc_dec;
     "counter_try_dec", test_counter_try_dec;
     "counter_blocking_dec", test_counter_blocking_dec];
  Printf.printf "\nThesis examples — Queue:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["queue", test_queue;
     "queue_empty", test_queue_empty;
     "concurrent_queue", test_concurrent_queue];
  Printf.printf "\nThesis examples — Reagent wrapper:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["reagent_treiber_stack", test_reagent_treiber_stack;
     "reagent_transfer", test_reagent_transfer;
     "reagent_choice", test_reagent_choice;
     "reagent_lifo", test_reagent_lifo;
     "reagent_try_pop", test_reagent_try_pop;
     "reagent_run_opt", test_reagent_run_opt;
     "reagent_blocking_pop", test_reagent_blocking_pop;
     "reagent_lift", test_reagent_lift;
     "reagent_pair", test_reagent_pair;
     "reagent_constant", test_reagent_constant;
     "reagent_concurrent", test_reagent_concurrent;
     "reagent_transfer_chain", test_reagent_transfer_chain];
  Printf.printf "\nAll tests passed.\n%!"
