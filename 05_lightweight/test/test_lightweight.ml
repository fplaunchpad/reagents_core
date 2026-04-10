let assert_eq msg expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %d, got %d" msg expected actual

let run_test name f =
  Printf.printf "  %s... %!" name;
  (try f (); Printf.printf "OK\n%!"
   with e -> Printf.printf "FAIL: %s\n%!" (Printexc.to_string e); exit 1)

(* ── Channel swap within scheduler ──────────────────────────────────────── *)

let test_basic_swap () =
  let result1 = ref 0 in
  let result2 = ref 0 in
  Sched.run (fun () ->
    let ep1, ep2 = Channel.mk_chan () in
    Sched.fork (fun () -> result1 := Channel.swap ep1 42);
    result2 := Channel.swap ep2 99);
  assert_eq "fiber1 got" 99 !result1;
  assert_eq "fiber2 got" 42 !result2

let test_swap_chain () =
  (* A -> B -> C: A sends to B, B forwards to C *)
  let final = ref 0 in
  Sched.run (fun () ->
    let ab1, ab2 = Channel.mk_chan () in
    let bc1, bc2 = Channel.mk_chan () in
    (* A sends 42 to B *)
    Sched.fork (fun () -> ignore (Channel.swap ab1 42));
    (* B receives from A, forwards to C *)
    Sched.fork (fun () ->
      let v = Channel.swap ab2 0 in
      ignore (Channel.swap bc1 v));
    (* C receives *)
    final := Channel.swap bc2 0);
  assert_eq "chain" 42 !final

let test_many_fibers () =
  let n = 20 in
  let results = Array.make n 0 in
  Sched.run (fun () ->
    let ep1, ep2 = Channel.mk_chan () in
    (* Spawn n sender fibers *)
    for i = 0 to n - 1 do
      Sched.fork (fun () -> ignore (Channel.swap ep1 i))
    done;
    (* Spawn n receiver fibers *)
    for i = 0 to n - 1 do
      Sched.fork (fun () -> results.(i) <- Channel.swap ep2 (100 + i))
    done);
  (* All receivers should have gotten a value 0..n-1 *)
  let sum = Array.fold_left ( + ) 0 results in
  (* Sum of 0..19 = 190 *)
  assert_eq "many fibers sum" (n * (n - 1) / 2) sum

(* ── Xt retry within scheduler ──────────────────────────────────────────── *)

let test_xt_retry_with_fibers () =
  let loc = Kcas.make 0 in
  let result = ref 0 in
  Sched.run (fun () ->
    (* Fiber 1: wait until loc > 0 *)
    Sched.fork (fun () ->
      result := Xt.commit (fun ~xt ->
        let v = Xt.get ~xt loc in
        if v <= 0 then Xt.retry ();
        v));
    (* Fiber 2: yield a bit then set loc *)
    Sched.fork (fun () ->
      Sched.yield ();
      Sched.yield ();
      ignore (Kcas.compare_and_set loc 0 42)));
  assert_eq "retry result" 42 !result

(* ── Treiber stack with fibers ──────────────────────────────────────────── *)

let test_stack_with_fibers () =
  let results = ref [] in
  Sched.run (fun () ->
    let s = Reagent.Treiber_stack.create () in
    (* Producer fiber *)
    Sched.fork (fun () ->
      for i = 1 to 5 do
        Reagent.run (Reagent.Treiber_stack.push s) i;
        Sched.yield ()
      done);
    (* Consumer fiber *)
    Sched.fork (fun () ->
      for _ = 1 to 5 do
        Sched.yield ();
        (match Reagent.run_opt (Reagent.Treiber_stack.try_pop s) () with
         | Some v -> results := v :: !results
         | None -> ())
      done));
  (* Should have gotten some values (interleaving is cooperative) *)
  let count = List.length !results in
  Printf.printf "    (got %d items) " count;
  assert_eq "got items" 1 (if count > 0 then 1 else 0)

(* ── Ping pong with fibers ──────────────────────────────────────────────── *)

let test_ping_pong () =
  let count = ref 0 in
  Sched.run (fun () ->
    let ep1, ep2 = Channel.mk_chan () in
    Sched.fork (fun () ->
      for i = 1 to 10 do
        ignore (Channel.swap ep1 i)
      done);
    Sched.fork (fun () ->
      for _ = 1 to 10 do
        let v = Channel.swap ep2 0 in
        count := !count + v
      done));
  (* Sum of 1..10 = 55 *)
  assert_eq "ping pong" 55 !count

(* ── Thesis examples ─────────────────────────────────────────────────────── *)

(* Producer-consumer with fibers (thesis: channels for coordination) *)
let test_producer_consumer () =
  let sum = ref 0 in
  Sched.run (fun () ->
    let ep_send, ep_recv = Channel.mk_chan () in
    Sched.fork (fun () ->
      for i = 1 to 10 do
        ignore (Channel.swap ep_send i)
      done);
    Sched.fork (fun () ->
      for _ = 1 to 10 do
        sum := !sum + Channel.swap ep_recv 0
      done));
  assert_eq "sum" 55 !sum

(* Counter with blocking dec via fibers (thesis p.180) *)
let test_counter_blocking_fiber () =
  let c = Kcas.make 0 in
  let result = ref 0 in
  Sched.run (fun () ->
    (* Fiber 1: blocking decrement *)
    Sched.fork (fun () ->
      result := Xt.commit (fun ~xt ->
        let v = Xt.get ~xt c in
        if v <= 0 then Xt.retry ();
        Xt.set ~xt c (v - 1); v));
    (* Fiber 2: increment after yielding *)
    Sched.fork (fun () ->
      Sched.yield ();
      ignore (Kcas.compare_and_set c 0 42)));
  assert_eq "counter" 42 !result

(* Atomic stack transfer with fibers (thesis p.184) *)
let test_fiber_stack_transfer () =
  let transferred = ref 0 in
  Sched.run (fun () ->
    let s1 = Reagent.Treiber_stack.create () in
    let s2 = Reagent.Treiber_stack.create () in
    (* Push items to s1 *)
    for i = 1 to 5 do
      Reagent.run (Reagent.Treiber_stack.push s1) i
    done;
    (* Fiber: transfer all from s1 to s2 *)
    Sched.fork (fun () ->
      for _ = 1 to 5 do
        Reagent.run
          Reagent.(Treiber_stack.try_pop s1 >> Treiber_stack.push s2) ()
      done);
    Sched.yield ();
    Sched.yield ();
    (* Count items in s2 *)
    let c = ref 0 in
    for _ = 1 to 5 do
      match Reagent.run_opt (Reagent.Treiber_stack.try_pop s2) () with
      | Some _ -> incr c
      | None -> ()
    done;
    transferred := !c);
  assert_eq "transferred" 5 !transferred

(* ── Run ────────────────────────────────────────────────────────────────── *)

let () =
  Printf.printf "Channel swap (fibers):\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["basic_swap", test_basic_swap;
     "swap_chain", test_swap_chain;
     "many_fibers", test_many_fibers;
     "ping_pong", test_ping_pong];
  Printf.printf "\nXt retry (fibers):\n%!";
  run_test "xt_retry_with_fibers" test_xt_retry_with_fibers;
  Printf.printf "\nTreiber stack (fibers):\n%!";
  run_test "stack_with_fibers" test_stack_with_fibers;
  Printf.printf "\nThesis examples:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["producer_consumer", test_producer_consumer;
     "counter_blocking_fiber", test_counter_blocking_fiber;
     "fiber_stack_transfer", test_fiber_stack_transfer];
  Printf.printf "\nAll tests passed.\n%!"
