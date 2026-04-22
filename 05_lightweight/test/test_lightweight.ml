(** Tests for composable channels with lightweight threads (fibers). *)

let assert_eq msg expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %d, got %d" msg expected actual

let run_test name f =
  Printf.printf "  %s... %!" name;
  (try f (); Printf.printf "OK\n%!"
   with e -> Printf.printf "FAIL: %s\n%!" (Printexc.to_string e); exit 1)

module Treiber_stack = struct
  type 'a t = 'a list Reagent.ref [@@warning "-34"]
  let create () = Reagent.ref []
  let push s = Reagent.upd s (fun xs x -> Some (x :: xs, ()))
  let try_pop s = Reagent.upd s (fun xs () ->
    match xs with [] -> None | x :: xs' -> Some (xs', x))
end

(* ── Basic swap with fibers ─────────────────────────────────────────────── *)

let test_basic_swap () =
  let got1 = ref 0 in
  let got2 = ref 0 in
  Sched.run (fun () ->
    let ep1, ep2 = Channel.mk_chan () in
    Sched.fork (fun () -> got1 := Reagent.run (Channel.swap ep1) 42);
    got2 := Reagent.run (Channel.swap ep2) 99);
  assert_eq "fiber1 got 99" 99 !got1;
  assert_eq "fiber2 got 42" 42 !got2

(* ── Composable swap with fibers ────────────────────────────────────────── *)

let test_swap_then_push () =
  (* Fiber A: swap then push received value onto stack.
     Fiber B: just swap. The whole thing is one atomic operation. *)
  let popped = ref 0 in
  Sched.run (fun () ->
    let ep1, ep2 = Channel.mk_chan () in
    let s = Treiber_stack.create () in
    Sched.fork (fun () ->
      let composed = Reagent.(Channel.swap ep1 >> Treiber_stack.push s) in
      Reagent.run composed 100);
    let _ = Reagent.run (Channel.swap ep2) 42 in
    popped := Reagent.run (Treiber_stack.try_pop s) ());
  assert_eq "popped 42" 42 !popped

(* ── Ping-pong across fibers ────────────────────────────────────────────── *)

let test_ping_pong () =
  let d1_sum = ref 0 in
  let d2_sum = ref 0 in
  Sched.run (fun () ->
    let ep1, ep2 = Channel.mk_chan () in
    let n = 10 in
    Sched.fork (fun () ->
      for i = 1 to n do
        d1_sum := !d1_sum + Reagent.run (Channel.swap ep1) i
      done);
    for i = 1 to 10 do
      d2_sum := !d2_sum + Reagent.run (Channel.swap ep2) (i * 100)
    done);
  (* d1 sent 1..10, received 100..1000; sum of received = 5500 *)
  assert_eq "d1 received" 5500 !d1_sum;
  (* d2 sent 100..1000, received 1..10; sum of received = 55 *)
  assert_eq "d2 received" 55 !d2_sum

(* ── Xt retry with fibers ───────────────────────────────────────────────── *)

let test_xt_retry () =
  let result = ref 0 in
  Sched.run (fun () ->
    let loc = Kcas.make 0 in
    Sched.fork (fun () ->
      result := Xt.commit (fun ~xt ->
        let v = Xt.get ~xt loc in
        if v <= 0 then Xt.retry ();
        v));
    Sched.yield ();
    Sched.yield ();
    ignore (Kcas.compare_and_set loc 0 42));
  assert_eq "retry woke up" 42 !result

(* ── Stack operations across fibers ─────────────────────────────────────── *)

let test_stack_concurrent () =
  let count = ref 0 in
  Sched.run (fun () ->
    let s = Treiber_stack.create () in
    let n = 50 in
    (* Producer fibers *)
    for _ = 1 to 4 do
      Sched.fork (fun () ->
        for i = 1 to n do
          Reagent.run (Treiber_stack.push s) i
        done)
    done;
    (* Consumer: pops everything *)
    let local = Stdlib.ref 0 in
    let go = Stdlib.ref true in
    while !go do
      Sched.yield ();  (* give producers a chance *)
      let rec drain () =
        match Reagent.run_opt (Treiber_stack.try_pop s) () with
        | Some _ -> incr local; drain ()
        | None -> ()
      in
      drain ();
      if !local >= 4 * n then go := false
    done;
    count := !local);
  assert_eq "total count" 200 !count

(* ── Run ────────────────────────────────────────────────────────────────── *)

let () =
  Printf.printf "Basic swap (fibers):\n%!";
  run_test "basic_swap" test_basic_swap;
  Printf.printf "\nComposable swap (fibers):\n%!";
  run_test "swap_then_push" test_swap_then_push;
  run_test "ping_pong" test_ping_pong;
  Printf.printf "\nXt retry (fibers):\n%!";
  run_test "xt_retry" test_xt_retry;
  Printf.printf "\nConcurrent stack (fibers):\n%!";
  run_test "stack_concurrent" test_stack_concurrent;
  Printf.printf "\nAll tests passed.\n%!"
