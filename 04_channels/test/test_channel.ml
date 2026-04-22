(** Tests for composable swap channels. *)

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

(* ── Basic swap ─────────────────────────────────────────────────────────── *)

let test_basic_swap () =
  let ep1, ep2 = Channel.mk_chan () in
  let d = Domain.spawn (fun () -> Reagent.run (Channel.swap ep1) 42) in
  let got = Reagent.run (Channel.swap ep2) 99 in
  let other = Domain.join d in
  assert_eq "ep2 got 42" 42 got;
  assert_eq "ep1 got 99" 99 other

let test_multiple_swaps () =
  let ep1, ep2 = Channel.mk_chan () in
  for i = 1 to 10 do
    let d = Domain.spawn (fun () -> Reagent.run (Channel.swap ep1) i) in
    let got = Reagent.run (Channel.swap ep2) (i * 10) in
    let other = Domain.join d in
    assert_eq (Printf.sprintf "iter %d ep2" i) i got;
    assert_eq (Printf.sprintf "iter %d ep1" i) (i * 10) other
  done

(* ── Composable swap: the whole point of CPS reagents ────────────────────── *)

let test_swap_then_push () =
  (* Thread A: swap ep1, then push received value to stack.
     Thread B: just swap ep2.
     Atomically: A receives B's payload AND pushes it to stack. *)
  let ep1, ep2 = Channel.mk_chan () in
  let s = Treiber_stack.create () in
  let d = Domain.spawn (fun () ->
    (* swap then push — composed! *)
    let composed = Reagent.(Channel.swap ep1 >> Treiber_stack.push s) in
    Reagent.run composed 100) in
  let received = Reagent.run (Channel.swap ep2) 42 in
  Domain.join d;
  assert_eq "B received 100" 100 received;
  (* Stack should have 42 (what A received and pushed) *)
  let popped = Reagent.run (Treiber_stack.try_pop s) () in
  assert_eq "stack has 42" 42 popped

let test_swap_composed_with_cas () =
  (* A: swap, then CAS a location based on received value.
     B: swap, then CAS another location.
     After both complete, both CASes have been applied atomically with their respective swaps. *)
  let ep1, ep2 = Channel.mk_chan () in
  let a = Reagent.ref 0 in
  let b = Reagent.ref 0 in
  (* We need: swap ep, then write received value to location.
     Use upd to capture "received value" as input and write it to ref. *)
  let store r = Reagent.upd r (fun _ v -> Some (v, ())) in
  let d = Domain.spawn (fun () ->
    let r = Reagent.(Channel.swap ep1 >> store a) in
    Reagent.run r 10) in
  let _ = Reagent.run Reagent.(Channel.swap ep2 >> store b) 20 in
  Domain.join d;
  (* A sent 10, received 20, wrote 20 to a.
     B sent 20, received 10, wrote 10 to b. *)
  assert_eq "a = 20" 20 (Reagent.get a);
  assert_eq "b = 10" 10 (Reagent.get b)

(* Note: [swap + alternative] for fall-through requires a two-phase offer
   protocol (try without offer first, post offer only if all alternatives
   block). Not implemented in this minimal version. *)

(* ── Ping pong ──────────────────────────────────────────────────────────── *)

let test_ping_pong () =
  let ep1, ep2 = Channel.mk_chan () in
  let n = 20 in
  let d1 = Domain.spawn (fun () ->
    let acc = ref 0 in
    for i = 1 to n do
      acc := !acc + Reagent.run (Channel.swap ep1) i
    done;
    !acc) in
  let d2 = Domain.spawn (fun () ->
    let acc = ref 0 in
    for i = 1 to n do
      acc := !acc + Reagent.run (Channel.swap ep2) (i * 100)
    done;
    !acc) in
  let s1 = Domain.join d1 in
  let s2 = Domain.join d2 in
  (* d1 sent 1..n (sum = n*(n+1)/2), received i*100 (sum = 100*n*(n+1)/2)
     d2 sent i*100, received i (sum = n*(n+1)/2) *)
  let expected1 = 100 * n * (n + 1) / 2 in
  let expected2 = n * (n + 1) / 2 in
  assert_eq "d1 sum" expected1 s1;
  assert_eq "d2 sum" expected2 s2

(* ── Run ────────────────────────────────────────────────────────────────── *)

let () =
  Printf.printf "Basic swap:\n%!";
  List.iter (fun (n, f) -> run_test n f)
    ["basic_swap", test_basic_swap;
     "multiple_swaps", test_multiple_swaps];
  Printf.printf "\nComposable swap:\n%!";
  List.iter (fun (n, f) -> run_test n f)
    ["swap_then_push", test_swap_then_push;
     "swap_composed_with_cas", test_swap_composed_with_cas];
  Printf.printf "\nPing-pong:\n%!";
  run_test "ping_pong" test_ping_pong;
  Printf.printf "\nAll tests passed.\n%!"
