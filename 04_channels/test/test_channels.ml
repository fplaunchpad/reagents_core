let assert_eq msg expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %d, got %d" msg expected actual

let run_test name f =
  Printf.printf "  %s... %!" name;
  (try f (); Printf.printf "OK\n%!"
   with e -> Printf.printf "FAIL: %s\n%!" (Printexc.to_string e); exit 1)

(* ── Basic swap ─────────────────────────────────────────────────────────── *)

let test_basic_swap () =
  let ep1, ep2 = Channel.mk_chan () in
  let d = Domain.spawn (fun () -> Channel.swap ep1 42) in
  let got = Channel.swap ep2 99 in
  let other = Domain.join d in
  assert_eq "ep2 got" 42 got;
  assert_eq "ep1 got" 99 other

let test_swap_symmetric () =
  (* Both sides send and receive *)
  let ep1, ep2 = Channel.mk_chan () in
  let d = Domain.spawn (fun () -> Channel.swap ep1 1) in
  let got = Channel.swap ep2 2 in
  let other = Domain.join d in
  assert_eq "ep2" 1 got;
  assert_eq "ep1" 2 other

let test_swap_multiple () =
  (* Multiple sequential swaps on the same channel *)
  let ep1, ep2 = Channel.mk_chan () in
  for i = 1 to 10 do
    let d = Domain.spawn (fun () -> Channel.swap ep1 i) in
    let got = Channel.swap ep2 (i * 10) in
    let other = Domain.join d in
    assert_eq (Printf.sprintf "ep2 iter %d" i) i got;
    assert_eq (Printf.sprintf "ep1 iter %d" i) (i * 10) other
  done

(* ── Concurrent ─────────────────────────────────────────────────────────── *)

let test_concurrent_many_swaps () =
  (* Many domains doing swaps concurrently. Each sender sends its id,
     each receiver checks it got a valid id back. *)
  let ep1, ep2 = Channel.mk_chan () in
  let n = 100 in
  let senders = List.init n (fun i ->
    Domain.spawn (fun () -> Channel.swap ep1 i)) in
  let receivers = List.init n (fun i ->
    Domain.spawn (fun () ->
      let got = Channel.swap ep2 (1000 + i) in
      assert_eq "valid range" 1 (if got >= 0 && got < n then 1 else 0);
      got))
  in
  let sender_results = List.map Domain.join senders in
  let _receiver_results = List.map Domain.join receivers in
  (* Each sender should have received a value from 1000..1000+n-1 *)
  List.iter (fun v ->
    assert_eq "sender range" 1
      (if v >= 1000 && v < 1000 + n then 1 else 0)
  ) sender_results

let test_ping_pong () =
  (* Two domains ping-pong values back and forth *)
  let ep1, ep2 = Channel.mk_chan () in
  let n = 50 in
  let d1 = Domain.spawn (fun () ->
    let v = ref 0 in
    for _ = 1 to n do
      v := Channel.swap ep1 !v
    done;
    !v)
  in
  let d2 = Domain.spawn (fun () ->
    let v = ref 1 in
    for _ = 1 to n do
      v := Channel.swap ep2 !v
    done;
    !v)
  in
  let _r1 = Domain.join d1 in
  let _r2 = Domain.join d2 in
  (* Both domains complete without deadlock — that's the main check *)
  ()

(* ── Xt integration (both work independently) ───────────────────────────── *)

let test_channel_with_xt () =
  (* Use channels and Xt transactions independently in the same program *)
  let ep1, ep2 = Channel.mk_chan () in
  let loc = Kcas.make 0 in
  let d = Domain.spawn (fun () ->
    let v = Channel.swap ep1 42 in
    (* After receiving, update a location *)
    Xt.commit (fun ~xt -> Xt.set ~xt loc v))
  in
  let sent = 99 in
  Channel.swap ep2 sent |> ignore;
  Domain.join d;
  assert_eq "xt after swap" 99 (Kcas.get loc)

(* ── Thesis examples ─────────────────────────────────────────────────────── *)

(* Synchronous send/recv channel (thesis p.180-181) *)
let test_send_recv () =
  (* A channel where one side "sends" (payload matters) and the other
     "receives" (payload is unit). Models async message passing. *)
  let ep_send, ep_recv = Channel.mk_chan () in
  let d = Domain.spawn (fun () ->
    (* "send 42" — we send 42, get back unit (0) *)
    Channel.swap ep_send 42) in
  (* "recv" — we send unit (0), get back the message *)
  let msg = Channel.swap ep_recv 0 in
  let _ = Domain.join d in
  assert_eq "recv got" 42 msg

(* Swap + Xt: receive from channel, then atomically update two locations
   (thesis: composing channels with CAS, p.184) *)
let test_recv_then_update () =
  let ep1, ep2 = Channel.mk_chan () in
  let a = Kcas.make 0 in
  let b = Kcas.make 0 in
  let d = Domain.spawn (fun () ->
    let v = Channel.swap ep1 0 in
    (* After receiving, atomically split the value between a and b *)
    Xt.commit (fun ~xt ->
      Xt.set ~xt a (v / 2);
      Xt.set ~xt b (v - v / 2))) in
  Channel.swap ep2 100 |> ignore;
  Domain.join d;
  assert_eq "a+b" 100 (Kcas.get a + Kcas.get b)

(* Producer-consumer pattern (thesis: channels for coordination) *)
let test_producer_consumer () =
  let ep_send, ep_recv = Channel.mk_chan () in
  let sum = Atomic.make 0 in
  let producer = Domain.spawn (fun () ->
    for i = 1 to 10 do
      ignore (Channel.swap ep_send i)
    done) in
  let consumer = Domain.spawn (fun () ->
    for _ = 1 to 10 do
      let v = Channel.swap ep_recv 0 in
      Atomic.fetch_and_add sum v |> ignore
    done) in
  Domain.join producer;
  Domain.join consumer;
  assert_eq "sum 1..10" 55 (Atomic.get sum)

(* ── Run ────────────────────────────────────────────────────────────────── *)

let () =
  Printf.printf "Basic swap:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["basic_swap", test_basic_swap;
     "swap_symmetric", test_swap_symmetric;
     "swap_multiple", test_swap_multiple];
  Printf.printf "\nConcurrent:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["concurrent_many_swaps", test_concurrent_many_swaps;
     "ping_pong", test_ping_pong];
  Printf.printf "\nXt integration:\n%!";
  run_test "channel_with_xt" test_channel_with_xt;
  Printf.printf "\nThesis examples:\n%!";
  List.iter (fun (n,f) -> run_test n f)
    ["send_recv", test_send_recv;
     "recv_then_update", test_recv_then_update;
     "producer_consumer", test_producer_consumer];
  Printf.printf "\nAll tests passed.\n%!"
