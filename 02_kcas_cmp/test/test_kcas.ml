open Kcas

(* ──────────────────────────────────────────────────────────────────────────
   Helpers
   ────────────────────────────────────────────────────────────────────────── *)

let assert_eq msg expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %d, got %d" msg expected actual

let assert_true msg b =
  if not b then failwith (msg ^ ": expected true")

let assert_false msg b =
  if b then failwith (msg ^ ": expected false")

(* ──────────────────────────────────────────────────────────────────────────
   Sequential tests
   ────────────────────────────────────────────────────────────────────────── *)

let test_make_and_get () =
  let loc = make 42 in
  assert_eq "make/get" 42 (get loc)

let test_single_cas_success () =
  let loc = make 1 in
  assert_true "cas success" (compare_and_set loc 1 2);
  assert_eq "after cas" 2 (get loc)

let test_single_cas_failure () =
  let loc = make 1 in
  assert_false "cas failure" (compare_and_set loc 999 2);
  assert_eq "unchanged" 1 (get loc)

let test_mcas_two_words_success () =
  let a = make 10 in
  let b = make 20 in
  let ok = atomically [CAS (a, 10, 11); CAS (b, 20, 21)] in
  assert_true "mcas success" ok;
  assert_eq "a after" 11 (get a);
  assert_eq "b after" 21 (get b)

let test_mcas_two_words_failure () =
  let a = make 10 in
  let b = make 20 in
  (* b's expected value is wrong *)
  let ok = atomically [CAS (a, 10, 11); CAS (b, 999, 21)] in
  assert_false "mcas failure" ok;
  (* Both locations should be unchanged *)
  assert_eq "a unchanged" 10 (get a);
  assert_eq "b unchanged" 20 (get b)

let test_mcas_three_words () =
  let a = make 1 in
  let b = make 2 in
  let c = make 3 in
  let ok = atomically [CAS (a, 1, 10); CAS (b, 2, 20); CAS (c, 3, 30)] in
  assert_true "3-word mcas" ok;
  assert_eq "a" 10 (get a);
  assert_eq "b" 20 (get b);
  assert_eq "c" 30 (get c)

let test_mcas_empty () =
  (* Empty k-CAS should trivially succeed *)
  assert_true "empty mcas" (atomically [])

let test_mcas_sequential_multiple () =
  let a = make 0 in
  let b = make 0 in
  for i = 0 to 99 do
    let ok = atomically [CAS (a, i, i + 1); CAS (b, i, i + 1)] in
    assert_true (Printf.sprintf "mcas iter %d" i) ok
  done;
  assert_eq "a final" 100 (get a);
  assert_eq "b final" 100 (get b)

let test_mcas_partial_overlap () =
  (* First operation succeeds *)
  let a = make 0 in
  let b = make 0 in
  let c = make 0 in
  assert_true "first" (atomically [CAS (a, 0, 1); CAS (b, 0, 1)]);
  (* Second operation: a is now 1, so expected=0 fails *)
  assert_false "second" (atomically [CAS (a, 0, 2); CAS (c, 0, 1)]);
  assert_eq "a" 1 (get a);
  assert_eq "b" 1 (get b);
  assert_eq "c" 0 (get c)

(* ──────────────────────────────────────────────────────────────────────────
   CMP tests
   ────────────────────────────────────────────────────────────────────────── *)

let test_cmp_success () =
  let a = make 10 in
  assert_true "cmp success" (atomically [CMP (a, 10)])

let test_cmp_failure () =
  let a = make 10 in
  assert_false "cmp failure" (atomically [CMP (a, 999)])

let test_cas_with_cmp () =
  (* CAS on a, CMP on b — succeeds when both conditions hold *)
  let a = make 1 in
  let b = make 2 in
  assert_true "cas+cmp" (atomically [CAS (a, 1, 10); CMP (b, 2)]);
  assert_eq "a updated" 10 (get a);
  assert_eq "b unchanged" 2 (get b)

let test_cas_with_cmp_failure () =
  (* CMP on b fails — whole operation aborted, a unchanged *)
  let a = make 1 in
  let b = make 2 in
  assert_false "cmp fails" (atomically [CAS (a, 1, 10); CMP (b, 999)]);
  assert_eq "a unchanged" 1 (get a)

let test_concurrent_cmps_no_contention () =
  (* Multiple domains doing CMP-only operations on the same location
     should all succeed — CMPs don't write, so no contention. *)
  let loc = make 42 in
  let n = 10_000 in
  let successes = Atomic.make 0 in
  let domains = List.init 4 (fun _ ->
    Domain.spawn (fun () ->
      for _ = 1 to n do
        if atomically [CMP (loc, 42)] then
          Atomic.incr successes
      done))
  in
  List.iter Domain.join domains;
  assert_eq "all CMPs succeed" (4 * n) (Atomic.get successes)

let test_concurrent_cas_with_cmp () =
  (* Some domains read (CMP) while others write (CAS).
     Invariant: a + b = 100 always holds. *)
  let a = make 50 in
  let b = make 50 in
  let invariant_violations = Atomic.make 0 in
  let domains = List.init 4 (fun i ->
    Domain.spawn (fun () ->
      for _ = 1 to 2_000 do
        if i mod 2 = 0 then begin
          (* Writer: transfer 1 from a to b *)
          let va = get a in
          let vb = get b in
          let _ok = atomically [CAS (a, va, va - 1); CAS (b, vb, vb + 1)] in
          ()
        end else begin
          (* Reader: check invariant using CMPs *)
          let va = get a in
          let vb = get b in
          if atomically [CMP (a, va); CMP (b, vb)] then
            (* If the CMP succeeded, va and vb were a consistent snapshot *)
            if va + vb <> 100 then Atomic.incr invariant_violations
        end
      done))
  in
  List.iter Domain.join domains;
  assert_eq "no invariant violations" 0 (Atomic.get invariant_violations);
  assert_eq "final invariant" 100 (get a + get b)

(* ──────────────────────────────────────────────────────────────────────────
   Concurrent tests
   ────────────────────────────────────────────────────────────────────────── *)

let test_concurrent_conflicting_cas () =
  (* Two domains race to CAS the same location. Exactly one should win. *)
  let loc = make 0 in
  let n = 10_000 in
  let count1 = Atomic.make 0 in
  let count2 = Atomic.make 0 in
  let d1 = Domain.spawn (fun () ->
    for _ = 1 to n do
      let v = get loc in
      if compare_and_set loc v (v + 1) then
        Atomic.incr count1
    done)
  in
  let d2 = Domain.spawn (fun () ->
    for _ = 1 to n do
      let v = get loc in
      if compare_and_set loc v (v + 1) then
        Atomic.incr count2
    done)
  in
  Domain.join d1;
  Domain.join d2;
  let total = Atomic.get count1 + Atomic.get count2 in
  assert_eq "concurrent cas total" total (get loc)

let test_concurrent_mcas_invariant () =
  (* Multiple domains do 2-word CAS on (a, b), maintaining a + b = 100.
     After all domains finish, a + b should still be 100. *)
  let a = make 50 in
  let b = make 50 in
  let n = 5_000 in
  let num_domains = 4 in
  let domains = List.init num_domains (fun _ ->
    Domain.spawn (fun () ->
      for _ = 1 to n do
        let va = get a in
        let vb = get b in
        (* Transfer 1 from a to b *)
        let _ok = atomically [CAS (a, va, va - 1); CAS (b, vb, vb + 1)] in
        ()
      done))
  in
  List.iter Domain.join domains;
  let sum = get a + get b in
  assert_eq "invariant a+b=100" 100 sum

(* ──────────────────────────────────────────────────────────────────────────
   Run all tests
   ────────────────────────────────────────────────────────────────────────── *)

let run_test name f =
  Printf.printf "  %s... %!" name;
  (try f (); Printf.printf "OK\n%!"
   with e -> Printf.printf "FAIL: %s\n%!" (Printexc.to_string e);
             exit 1)

let () =
  Printf.printf "Sequential tests:\n%!";
  run_test "make_and_get" test_make_and_get;
  run_test "single_cas_success" test_single_cas_success;
  run_test "single_cas_failure" test_single_cas_failure;
  run_test "mcas_two_words_success" test_mcas_two_words_success;
  run_test "mcas_two_words_failure" test_mcas_two_words_failure;
  run_test "mcas_three_words" test_mcas_three_words;
  run_test "mcas_empty" test_mcas_empty;
  run_test "mcas_sequential_multiple" test_mcas_sequential_multiple;
  run_test "mcas_partial_overlap" test_mcas_partial_overlap;
  Printf.printf "\nCMP tests:\n%!";
  run_test "cmp_success" test_cmp_success;
  run_test "cmp_failure" test_cmp_failure;
  run_test "cas_with_cmp" test_cas_with_cmp;
  run_test "cas_with_cmp_failure" test_cas_with_cmp_failure;
  run_test "concurrent_cmps_no_contention" test_concurrent_cmps_no_contention;
  run_test "concurrent_cas_with_cmp" test_concurrent_cas_with_cmp;
  Printf.printf "\nConcurrent tests:\n%!";
  run_test "concurrent_conflicting_cas" test_concurrent_conflicting_cas;
  run_test "concurrent_mcas_invariant" test_concurrent_mcas_invariant;
  Printf.printf "\nAll tests passed.\n%!"
