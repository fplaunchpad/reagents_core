(** QCheck-STM model-based test for k-CAS.

    Compares the k-CAS implementation against a simple sequential model
    (plain integer values) to verify that every concurrent execution is
    consistent with the model's predictions. *)

open QCheck
open STM

(** The shared state under test: two k-CAS locations. *)
type sut = {
  a : int Kcas.loc;
  b : int Kcas.loc;
}

(** Commands that can be executed on the shared state. *)
type cmd =
  | Get_a
  | Get_b
  | CAS_a of int * int
  | CAS_b of int * int
  | MCAS_ab of int * int * int * int  (* oa, na, ob, nb *)

let show_cmd = function
  | Get_a -> "Get_a"
  | Get_b -> "Get_b"
  | CAS_a (e, d) -> Printf.sprintf "CAS_a(%d, %d)" e d
  | CAS_b (e, d) -> Printf.sprintf "CAS_b(%d, %d)" e d
  | MCAS_ab (oa, na, ob, nb) ->
    Printf.sprintf "MCAS_ab(%d, %d, %d, %d)" oa na ob nb

module Spec = struct
  type nonrec cmd = cmd
  type nonrec sut = sut

  (** Model state: just two integers. *)
  type state = { ma : int; mb : int }

  let show_cmd = show_cmd

  let arb_cmd _state =
    let small = Gen.int_range 0 3 in
    QCheck.make ~print:show_cmd
      (Gen.oneof [
        Gen.return Get_a;
        Gen.return Get_b;
        Gen.map2 (fun e d -> CAS_a (e, d)) small small;
        Gen.map2 (fun e d -> CAS_b (e, d)) small small;
        Gen.map4 (fun oa na ob nb -> MCAS_ab (oa, na, ob, nb))
          small small small small;
      ])

  let init_state = { ma = 0; mb = 0 }

  let init_sut () = { a = Kcas.make 0; b = Kcas.make 0 }
  let cleanup _ = ()

  (** Update the model: apply the command sequentially. *)
  let next_state cmd s =
    match cmd with
    | Get_a | Get_b -> s
    | CAS_a (exp, des) ->
      if s.ma = exp then { s with ma = des } else s
    | CAS_b (exp, des) ->
      if s.mb = exp then { s with mb = des } else s
    | MCAS_ab (oa, na, ob, nb) ->
      if s.ma = oa && s.mb = ob then { ma = na; mb = nb } else s

  let precond _ _ = true

  (** Execute the command on the real implementation. *)
  let run cmd sut =
    match cmd with
    | Get_a -> Res (int, Kcas.get sut.a)
    | Get_b -> Res (int, Kcas.get sut.b)
    | CAS_a (exp, des) -> Res (bool, Kcas.compare_and_set sut.a exp des)
    | CAS_b (exp, des) -> Res (bool, Kcas.compare_and_set sut.b exp des)
    | MCAS_ab (oa, na, ob, nb) ->
      Res (bool,
        Kcas.atomically [Kcas.CAS (sut.a, oa, na); Kcas.CAS (sut.b, ob, nb)])

  (** Check that the real result matches the model's prediction. *)
  let postcond cmd state res =
    match cmd, res with
    | Get_a, Res ((Int, _), v) -> v = state.ma
    | Get_b, Res ((Int, _), v) -> v = state.mb
    | CAS_a (exp, _), Res ((Bool, _), ok) -> ok = (state.ma = exp)
    | CAS_b (exp, _), Res ((Bool, _), ok) -> ok = (state.mb = exp)
    | MCAS_ab (oa, _, ob, _), Res ((Bool, _), ok) ->
      ok = (state.ma = oa && state.mb = ob)
    | _, _ -> false
end

let () =
  let module Seq = STM_sequential.Make (Spec) in
  let module Dom = STM_domain.Make (Spec) in
  let test_name =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "all"
  in
  let exit_code =
    match test_name with
    | "sequential" | "seq" ->
      QCheck_base_runner.run_tests ~verbose:true
        [Seq.agree_test ~count:1000 ~name:"k-CAS STM sequential"]
    | "concurrent" | "conc" ->
      QCheck_base_runner.run_tests ~verbose:true
        [Dom.agree_test_par ~count:500 ~name:"k-CAS STM concurrent"]
    | "all" | _ ->
      QCheck_base_runner.run_tests ~verbose:true
        [Seq.agree_test ~count:1000 ~name:"k-CAS STM sequential";
         Dom.agree_test_par ~count:500 ~name:"k-CAS STM concurrent"]
  in
  exit exit_code
