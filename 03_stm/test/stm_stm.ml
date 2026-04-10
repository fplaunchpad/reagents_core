(** QCheck-STM model-based test for Xt transactions on two locations. *)

open QCheck
open STM

type cmd =
  | Get_a
  | Get_b
  | Set_a of int
  | Set_b of int
  | Transfer_a_to_b
  | Swap_ab

let show_cmd = function
  | Get_a -> "Get_a" | Get_b -> "Get_b"
  | Set_a v -> Printf.sprintf "Set_a(%d)" v
  | Set_b v -> Printf.sprintf "Set_b(%d)" v
  | Transfer_a_to_b -> "Transfer_a_to_b"
  | Swap_ab -> "Swap_ab"

type sut = { a : int Kcas.loc; b : int Kcas.loc }

module Spec = struct
  type nonrec cmd = cmd
  type nonrec sut = sut
  type state = { ma : int; mb : int }

  let show_cmd = show_cmd

  let arb_cmd _state =
    let small = Gen.int_range 0 3 in
    QCheck.make ~print:show_cmd
      (Gen.oneof [
        Gen.return Get_a;
        Gen.return Get_b;
        Gen.map (fun v -> Set_a v) small;
        Gen.map (fun v -> Set_b v) small;
        Gen.return Transfer_a_to_b;
        Gen.return Swap_ab;
      ])

  let init_state = { ma = 0; mb = 0 }
  let init_sut () = { a = Kcas.make 0; b = Kcas.make 0 }
  let cleanup _ = ()

  let next_state cmd s = match cmd with
    | Get_a | Get_b -> s
    | Set_a v -> { s with ma = v }
    | Set_b v -> { s with mb = v }
    | Transfer_a_to_b -> { ma = s.ma - 1; mb = s.mb + 1 }
    | Swap_ab -> { ma = s.mb; mb = s.ma }

  let precond _ _ = true

  let run cmd sut = match cmd with
    | Get_a -> Res (int, Xt.commit (fun ~xt -> Xt.get ~xt sut.a))
    | Get_b -> Res (int, Xt.commit (fun ~xt -> Xt.get ~xt sut.b))
    | Set_a v -> Res (unit, Xt.commit (fun ~xt -> Xt.set ~xt sut.a v))
    | Set_b v -> Res (unit, Xt.commit (fun ~xt -> Xt.set ~xt sut.b v))
    | Transfer_a_to_b ->
      Res (unit, Xt.commit (fun ~xt ->
        let v = Xt.get ~xt sut.a in
        Xt.set ~xt sut.a (v - 1);
        Xt.set ~xt sut.b (Xt.get ~xt sut.b + 1)))
    | Swap_ab ->
      Res (unit, Xt.commit (fun ~xt ->
        let va = Xt.get ~xt sut.a in
        let vb = Xt.get ~xt sut.b in
        Xt.set ~xt sut.a vb;
        Xt.set ~xt sut.b va))

  let postcond cmd state res = match cmd, res with
    | Get_a, Res ((Int, _), v) -> v = state.ma
    | Get_b, Res ((Int, _), v) -> v = state.mb
    | Set_a _, Res ((Unit, _), ()) -> true
    | Set_b _, Res ((Unit, _), ()) -> true
    | Transfer_a_to_b, Res ((Unit, _), ()) -> true
    | Swap_ab, Res ((Unit, _), ()) -> true
    | _, _ -> false
end

let () =
  let module Seq = STM_sequential.Make (Spec) in
  let module Dom = STM_domain.Make (Spec) in
  let exit_code =
    QCheck_base_runner.run_tests ~verbose:true
      [Seq.agree_test ~count:1000 ~name:"Xt STM sequential";
       Dom.agree_test_par ~count:500 ~name:"Xt STM concurrent"]
  in
  exit exit_code
