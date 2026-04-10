(** QCheck-STM model-based test for Treiber stack at the channels layer. *)

open QCheck
open STM

type cmd = Push of int | Try_pop

let show_cmd = function
  | Push i -> Printf.sprintf "Push(%d)" i
  | Try_pop -> "Try_pop"

module Spec = struct
  type nonrec cmd = cmd
  type sut = int Reagent.Treiber_stack.stack
  type state = int list

  let show_cmd = show_cmd

  let arb_cmd _state =
    QCheck.make ~print:show_cmd
      (Gen.oneof [
        Gen.map (fun i -> Push i) (Gen.int_range 0 5);
        Gen.return Try_pop;
      ])

  let init_state = []
  let init_sut () = Reagent.Treiber_stack.create ()
  let cleanup _ = ()

  let next_state cmd s = match cmd with
    | Push i -> i :: s
    | Try_pop -> (match s with [] -> [] | _ :: rest -> rest)

  let precond _ _ = true

  let run cmd sut = match cmd with
    | Push i ->
      Reagent.run (Reagent.Treiber_stack.push sut) i;
      Res (unit, ())
    | Try_pop ->
      Res (option int, Reagent.run_opt (Reagent.Treiber_stack.try_pop sut) ())

  let postcond cmd (state : state) res =
    match cmd with
    | Push _ -> (match res with Res ((Unit, _), ()) -> true | _ -> false)
    | Try_pop ->
      match res with
      | Res ((Option Int, _), None) -> state = []
      | Res ((Option Int, _), Some v) ->
        (match state with [] -> false | x :: _ -> Int.equal v x)
      | _ -> false
end

let () =
  let module Seq = STM_sequential.Make (Spec) in
  let module Dom = STM_domain.Make (Spec) in
  let exit_code =
    QCheck_base_runner.run_tests ~verbose:true
      [Seq.agree_test ~count:1000 ~name:"Stack STM sequential (channels layer)";
       Dom.agree_test_par ~count:500 ~name:"Stack STM concurrent (channels layer)"]
  in
  exit exit_code
