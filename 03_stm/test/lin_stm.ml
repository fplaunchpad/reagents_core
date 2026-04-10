(** QCheck-Lin linearizability test for Xt transactions and Treiber stack. *)

module StmSig = struct
  type t = {
    a : int Kcas.loc;
    b : int Kcas.loc;
    s : int Reagent.Treiber_stack.stack;
  }

  let init () = {
    a = Kcas.make 0;
    b = Kcas.make 0;
    s = Reagent.Treiber_stack.create ();
  }
  let cleanup _ = ()

  open Lin

  let api =
    [
      val_ "get_a"
        (fun t -> Xt.commit (fun ~xt -> Xt.get ~xt t.a))
        (t @-> returning int);

      val_ "get_b"
        (fun t -> Xt.commit (fun ~xt -> Xt.get ~xt t.b))
        (t @-> returning int);

      val_ "set_a"
        (fun t v -> Xt.commit (fun ~xt -> Xt.set ~xt t.a v))
        (t @-> int @-> returning unit);

      val_ "transfer_a_to_b"
        (fun t ->
          Xt.commit (fun ~xt ->
            let v = Xt.get ~xt t.a in
            Xt.set ~xt t.a (v - 1);
            Xt.set ~xt t.b (Xt.get ~xt t.b + 1)))
        (t @-> returning unit);

      val_ "push"
        (fun t v -> Reagent.run (Reagent.Treiber_stack.push t.s) v)
        (t @-> int @-> returning unit);

      val_ "try_pop"
        (fun t -> Reagent.run_opt (Reagent.Treiber_stack.try_pop t.s) ())
        (t @-> returning (option int));
    ]
end

module StmDomain = Lin_domain.Make (StmSig)

let () =
  QCheck_base_runner.run_tests_main
    [StmDomain.lin_test ~count:1000 ~name:"STM + Reagent linearizability"]
