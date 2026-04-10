(** QCheck-Lin linearizability test for Treiber stack (via Reagent/Xt)
    and basic Xt transactions, running alongside channel infrastructure.

    Note: Channel.swap itself is blocking and not directly testable with
    Lin (which needs non-blocking operations). We test the Xt/Reagent
    layer that coexists with channels. *)

module Sig = struct
  type t = {
    s : int Reagent.Treiber_stack.stack;
    a : int Kcas.loc;
  }

  let init () = {
    s = Reagent.Treiber_stack.create ();
    a = Kcas.make 0;
  }
  let cleanup _ = ()

  open Lin

  let api =
    [
      val_ "push"
        (fun t v -> Reagent.run (Reagent.Treiber_stack.push t.s) v)
        (t @-> int @-> returning unit);

      val_ "try_pop"
        (fun t -> Reagent.run_opt (Reagent.Treiber_stack.try_pop t.s) ())
        (t @-> returning (option int));

      val_ "get_a"
        (fun t -> Xt.commit (fun ~xt -> Xt.get ~xt t.a))
        (t @-> returning int);

      val_ "inc_a"
        (fun t -> Xt.commit (fun ~xt -> Xt.modify ~xt t.a (fun x -> x + 1)))
        (t @-> returning unit);
    ]
end

module D = Lin_domain.Make (Sig)

let () =
  QCheck_base_runner.run_tests_main
    [D.lin_test ~count:1000 ~name:"Channels layer linearizability"]
