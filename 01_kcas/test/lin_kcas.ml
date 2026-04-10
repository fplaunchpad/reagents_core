(** QCheck-Lin linearizability test for k-CAS.

    Tests that concurrent operations on shared locations are linearizable:
    every concurrent execution can be explained by some sequential interleaving. *)

module KcasSig = struct
  type t = {
    a : int Kcas.loc;
    b : int Kcas.loc;
  }

  let init () = { a = Kcas.make 0; b = Kcas.make 0 }
  let cleanup _ = ()

  open Lin

  let api =
    [
      val_ "get_a"
        (fun t -> Kcas.get t.a)
        (t @-> returning int);

      val_ "get_b"
        (fun t -> Kcas.get t.b)
        (t @-> returning int);

      val_ "cas_a"
        (fun t o n -> Kcas.compare_and_set t.a o n)
        (t @-> int @-> int @-> returning bool);

      val_ "cas_b"
        (fun t o n -> Kcas.compare_and_set t.b o n)
        (t @-> int @-> int @-> returning bool);

      val_ "mcas_ab"
        (fun t oa na ob nb ->
          Kcas.atomically [Kcas.CAS (t.a, oa, na); Kcas.CAS (t.b, ob, nb)])
        (t @-> int @-> int @-> int @-> int @-> returning bool);
    ]
end

module KcasDomain = Lin_domain.Make (KcasSig)

let () =
  QCheck_base_runner.run_tests_main
    [
      KcasDomain.lin_test ~count:1000 ~name:"k-CAS linearizability";
    ]
