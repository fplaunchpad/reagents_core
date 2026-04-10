(** Reagent combinators — a functional wrapper over [Xt].

    A reagent [('a, 'b) t] is a function from input + transaction log to
    output. It is literally [fun (a : 'a) (xt : Xt.t) -> ... : 'b].

    Sequencing is function composition. Choice is [Xt.or_else].
    Blocking is [Xt.retry]. *)

type 'a ref = 'a Kcas.loc

(** A reagent is just a function: input -> transaction log -> output. *)
type ('a, 'b) t = 'a -> Xt.t -> 'b

(* ── Ref operations ─────────────────────────────────────────────────────── *)

let ref v = Kcas.make v
let get r = Kcas.get r
let read r = fun () xt -> Xt.get ~xt r

let upd r f = fun b xt ->
  let v = Xt.get ~xt r in
  match f v b with
  | Some (v', c) -> Xt.set ~xt r v'; c
  | None -> Xt.retry ()

let cas r expected desired = fun () xt ->
  let v = Xt.get ~xt r in
  if v = expected then Xt.set ~xt r desired
  else Xt.retry ()

let modify r f = fun () xt -> Xt.modify ~xt r f
let set r v = fun () xt -> Xt.set ~xt r v

(* ── Combinators ────────────────────────────────────────────────────────── *)

let (>>) r1 r2 = fun a xt -> r2 (r1 a xt) xt

let (+) r1 r2 = fun a xt ->
  Xt.or_else (fun ~xt -> r1 a xt) (fun ~xt -> r2 a xt) ~xt

let constant v = fun _ _ -> v
let lift f = fun a _ -> f a

let pair r1 r2 = fun (a, b) xt ->
  let c = r1 a xt in
  let d = r2 b xt in
  (c, d)

(* ── Execution ──────────────────────────────────────────────────────────── *)

let run r a = Xt.commit (fun ~xt -> r a xt)

let run_opt r a =
  match Xt.commit (fun ~xt ->
    Xt.or_else (fun ~xt -> Some (r a xt)) (fun ~xt -> ignore xt; None) ~xt)
  with v -> v
