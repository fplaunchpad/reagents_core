(** k-CAS + CMP + awaiters (GKMZ algorithm).

    Extends [02_kcas_cmp] with an awaiter mechanism for blocking support.
    Locations carry a separate awaiter list. After a successful [atomically],
    awaiters on all touched locations are fired, waking waiting threads.

    This is the k-CAS layer underneath the Xt transaction API. *)

(* ──────────────────────────────────────────────────────────────────────────
   Types
   ────────────────────────────────────────────────────────────────────────── *)

type awaiter = unit -> unit

type 'a state = {
  before : 'a;
  after : 'a;
  casn : casn;
}
and cass = CASS : 'a loc * 'a state -> cass
and casn = status Atomic.t
and status =
  | Undetermined of cass list
  | After
  | Before

(** A shared memory location with a separate awaiter list. *)
and 'a loc = {
  state : 'a state Atomic.t;
  awaiters : awaiter list Atomic.t;
}

type op =
  | CAS : 'a loc * 'a * 'a -> op
  | CMP : 'a loc * 'a -> op

(* ──────────────────────────────────────────────────────────────────────────
   GKMZ Algorithm (operates on loc.state)
   ────────────────────────────────────────────────────────────────────────── *)

let is_cmp casn state = state.casn != casn

let finish casn desired =
  match Atomic.get casn with
  | After -> true
  | Before -> false
  | Undetermined cass as current ->
    let desired =
      if desired == After
         && List.exists (fun (CASS (loc, state)) ->
              is_cmp casn state && Atomic.get loc.state != state) cass
      then Before
      else desired
    in
    Atomic.compare_and_set casn current desired |> ignore;
    Atomic.get casn == After

let rec gkmz casn = function
  | [] -> finish casn After
  | (CASS (loc, desired) :: continue) as retry ->
    let current = Atomic.get loc.state in
    if desired == current then
      gkmz casn continue
    else if is_cmp casn desired then
      finish casn Before
    else
      let current_value =
        if is_after current.casn then current.after
        else current.before
      in
      if current_value != desired.before then
        finish casn Before
      else
        match Atomic.get casn with
        | Undetermined _ ->
          if Atomic.compare_and_set loc.state current desired then
            gkmz casn continue
          else
            gkmz casn retry
        | After -> true
        | Before -> false

and is_after casn =
  match Atomic.get casn with
  | Undetermined cass -> gkmz casn cass
  | After -> true
  | Before -> false

(* ──────────────────────────────────────────────────────────────────────────
   Awaiter mechanism
   ────────────────────────────────────────────────────────────────────────── *)

let fire_awaiters (loc : 'a loc) =
  let rec swap () =
    let current = Atomic.get loc.awaiters in
    match current with
    | [] -> ()
    | _ ->
      if Atomic.compare_and_set loc.awaiters current [] then
        List.iter (fun f -> f ()) current
      else swap ()
  in
  swap ()

let fire_awaiters_for_ops ops =
  List.iter (function
    | CAS (loc, _, _) -> fire_awaiters loc
    | CMP (loc, _) -> fire_awaiters loc) ops

let add_awaiter (loc : 'a loc) (f : awaiter) : bool =
  let current = Atomic.get loc.awaiters in
  Atomic.compare_and_set loc.awaiters current (f :: current)

let remove_awaiter (loc : 'a loc) (f : awaiter) : unit =
  let rec loop () =
    let current = Atomic.get loc.awaiters in
    let filtered = List.filter (fun g -> g != f) current in
    if Atomic.compare_and_set loc.awaiters current filtered then ()
    else loop ()
  in
  loop ()

(* ──────────────────────────────────────────────────────────────────────────
   Public API
   ────────────────────────────────────────────────────────────────────────── *)

let make (v : 'a) : 'a loc =
  let casn = Atomic.make After in
  { state = Atomic.make { before = v; after = v; casn };
    awaiters = Atomic.make [] }

let get (loc : 'a loc) : 'a =
  let s = Atomic.get loc.state in
  if is_after s.casn then s.after
  else s.before

let atomically (ops : op list) : bool =
  let casn = Atomic.make After in
  let cass =
    ops
    |> List.map (function
      | CAS (loc, before, after) -> CASS (loc, { before; after; casn })
      | CMP (loc, expected) ->
        let current = Atomic.get loc.state in
        if get loc != expected || Atomic.get loc.state != current then
          raise Exit
        else
          CASS (loc, current))
  in
  Atomic.set casn (Undetermined cass);
  let result = gkmz casn cass in
  if result then fire_awaiters_for_ops ops;
  result

let atomically ops =
  try atomically ops with Exit -> false

let compare_and_set (loc : 'a loc) (expected : 'a) (desired : 'a) : bool =
  atomically [CAS (loc, expected, desired)]
