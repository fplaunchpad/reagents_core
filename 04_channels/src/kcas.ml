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
  mcas_desc : mcas_desc;
}
and word_desc = Word_desc : 'a loc * 'a state -> word_desc
and mcas_desc = status Atomic.t
and status =
  | Undetermined of word_desc list
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

let is_cmp mcas_desc state = state.mcas_desc != mcas_desc

let finish mcas_desc desired =
  match Atomic.get mcas_desc with
  | After -> true
  | Before -> false
  | Undetermined word_desc as current ->
    let desired =
      if desired == After
         && List.exists (fun (Word_desc (loc, state)) ->
              is_cmp mcas_desc state && Atomic.get loc.state != state) word_desc
      then Before
      else desired
    in
    Atomic.compare_and_set mcas_desc current desired |> ignore;
    Atomic.get mcas_desc == After

let rec gkmz mcas_desc = function
  | [] -> finish mcas_desc After
  | (Word_desc (loc, desired) :: continue) as retry ->
    let current = Atomic.get loc.state in
    if desired == current then
      gkmz mcas_desc continue
    else if is_cmp mcas_desc desired then
      finish mcas_desc Before
    else
      let current_value =
        if is_after current.mcas_desc then current.after
        else current.before
      in
      if current_value != desired.before then
        finish mcas_desc Before
      else
        match Atomic.get mcas_desc with
        | Undetermined _ ->
          if Atomic.compare_and_set loc.state current desired then
            gkmz mcas_desc continue
          else
            gkmz mcas_desc retry
        | After -> true
        | Before -> false

and is_after mcas_desc =
  match Atomic.get mcas_desc with
  | Undetermined word_desc -> gkmz mcas_desc word_desc
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
  let mcas_desc = Atomic.make After in
  { state = Atomic.make { before = v; after = v; mcas_desc };
    awaiters = Atomic.make [] }

let get (loc : 'a loc) : 'a =
  let s = Atomic.get loc.state in
  if is_after s.mcas_desc then s.after
  else s.before

let atomically (ops : op list) : bool =
  let mcas_desc = Atomic.make After in
  let word_desc =
    ops
    |> List.map (function
      | CAS (loc, before, after) -> Word_desc (loc, { before; after; mcas_desc })
      | CMP (loc, expected) ->
        let current = Atomic.get loc.state in
        if get loc != expected || Atomic.get loc.state != current then
          raise Exit
        else
          Word_desc (loc, current))
  in
  Atomic.set mcas_desc (Undetermined word_desc);
  let result = gkmz mcas_desc word_desc in
  if result then fire_awaiters_for_ops ops;
  result

let atomically ops =
  try atomically ops with Exit -> false

let compare_and_set (loc : 'a loc) (expected : 'a) (desired : 'a) : bool =
  atomically [CAS (loc, expected, desired)]
