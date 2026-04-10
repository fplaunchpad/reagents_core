(** Swap channels using fiber-level suspension via Trigger.

    Same semantics as [04_channels], but blocking uses [Trigger.await]
    instead of Mutex/Condition. This means swap must run within [Sched.run]. *)

(* ── Offers ─────────────────────────────────────────────────────────────── *)

type 'a offer = {
  mutable result : 'a option;
  trigger : Trigger.t;
}

let make_offer () : 'a offer = {
  result = None;
  trigger = Trigger.create ();
}

let fulfill (offer : 'a offer) (v : 'a) : bool =
  offer.result <- Some v;
  Trigger.signal offer.trigger

let is_pending (offer : 'a offer) : bool =
  offer.result = None

(* ── Endpoints ──────────────────────────────────────────────────────────── *)

type ('a, 'b) message = {
  payload : 'a;
  offer : 'b offer;
}

type ('a, 'b) endpoint = {
  outgoing : ('a, 'b) message Queue.t;
  incoming : ('b, 'a) message Queue.t;
}

let mk_chan () : ('a, 'b) endpoint * ('b, 'a) endpoint =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  ( { outgoing = q1; incoming = q2 },
    { outgoing = q2; incoming = q1 } )

(* ── Swap ───────────────────────────────────────────────────────────────── *)

let clean (q : ('a, 'b) message Queue.t) : unit =
  let rec loop () =
    match Queue.peek_opt q with
    | Some msg when not (is_pending msg.offer) ->
      ignore (Queue.pop q); loop ()
    | _ -> ()
  in
  loop ()

let swap (ep : ('a, 'b) endpoint) (v : 'a) : 'b =
  let rec try_match () =
    clean ep.incoming;
    match Queue.peek_opt ep.incoming with
    | Some msg ->
      ignore (Queue.pop ep.incoming);
      if fulfill msg.offer v then
        msg.payload
      else
        try_match ()
    | None ->
      let offer = make_offer () in
      Queue.push { payload = v; offer } ep.outgoing;
      (* Suspend this fiber until a partner fulfills our offer *)
      Trigger.await offer.trigger;
      match offer.result with
      | Some result -> result
      | None -> try_match ()
  in
  try_match ()
