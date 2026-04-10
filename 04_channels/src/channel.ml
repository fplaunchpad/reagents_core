(** Swap channels — synchronous rendezvous between two domains.

    A channel connects two endpoints. A [swap] on one endpoint blocks until
    a partner does [swap] on the dual endpoint. The two exchange values
    atomically: if A sends [x] and B sends [y], A receives [y] and B
    receives [x].

    Blocking uses Mutex/Condition (domain-safe, no effect handlers needed).

    {b Limitation:} [swap] is a standalone blocking operation, not composable
    within an [Xt] transaction. Atomically combining a channel swap with a
    CAS update (e.g., [swap ep >> push stack]) would require the CPS reagent
    structure from the original reagents paper. *)

(* ──────────────────────────────────────────────────────────────────────────
   Offers
   ────────────────────────────────────────────────────────────────────────── *)

(** An offer represents a blocked domain waiting for a partner. *)
type 'a offer_status =
  | Pending           (** Waiting for a partner *)
  | Fulfilled of 'a   (** Partner delivered a value *)

type 'a offer = {
  status : 'a offer_status Atomic.t;
  mutex : Mutex.t;
  cond : Condition.t;
}

let make_offer () : 'a offer = {
  status = Atomic.make Pending;
  mutex = Mutex.create ();
  cond = Condition.create ();
}

(** Try to fulfill an offer with a value. Returns [true] if successful
    (the offer was still pending). *)
let fulfill (offer : 'a offer) (v : 'a) : bool =
  if Atomic.compare_and_set offer.status Pending (Fulfilled v) then begin
    Mutex.lock offer.mutex;
    Condition.signal offer.cond;
    Mutex.unlock offer.mutex;
    true
  end else
    false

(** Check if an offer is still pending. *)
let is_pending (offer : 'a offer) : bool =
  match Atomic.get offer.status with Pending -> true | _ -> false

(** Block until the offer is fulfilled or rescinded. Returns the result. *)
let await (offer : 'a offer) : 'a option =
  Mutex.lock offer.mutex;
  while Atomic.get offer.status = Pending do
    Condition.wait offer.cond offer.mutex
  done;
  Mutex.unlock offer.mutex;
  match Atomic.get offer.status with
  | Fulfilled v -> Some v
  | _ -> None

(* ──────────────────────────────────────────────────────────────────────────
   Messages and endpoints
   ────────────────────────────────────────────────────────────────────────── *)

(** A message posted on a channel endpoint's queue. *)
type ('a, 'b) message = {
  payload : 'a;        (** The value being sent *)
  offer : 'b offer;    (** How to deliver the response back to the sender *)
}

(** One end of a channel. Has an outgoing queue (where we post our messages)
    and an incoming queue (where we look for partners). *)
type ('a, 'b) endpoint = {
  outgoing : ('a, 'b) message Queue.t;
  incoming : ('b, 'a) message Queue.t;
  lock : Mutex.t;  (** Protects both queues for this endpoint pair *)
}

(** [mk_chan ()] creates a channel, returning a pair of dual endpoints. *)
let mk_chan () : ('a, 'b) endpoint * ('b, 'a) endpoint =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let lock = Mutex.create () in
  ( { outgoing = q1; incoming = q2; lock },
    { outgoing = q2; incoming = q1; lock } )

(* ──────────────────────────────────────────────────────────────────────────
   Swap
   ────────────────────────────────────────────────────────────────────────── *)

(** Remove fulfilled/rescinded messages from the front of a queue. *)
let clean (q : ('a, 'b) message Queue.t) : unit =
  let rec loop () =
    match Queue.peek_opt q with
    | Some msg when not (is_pending msg.offer) ->
      ignore (Queue.pop q); loop ()
    | _ -> ()
  in
  loop ()

(** [swap ep v] sends [v] on endpoint [ep] and blocks until a partner
    sends on the dual endpoint. Returns the partner's value.

    If multiple partners are waiting, one is chosen (FIFO order). *)
let swap (ep : ('a, 'b) endpoint) (v : 'a) : 'b =
  let rec try_match () =
    (* First, try to find an existing partner in the incoming queue. *)
    Mutex.lock ep.lock;
    clean ep.incoming;
    match Queue.peek_opt ep.incoming with
    | Some msg ->
      ignore (Queue.pop ep.incoming);
      Mutex.unlock ep.lock;
      (* Try to fulfill the partner's offer with our value. *)
      if fulfill msg.offer v then
        (* Success — the partner gets our value, we get theirs. *)
        msg.payload
      else
        (* The partner's offer was already fulfilled/rescinded. Try next. *)
        try_match ()
    | None ->
      (* No partner yet. Post our offer and wait. *)
      let offer = make_offer () in
      let msg = { payload = v; offer } in
      Queue.push msg ep.outgoing;
      Mutex.unlock ep.lock;
      match await offer with
      | Some result -> result
      | None ->
        (* Offer was rescinded (shouldn't happen in normal use). Retry. *)
        try_match ()
  in
  try_match ()
