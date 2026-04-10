(** A one-shot signaling primitive for cooperative scheduling.

    A trigger transitions through a simple state machine:
    {v
      Initial ──signal──> Signaled
         │
      on_signal(cb)
         │
         v
      Waiting(cb) ──signal──> Signaled (callback invoked)
    v}

    Used by {!Sched} to suspend and resume fibers. When a fiber calls
    {!await}, the scheduler registers a callback via {!on_signal} and
    switches to the next runnable fiber. When {!signal} is called later,
    the callback re-enqueues the fiber. *)

type t
(** A one-shot trigger. *)

val create : unit -> t
(** [create ()] returns a new trigger in the [Initial] state. *)

val signal : t -> bool
(** [signal t] transitions [t] to [Signaled]. If a callback was registered
    via {!on_signal}, it is invoked. Returns [false] if already signaled. *)

val on_signal : t -> (unit -> unit) -> bool
(** [on_signal t cb] registers [cb] to be called when [t] is signaled.
    Returns [true] if registered ([Initial] → [Waiting]), [false] if [t]
    was already [Signaled].
    @raise Failure if a callback is already registered. *)

val await : t -> unit
(** [await t] suspends the current fiber until [t] is signaled. Performs
    the {!Await} effect, which must be handled by {!Sched.run}. *)

type _ Effect.t += Await : t -> unit Effect.t
(** The effect performed by {!await}. *)
