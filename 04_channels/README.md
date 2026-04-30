# 04: Composable Swap Channels

Adds **swap channels** (synchronous rendezvous) that compose with other
reagent operations. This is the full reagents abstraction from the original
paper (Turon, PLDI 2012).

## The key upgrade: CPS reagents

[03_stm](../03_stm/)'s reagent was a simple function:
```ocaml
type ('a, 'b) t = 'a -> Xt.t -> 'b
```

This can't express composable channel swap. Why? When `swap ep >> k` runs,
swap needs to post its *continuation* (`k`) to the channel. A matching
partner then runs `k` with the partner's payload before committing the
combined transaction. The simple function type has no way to expose `k`.

So in 04 we switch to a CPS (continuation-passing) record:
```ocaml
type 'a result = Done of 'a | Block | Retry | BlockAndRetry

type ('a, 'b) t = {
  try_react : 'a -> Xt.t -> 'b offer option -> 'b result;
  seq : 'c. ('b, 'c) t -> ('a, 'c) t;
  always_commits : bool;
}
```

The `seq` field is the explicit continuation mechanism. `r1 >> r2 = r1.seq r2`.

The `try_react` function takes an optional `'b offer` (where the final
result is delivered if the reagent blocks). This enables the two-phase
offer protocol needed for `swap + alternative`.

## `commit` is itself a reagent

The single most important algorithmic point: every primitive's chain ends
in a first-class `commit` reagent (constructed via `make_reagent body`).
On a kCAS conflict, `commit.try_react` returns `Retry`, which propagates
up to `+` and dispatches to the alternative branch.

This is what makes the elimination-stack pattern actually work under
contention:

```ocaml
let push s = Reagent.(Treiber.push s + Channel.swap elim_push)
```

When `Treiber.push`'s kCAS loses to a concurrent push, `commit` returns
`Retry`, `+` falls through to `Channel.swap`, and the push can rendezvous
with a popping partner instead of fighting the stack head again.

Without commit-as-reagent, `upd`-built `push` would always return `Done`
from its body (push has no precondition), the kCAS conflict would only
surface to the *outer* `Xt.commit`'s silent retry loop, and `+` would
never see a `Retry`. The elimination side would be dead code on the
contended path. (See `test/test_elimination.ml` for the regression
test.)

## `BlockAndRetry`

The fourth result-state lets `+` distinguish "one branch blocked, another
retried transiently" from pure block or pure retry. The run loop uses
this to decide whether to enter phase 2 (with offer) or just spin in
phase 1.

| r1            | r2            | result        |
|---------------|---------------|---------------|
| Done          | (skipped)     | Done          |
| Block         | Done          | Done          |
| Block         | Block         | Block         |
| Block         | Retry         | BlockAndRetry |
| Retry         | Done          | Done          |
| Retry         | Block         | BlockAndRetry |
| Retry         | Retry         | Retry         |
| BlockAndRetry | non-Done      | BlockAndRetry |

## Composable swap

```ocaml
(* Atomically receive from channel and push onto stack: *)
let recv_and_push ep stack =
  Reagent.(Channel.swap ep >> Treiber_stack.push stack)

Reagent.run (recv_and_push ep s) 0
```

When one side finds no partner, it blocks. When a partner arrives, the
two transactions (each side's pre-swap and post-swap ops) commit
atomically as a single k-CAS — including a CAS on the partner's offer
location that transitions it to `Completed v`. Offer fulfilment is part
of the kCAS payload, not a post-commit hook.

## Implementation sketch

1. Thread A runs `swap ep >> k`. At the swap, A has accumulated Xt ops.
2. Looking in the incoming queue: no partner. A posts
   `Message(payload, k, offer, snapshot)` to the outgoing queue and
   parks on `offer`.
3. Thread B runs `swap dual_ep`. It finds A's message.
4. B builds: `merged = partner_k >> swap_k partner_payload partner_offer >> k_B`
5. B runs `merged` with B's payload:
   - Runs A's `k` with B's payload → A's final result `r_A`
   - `swap_k` does `Xt.set ~xt partner_offer (Completed r_A)` — adds a
     CAS to the merged Xt log.
   - Continues with B's own continuation
6. B's tail `commit` fires kCAS atomically: A's pre-swap ops, B's ops,
   the offer CAS — all-or-nothing.
7. The kCAS fires awaiters on the offer loc; A's parked thread wakes up
   and reads `Completed r_A`.

## Two-phase offer protocol

`run` attempts the reagent in two phases:

1. **Without offer** (`without_offer`): try synchronously. On `Done`,
   return. On `Retry`, pause and re-try phase 1. On `Block` or
   `BlockAndRetry`, enter phase 2.
2. **With offer** (`with_offer`): create an offer (a `Kcas.loc` carrying
   `Empty | Completed _ | Rescinded`), try once with it. On `Done`,
   return. On `Block`, install kCAS awaiters on every read-set location
   so any precondition change rescinds the offer (waking us); then park
   on a Mutex+Condition signaled by an awaiter on the offer loc. On
   `Retry`/`BlockAndRetry`, pause briefly, try to rescind the offer (in
   case a partner fulfilled it concurrently), then loop with a fresh
   offer.

This makes these work:
```ocaml
(* Fall through to default if no partner: *)
Channel.swap ep + constant 42

(* Offer on two channels, take whichever matches first: *)
Channel.swap ep1 + Channel.swap ep2
```

## `+` invariant: seq distribution

`(r1 + r2).seq k = (r1.seq k) + (r2.seq k)`. Composition pushes into
both branches so each branch ends in its own `commit` at its own tail.
`+` snapshots `Xt` before `r1`, runs r1, and on any non-`Done` rolls
the snapshot back before running `r2` — and the snapshot covers a
complete branch including its commit. **Any new combinator nested under
`+` must preserve this distribution**, otherwise `r1`'s body mutations
would leak into `r2`.

## Scope

- **`pair` / `<*>` is omitted.** The previous `pair` ran two reagents
  independently with separate `try_react` calls — incompatible with the
  commit-at-tail rule (would fire two extra commits). A faithful
  reimplementation as `lift dup >> first r1 >> second r2` requires
  `first`/`second` CPS combinators; deferred.
- **`always_commits` flag** is wired (`+`, `commit`, `swap`) but the
  immediate-CAS optimisation in `upd` is not yet implemented — every
  commit goes through `Kcas.atomically` even for single-CAS chains.
  Performance optimisation, not a correctness issue.
- **Catalysts** out of scope.

## Tests

- `test_channel`: basic swap, composable swap, choice, ping-pong.
- `test_stm`: full STM + Reagent suite (the same suite as 03_stm).
- `test_elimination`: deterministic regression test for commit-time
  Retry dispatch + smoke test of the elimination stack.
- `lin_stm` / `stm_stm`: linearizability and QCheck-STM property tests.
