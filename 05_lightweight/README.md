# 05: Lightweight Threads

Extends [04_channels](../04_channels/) by replacing `Mutex`/`Condition` blocking
with **fiber-level suspension** via OCaml 5 effect handlers. Adds two modules:

- **Trigger**: a one-shot signaling primitive
- **Sched**: a cooperative round-robin scheduler

## Why lightweight threads?

In 04_channels, `swap` blocks an OS thread (domain) with `Mutex.lock` /
`Condition.wait`. This is fine for a few domains, but doesn't scale to
thousands of concurrent tasks.

With lightweight threads (fibers), blocking suspends just the current fiber and
switches to the next one — no OS thread is wasted. A single domain can run
thousands of fibers cooperatively.

## Trigger

A one-shot signaling primitive with three states:

```
Initial ──signal──> Signaled
   │
on_signal(cb)
   │
   v
Waiting(cb) ──signal──> Signaled (callback invoked)
```

- `Trigger.create ()` — new trigger in Initial state
- `Trigger.signal t` — transition to Signaled, invoke callback if registered
- `Trigger.on_signal t cb` — register callback (Initial → Waiting)
- `Trigger.await t` — perform the `Await` effect (handled by Sched)

## Sched

A cooperative round-robin scheduler handling three effects:

```ocaml
Sched.run (fun () ->
  Sched.fork (fun () -> ...);   (* spawn a fiber *)
  Sched.yield ();                (* yield to next fiber *)
  Trigger.await trigger)         (* suspend until signaled *)
```

The scheduler's effect handler for `Trigger.Await`:
1. Register the continuation as a callback via `Trigger.on_signal`
2. Switch to the next runnable fiber via `dequeue`
3. When `signal` fires, the callback re-enqueues the continuation

## What changed from 04_channels

- `Channel.swap` uses `Trigger.await` instead of `Condition.wait`
- `Xt.commit`'s retry path uses `Trigger.await` instead of `Condition.wait`
- Channel queues no longer need a `Mutex` (single-domain cooperative scheduling)

## Example: fibers with channels

```ocaml
Sched.run (fun () ->
  let ep1, ep2 = Channel.mk_chan () in

  Sched.fork (fun () ->
    let v = Channel.swap ep1 42 in
    Printf.printf "fiber 1 got %d\n" v);

  let v = Channel.swap ep2 99 in
  Printf.printf "fiber 2 got %d\n" v)
```

## Testing

Fiber-level interleaving is cooperative (single domain), so **QCheck-Lin/STM
do not apply** — they require domain-level parallelism. Instead, tests use
deterministic scenarios within `Sched.run`.

9 tests:
- Channel swap with fibers: basic, chain (A→B→C), many fibers, ping-pong
- Xt retry with fibers: blocking decrement woken by another fiber
- Treiber stack with fibers: producer-consumer
- Thesis examples: producer-consumer, blocking counter, stack transfer

```sh
dune test
```

## Reference

The Trigger and Sched modules are from CS6868 (Concurrent Programming, IIT Madras)
lecture 10 on lightweight concurrency.
