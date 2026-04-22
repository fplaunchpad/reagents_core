# 05: Lightweight Threads

Extends [04_channels](../04_channels/) by replacing `Mutex`/`Condition`
blocking with **fiber-level suspension** via OCaml 5 effect handlers.
Adds two modules:

- **Trigger**: a one-shot signaling primitive
- **Sched**: a cooperative round-robin scheduler

Everything else (the CPS reagent, Xt, composable channels) is identical
to 04 — only the blocking mechanism changes.

## Why lightweight threads?

In 04, `Channel.swap` and `Xt.retry` block the OS thread via
`Mutex.lock` / `Condition.wait`. This is fine for a few domains, but
doesn't scale to thousands of concurrent tasks.

With fibers, blocking suspends just the current fiber and switches to
the next runnable one — no OS thread is wasted. A single domain can run
thousands of fibers cooperatively.

## What changed from 04

| File | Change |
|------|--------|
| `trigger.ml`, `sched.ml` | New: one-shot signaling + cooperative scheduler |
| `channel.ml` | Offers use `Trigger` instead of Mutex/Condition. No endpoint lock (cooperative scheduling serializes queue access). |
| `xt.ml` | `block_on_log` uses `Trigger.await` instead of Mutex/Condition |
| `kcas.ml`, `reagent.ml` | unchanged |

## Example

```ocaml
Sched.run (fun () ->
  let ep1, ep2 = Channel.mk_chan () in
  let s = Stack.create () in

  (* Fiber A: swap, then push received value onto stack *)
  Sched.fork (fun () ->
    let composed = Reagent.(Channel.swap ep1 >> Stack.push s) in
    Reagent.run composed 100);

  (* Fiber B: just swap *)
  let _ = Reagent.run (Channel.swap ep2) 42 in
  ...)
```

Fiber A's swap-and-push runs atomically: either the swap completes and
the value gets pushed, or nothing happens.

## Testing

Fiber-level interleaving is cooperative (single domain), so
**QCheck-Lin/STM do not apply** — they require domain-level parallelism.
Tests use deterministic scenarios within `Sched.run`.

```sh
dune test
```

## Reference

The Trigger and Sched modules are from CS6868 (Concurrent Programming,
IIT Madras) lecture 10 on lightweight concurrency.
