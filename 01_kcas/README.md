# 01: k-CAS (Multi-Word Compare-and-Swap)

Implements the GKMZ algorithm from "Efficient Multi-word Compare and Swap"
(Guerraoui, Kogan, Marathe, Zablotchi — DISC 2020).

This is the foundation. Everything else builds on top of it.

## What is k-CAS?

A single-word CAS atomically updates one memory location:

```ocaml
(* If loc holds 1, set it to 2; returns true on success *)
Kcas.compare_and_set loc 1 2
```

A **k-word CAS** atomically updates multiple locations — either all succeed or
none do:

```ocaml
(* Atomically: if a=1 and b=2, set a:=10 and b:=20 *)
Kcas.atomically [CAS (a, 1, 10); CAS (b, 2, 20)]
```

## How it works

The GKMZ algorithm proceeds in two phases:

1. **Acquire**: Walk the list of word descriptors and install our state
   descriptor into each target location via single-word CAS. If we encounter a
   location owned by another active operation, help that operation complete
   first (cooperative helping — ensures lock-freedom).

2. **Finalize**: Atomically set the operation status to success or failure via
   a single CAS on the status word. This is the (k+1)-th CAS that makes the
   whole operation appear atomic.

Key properties:
- **Lock-free**: at least one thread always makes progress (via helping)
- **Linearizable**: the operation takes effect at the finalization point
- **ABA-safe**: fresh allocations per operation prevent ABA problems

## Key types

```ocaml
type 'a loc                             (* shared memory location *)
type op = CAS : 'a loc * 'a * 'a -> op  (* single CAS within a k-CAS *)

val make : 'a -> 'a loc
val get : 'a loc -> 'a
val compare_and_set : 'a loc -> 'a -> 'a -> bool
val atomically : op list -> bool
```

## Limitations

- No read-only compare operations → see [02_kcas_cmp](../02_kcas_cmp/)
- No blocking/awaiter mechanism → callers must spin on retry

## Tests

- 11 unit tests (sequential + concurrent)
- QCheck-Lin linearizability tests
- QCheck-STM model-based tests

```sh
dune test
```
