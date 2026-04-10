# 02: k-CAS + CMP (Read-Only Compare)

Extends [01_kcas](../01_kcas/) with **CMP** operations — read-only assertions
that a location holds an expected value, without writing to it.

## Why CMP?

In 01_kcas, even a "read" that doesn't change a value must use
`CAS(loc, v, v)`, which writes to the location. This causes unnecessary
contention: two transactions that only *read* overlapping locations block each
other.

CMP solves this. A `CMP(loc, expected)` validates the location's value at
commit time without installing a descriptor. Multiple CMPs on the same location
from different transactions proceed in parallel.

## Example

```ocaml
(* Atomically: assert a=10 (read-only), set b:=20 *)
Kcas.atomically [CMP (a, 10); CAS (b, 0, 20)]
```

## How CMP works

The insight is elegant: a CMP descriptor reuses the *existing* state from the
location (which has a different `casn` than the current operation). We
distinguish CMP from CAS by comparing `casn` pointers:

```ocaml
let is_cmp casn state = state.casn != casn
```

Three additions to the GKMZ algorithm:

1. **In `gkmz`**: if a CMP descriptor doesn't match the current state, the
   location has changed → operation fails.

2. **In `finish`**: before declaring success, verify all CMP locations still
   hold their original states.

3. **In `atomically`**: CMP snapshots the existing state from the location
   (reuses it, so `is_cmp` detects it).

This extension is from the kcas documentation
(`gkmz-with-read-only-cmp-ops.md`), not the original GKMZ paper.

## Key types

```ocaml
type op =
  | CAS : 'a loc * 'a * 'a -> op  (* read-write *)
  | CMP : 'a loc * 'a -> op       (* read-only assertion *)
```

## Tests

- All 11 tests from 01 (CAS-only, unchanged)
- 6 new CMP tests: single CMP, mixed CAS+CMP, concurrent CMPs (no contention),
  concurrent CAS+CMP invariant checking

```sh
dune test
```
