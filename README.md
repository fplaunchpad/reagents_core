# Reagents Core

A readable, layered reimplementation of
[reagents](https://aturon.github.io/academic/reagents.pdf) and the
[kcas](https://github.com/ocaml-multicore/kcas) STM for OCaml 5, designed for
**formal verification** and **teaching**.

Each directory is a self-contained OCaml project that builds on the previous
one. A student reads them in order, with each step adding one concept.

## Progression

| Directory | What it adds | Key files |
|-----------|-------------|-----------|
| [01_kcas](01_kcas/) | GKMZ k-CAS algorithm | `kcas.ml` |
| [02_kcas_cmp](02_kcas_cmp/) | CMP read-only compare operations | `kcas.ml` |
| [03_stm](03_stm/) | Xt transaction log, `retry`, `or_else`, simple-function Reagent | `kcas.ml`, `xt.ml`, `reagent.ml` |
| [04_channels](04_channels/) | CPS Reagent + composable swap channels | `reagent.ml`, `channel.ml` |
| [05_lightweight](05_lightweight/) | Fiber scheduler + Trigger (replaces Mutex/Condition) | `trigger.ml`, `sched.ml` |

## Architecture

```
+------------------------------+
|  05: Lightweight threads     |  Trigger + Sched (replaces Mutex/Condition)
+------------------------------+
|  04: Composable channels     |  CPS reagent + swap via rendezvous
+------------------------------+
|  03: STM + simple reagents   |  Xt transaction log + function-based reagent
+------------------------------+
|  02: k-CAS + CMP             |  GKMZ + read-only compare
+------------------------------+
|  01: k-CAS                   |  GKMZ algorithm
+------------------------------+
```

## Building

Each directory is independent. To build and test any of them:

```sh
cd 03_stm
dune build
dune test
```

## Dependencies

- OCaml >= 5.0 (for `Atomic` and effect handlers)
- `qcheck-lin`, `qcheck-stm` (for linearizability/model-based tests in 01, 02)

## References

- **GKMZ paper**: "Efficient Multi-word Compare and Swap" — Guerraoui, Kogan,
  Marathe, Zablotchi (DISC 2020, [arXiv:2008.02527](https://arxiv.org/abs/2008.02527))
- **Reagents**: "Reagents: Expressing and Composing Fine-grained Concurrency" —
  Aaron Turon (PLDI 2012)
- **GHC STM**: `retry` and `orElse` semantics follow Haskell's STM model
- **Reference kcas**: [ocaml-multicore/kcas](https://github.com/ocaml-multicore/kcas) —
  the production implementation this project aims to make verifiable
