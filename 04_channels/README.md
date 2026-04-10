# 04: Swap Channels

Extends [03_stm](../03_stm/) with **synchronous swap channels** — a rendezvous
primitive where two domains exchange values.

## What is a swap channel?

A channel connects two endpoints. When domain A calls `swap` on one endpoint
and domain B calls `swap` on the dual endpoint, they exchange values
atomically: A sends `x` and receives `y`, B sends `y` and receives `x`.

```ocaml
let ep1, ep2 = Channel.mk_chan () in

(* Domain 1 *)
let got = Channel.swap ep1 42    (* sends 42, receives 99 *)

(* Domain 2 *)
let got = Channel.swap ep2 99    (* sends 99, receives 42 *)
```

## How it works

Each endpoint has an outgoing and incoming message queue. `swap`:

1. Checks the incoming queue for a waiting partner
2. If found: fulfill the partner's offer with our value, return their payload
3. If not: post our offer on the outgoing queue, block until a partner arrives

Blocking uses `Mutex`/`Condition` — domain-safe, no effect handlers needed.

### Offers

An offer represents a blocked domain waiting for a partner:

```
Pending ──fulfill(v)──> Fulfilled(v)   (partner delivers value, signals Condition)
```

Fulfilled offers are cleaned from queues by `clean`.

## Limitation

`swap` is a standalone blocking operation — it is **not composable** within an
`Xt` transaction. You cannot atomically combine a channel swap with a CAS
(e.g., `swap ep >> push stack`). That would require the CPS reagent structure
from the original reagents paper.

You can use channels and Xt transactions in the same program, just not in the
same atomic operation.

## Tests

9 tests:
- Basic swap, symmetric exchange, sequential swaps
- Concurrent: many domains swapping, ping-pong
- Xt integration: swap then transactional update
- Thesis examples: send/recv, producer-consumer

```sh
dune test
```
