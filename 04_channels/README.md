# 04: Composable Swap Channels

Adds **swap channels** (synchronous rendezvous) that compose with other
reagent operations. This is the full reagents abstraction from the original
paper.

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
type ('a, 'b) t = {
  try_react : 'a -> Xt.t -> 'b result;
  seq : 'c. ('b, 'c) t -> ('a, 'c) t;
}
```

The `seq` field is the explicit continuation mechanism. `r1 >> r2 = r1.seq r2`.

## Composable swap

```ocaml
(* Atomically receive from channel and push onto stack: *)
let recv_and_push ep stack =
  Reagent.(Channel.swap ep >> Treiber_stack.push stack)

Reagent.run (recv_and_push ep s) 0
```

When one side finds no partner, it blocks. When a partner arrives, the two
transactions (each side's pre-swap and post-swap ops) commit atomically
as a single k-CAS — with offer fulfillment scheduled as a post-commit
action so the result is delivered only if the whole thing succeeds.

## Implementation sketch

1. Thread A runs `swap ep >> k`. At the swap, A has accumulated Xt ops.
2. Looking in the incoming queue: no partner. A posts
   `Message(payload, k, offer)` to the outgoing queue and blocks on `offer`.
3. Thread B runs `swap dual_ep`. It finds A's message.
4. B builds: `merged = partner_k >> swap_k partner_payload partner_offer >> k_B`
5. B runs `merged` with B's payload:
   - Runs A's `k` with B's payload → A's final result `r_A`
   - Schedules `fulfill(partner_offer, r_A)` as post-commit action
   - Continues with B's own continuation
6. B commits atomically (Xt ops from both sides + post-commit fulfillment).
7. A wakes up with `r_A`.

## Tests

- Basic swap between two domains
- Multiple sequential swaps
- Composable swap: `swap >> push stack`, `swap >> store ref`
- Ping-pong between two domains

## Limitations

`swap + alternative` (choice where swap falls through if no partner) requires
a two-phase protocol (try without offer first, post offer only if all
alternatives block). Not implemented in this minimal version.
