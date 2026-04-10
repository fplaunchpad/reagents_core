# 03: STM (Software Transactional Memory)

Extends [02_kcas_cmp](../02_kcas_cmp/) with three modules:
- **Xt**: explicit transaction log with `retry` and `or_else`
- **Reagent**: functional combinator wrapper over Xt
- **Kcas**: k-CAS + CMP + awaiters (for blocking on retry)

## Xt: Transaction Log

Write transactions as normal OCaml code. Reads and writes are logged; the log
is committed atomically via k-CAS.

```ocaml
(* Atomically transfer from a to b *)
Xt.commit (fun ~xt ->
  let v = Xt.get ~xt a in
  Xt.set ~xt a (v - 1);
  Xt.set ~xt b (Xt.get ~xt b + 1))
```

On commit, reads become CMPs, writes become CASes — the whole log is committed
as one k-CAS operation.

### retry

`Xt.retry ()` aborts the transaction and blocks until a read-set location
changes. Follows GHC STM's `retry` semantics.

```ocaml
(* Block until counter > 0, then decrement *)
Xt.commit (fun ~xt ->
  let v = Xt.get ~xt counter in
  if v <= 0 then Xt.retry ();
  Xt.set ~xt counter (v - 1))
```

### or_else

`Xt.or_else tx1 tx2` runs `tx1`; if it calls `retry`, rolls back and runs
`tx2`. Follows GHC STM's `orElse`.

```ocaml
(* Pop from s1; if empty, pop from s2 *)
Xt.commit (fun ~xt ->
  Xt.or_else
    (fun ~xt -> match pop ~xt s1 with Some v -> v | None -> Xt.retry ())
    (fun ~xt -> match pop ~xt s2 with Some v -> v | None -> Xt.retry ())
    ~xt)
```

## Reagent: Combinator Wrapper

A reagent is just `type ('a, 'b) t = 'a -> Xt.t -> 'b` — a function from
input + transaction log to output. The Reagent module provides combinators:

```ocaml
val (>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t   (* sequence *)
val (+)  : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t   (* choice = or_else *)
val upd  : 'a ref -> ('a -> 'b -> ('a * 'c) option) -> ('b, 'c) t
```

Sequencing is function composition. Choice is `or_else`. Blocking is `retry`.

```ocaml
(* Atomic pop-and-push via >> *)
let transfer s1 s2 =
  Reagent.(Treiber_stack.try_pop s1 >> Treiber_stack.push s2)

Reagent.run (transfer s1 s2) ()
```

## Data structures (from Turon's thesis)

The tests include implementations of:
- **Treiber stack**: push, pop, atomic transfer between stacks
- **Counter**: inc, dec (blocking), try_dec
- **Michael-Scott queue**: enq, try_deq

## Tests

24 tests covering:
- Basic Xt: single read/write, multi-location, read-own-write
- `retry`: blocking until a condition is met
- `or_else`: fallthrough, both-retry
- Concurrent: multi-domain transfers, read-only transactions
- Thesis examples: stack, counter, queue, reagent composition

```sh
dune test
```
