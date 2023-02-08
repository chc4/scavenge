Scavenge
========

Compacting region allocator in Rust that uses lifetimes in order to guarantee that values aren't kept alive across the compaction step without updating the possibly-moved references. In order to call Arena::compact(), you first need to "tokenize" all alive references; these tokens can then be "redeemed" back into references again, but can't outlive the end of the compaction step. Rust lifetimes encode these usage invariants, making incorrect usage a compile-time error.

Example:
```rust
    make_guard!(guard);
    // create an arena and allocate some values
    let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
    let mut garbage = arena.allocate::<u8>(&state)?;
    let mut a = arena.allocate::<u8>(&state)?;
    *a = 0xAA;
    let mut b = arena.allocate::<u8>(&state)?;
    *b = 0xBB;

    make_guard!(compact);
    // convert all outstanding references to tokens
    let tok_a = arena.tokenize(&compact, a);
    let tok_b = arena.tokenize(&compact, b);
    drop(garbage);
    // start compacting, which guarantees all references are tokens or dropped
    let state = arena.compact(&compact, state);
    // redeem tokens, reviving alive objects and overwriting the free `garbage`
    let b = tok_b(&arena);
    let a = tok_a(&arena);
    // finish compacting, guaranteeing all tokens are used or dropped
    arena.finish(compact, state);
    // live references have been forwarded and point to the same objects
    assert_eq!(*a, 0xAA);
    assert_eq!(*b, 0xBB);
```
