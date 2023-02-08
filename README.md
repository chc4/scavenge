Scavenge
========

Compacting region allocator in Rust that uses lifetimes in order to guarantee that values aren't kept alive across the compaction step without updating the possibly-moved references. In order to call Arena::compact(), you first need to "tokenize" all alive references; these tokens can then be "redeemed" back into references again, but can't outlive the end of the compaction step. Rust lifetimes encode these usage invariants, making incorrect usage a compile-time error.

# Example:
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

#Downsides:

I mean. There's a few. You have to destructure any collections that are keeping around Item references in order to convert them to Tokens, because it's a compile-time type-level check - the Item and Token reprs should be the same, but Rust probably isn't very smart about knowing that round-tripping a Vec::into_iter through the conversions can be optimized.

The Arena is a variable sized allocator, and so you can allocate whatever data you want in it - but also if you don't redeem tokens in ascending address order, you aren't guaranteed to get optimal (or even very good) space usage, due to the compactor not being able to move all the items at once and wasting bytes on padding or two-small gaps. If that's important, use per-type Arenas instead, which prevent fragmentation since all items can fit all free spaces.

I had the idea of being able to convert an `Item<WithHeader<T>>` into a `Pin<Box<Forward<T>>>` pointer, which would be automatically fixed-up by the compactor (with the allocation being able to keep the Box's pointer address to fixup inline with the item header, since it is immovable), so that you could automatically(tm) update the forwarding pointers after compaction without having to tear-down and rebuild datastructures (at the cost of deref being two pointer chases instead). That scheme would probably be able to help a bit with compacting the region, since it would be able to fixup all the outstanding `Forward<T>` items in ascending order all at once, before all of the `Token<T>` items. I didn't implement that part yet, though.

Also I don't actually have proof any of this is actually sound. Maybe I horribly misunderstand how lifetimes or `generativity` guards work.
