#![feature(unboxed_closures, fn_traits, inline_const, ptr_sub_ptr, pin_macro)]
use generativity::{make_guard, Guard, Id};
use roaring::{RoaringBitmap};
use core::sync::atomic::{Ordering, AtomicUsize};
use std::alloc::Layout;
use core::cell::RefCell;
use core::marker::PhantomData;
use core::pin::{Pin, pin};
use std::ops::{Deref, DerefMut};

pub struct Arena<'life, const SIZE: usize> {
    // these refcells are kinda stupid: we use the presence of &mut Arenas in order
    // to encode API lifetime invariants, and so don't have mutability in functions
    // that do work. could probably just have them be LCells and put the LCellOwner
    // in the arena itself?
    current: RefCell<Layout>,
    bytes: *mut [u8; SIZE],
    bitmap: RefCell<RoaringBitmap>,
    _token: PhantomData<Id<'life>>,
}

impl<'life, const SIZE: usize> Drop for Arena<'life, SIZE> {
    fn drop(&mut self) {
        let Arena { current, ref bytes, bitmap, _token } = self;
        unsafe {
            let bytes = Box::from_raw(*bytes);
            drop(bytes);
        }
        drop(current);
        drop(bitmap);
        drop(_token);
        core::mem::forget(self);
    }
}

#[repr(transparent)]
pub struct Item<'life, T> {
    data: T,
    _phantom: PhantomData<Id<'life>>,
}

#[repr(transparent)]
pub struct Token<'life, T, COMPACT> {
    ptr: *mut T,
    _phantom: PhantomData<Id<'life>>,
    _compact: PhantomData<COMPACT>,
}

impl<'life, T, COMPACT> Drop for Token<'life, T, COMPACT> {
    fn drop<'a>(&'a mut self) {
        // It's safe to just drop a Token. It will consume space via Arena bitmap,
        // but only until the next compaction cycle. We can also run Drop for the
        // item, due to COMPACT guaranteeing we are either being dropped before
        // the guard goes out of scope, or are leaked (in which case Drop is never ran).
        let data: &'a mut T = unsafe { core::mem::transmute(self.ptr) };
        drop(data);
    }
}

impl<'a, 'life, 'compact, T: 'a, const SIZE: usize> FnOnce<(&'a Arena<'life, SIZE>,)> for Token<'life, T, &Guard<'compact>> {
    type Output = Item<'life, &'a mut T>;

    extern "rust-call" fn call_once(self, arena: (&'a Arena<'life, SIZE>,)) -> Self::Output {
        // This token is being redeemed, meaning it's an alive object; we move the
        // object to the end of the compacted region.
        //
        // SAFETY: The token is branded with a 'life ID, which must match the Arena
        // that it is paired with in order to be redeemed and revive the object.
        // Likewise, the token has a borrow for a Guard lifetime for the compaction step,
        // and thus can't be redeemed after compacting has finished.
        // only reallocate if the item isn't already in the compact region
        let Token { ptr: mut data , .. } = self;
        if data as usize > arena.0.current.borrow().size() {
            println!("moving {:?}", self.ptr);
            // mark our *own* allocation as free, so that e.g. [A][B][C] can
            // become [A][C][free] if sizeof(B)<sizeof(C).
            let off = data as usize - arena.0.bytes as usize;
            arena.0.bitmap.borrow_mut().insert_range(off as u32..(off+core::mem::size_of::<T>()) as u32);
            println!("{:?}", arena.0.bitmap.borrow());
            // this unwrap shouldn't ever fail, since our memory usage usually shouldn't (can't?)
            // *increase* during a compaction
            let new_loc = loop {
                let start: u32 = (arena.0.current.borrow().size() as usize - arena.0.bytes as usize).try_into().unwrap();
                let new_loc = arena.0.reserve::<T>().unwrap();
                let end: u32 = (arena.0.current.borrow().size() as usize - arena.0.bytes as usize).try_into().unwrap();
                // the bitmap is the available *holes*, not alive values, thanks
                // to Arena::compact(). if we're able to clear the entire region,
                // then it was free and we can allocate into it.
                if arena.0.bitmap.borrow_mut().remove_range(start..end) == (start..end).len() as u64 {
                    break new_loc
                }
                // else jump to the next free space
                *arena.0.current.borrow_mut() = Layout::from_size_align(
                    arena.0.bytes as usize + arena.0.bitmap.borrow().min().unwrap() as usize, 1).unwrap();
            };
            unsafe {
                core::ptr::copy(data, new_loc, 1);
                data = new_loc;
            }
        }
        let data: &mut T = unsafe { core::mem::transmute(data) };
        core::mem::forget(self); // don't run destructor, which would free self.ptr
        Item { data, _phantom: PhantomData }
    }
}

impl<'life, T: 'life> core::ops::Deref for Item<'life, &mut T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl<'life, T: 'life> core::ops::DerefMut for Item<'life, &mut T> {
    fn deref_mut(&mut self) -> &mut <Self as core::ops::Deref>::Target {
        &mut self.data
    }
}

impl<'life, const SIZE: usize> Arena<'life, SIZE> {
    pub fn new<'a>(id: Guard<'a>) -> (Arena<'a, SIZE>, Allocating<'a>) {
        let bytes = Box::into_raw(Box::new([0; SIZE]));
        (Arena {
            current: RefCell::new(Layout::from_size_align(bytes as usize, 1).unwrap()),
            bytes,
            bitmap: RefCell::new(RoaringBitmap::new()),
            _token: Default::default(),
        }, Allocating(id.into()))
    }

    fn reserve<'a, T>(&'a self) -> Result<*mut T, Box<dyn std::error::Error>> {
        let start = self.current.borrow().clone();
        let (end, next) = start.extend(Layout::new::<T>())?;
        *self.current.borrow_mut() = end;
        println!("item at {}, next at {}", next - self.bytes as usize, end.size() - self.bytes as usize);
        unsafe {
            Ok((self.bytes as *mut u8).add(next - self.bytes as usize) as *mut T)
        }
    }

    pub fn allocate<'a, T: Default>(&'a self, allocating: &Allocating<'life>) -> Result<Item<'life, &'a mut T>, Box<dyn std::error::Error>> {
        let item = self.reserve::<T>()?;
        unsafe {
            println!("{:x} {}", item as usize, core::mem::align_of::<T>());
            core::ptr::write(item, Default::default());
            let item = core::mem::transmute::<*mut T, &'a mut T>(item);
            Ok(Item { data: item, _phantom: PhantomData })
        }
    }

    /// Tokenize an allocated item into a forwarding pointer, which removes the
    /// tie to the Arena lifetime until redeemed again.
    ///
    /// The tokens use a "lifetime two phase commit" mechanism to make sure that
    /// 1) there are no outstanding direct Item references when compacting starts, only Tokens
    /// 2) there are no outstanding Tokens when compacting ends, only Items
    ///
    /// Objects are marked as live when they are tokenized, not when they are redeemed.
    /// This means that if you are compacting memory and create tokens for a set
    /// of objects, but only redeem half of them and drop the rest, the memory
    /// for the unredeemed half will not be reclaimed (until the next compaction set).
    pub fn tokenize<'a, 'compact, 'fresh, T>(&self, guard: &'fresh Guard<'compact>, item: Item<'life, &'a mut T>) -> Token<'life, T, &'fresh Guard<'compact>> {
        // Mark the item's region as used
        let ptr = (item.data as *mut T as usize) - self.bytes as usize;
        let ptr_trunc: u32 = ptr.try_into().unwrap();
        println!("marking {}..={} as alive", ptr_trunc, ptr_trunc+(core::mem::size_of::<T>() as u32));
        self.bitmap.borrow_mut().insert_range(ptr_trunc..ptr_trunc+(core::mem::size_of::<T>() as u32));
        Token { ptr: item.data as *mut T, _phantom: PhantomData, _compact: PhantomData }
    }


    /// Start the compaction cycle for a region.
    /// This uses branded lifetimes to tie a a unique "currently executing compaction
    /// cycle" lifetime guard to the state input, which must be completed before subsequent
    /// compactions can start, or new allocations can be created.
    ///
    /// No outstanding Item<T> references can exist across this call; all alive
    /// references must be "tokenized" (or dropped) in order to mark them alive
    /// before this is called, or else you'll get confusing lifetime errors.
    pub fn compact<'compact>(&mut self, guard: &Guard<'compact>, compact: Allocating<'life>) -> Compacting<'compact, 'life> {
        // now we've started compaction, and it's impossible to any more values to
        // tokenize themselves and mark themselves alive.

        let top = self.bitmap.borrow().max().unwrap_or(0) as usize;

        // create the inverse of our alive bitmap; this lets us quickly find empty
        // regions.
        let mut full = RoaringBitmap::new();
        full.insert_range(0..=<usize as TryInto<u32>>::try_into(top + 1).unwrap());
        use std::ops::BitXor;
        let alive = self.bitmap.borrow();
        let dif = full.bitxor(&*alive);
        println!("dif {:?}", dif);
        // advance the bump pointer to the first free byte in the region
        *self.current.borrow_mut() = Layout::from_size_align(
            self.bytes as usize + dif.min().unwrap_or(0) as usize, 1).unwrap();

        // set out bitmap to the inverted one
        drop(alive);
        *self.bitmap.borrow_mut() = dif;

        // we can't have Arena itself be a linear state transitioning type, and have
        // to use a seperate parameter. this is because although we could move self
        // in this, we can't move it again in the corrosponding finish() call, due to
        // token redeeming having to re-borrow the Item with the new lifetime after
        // compacting.
        Compacting(compact.0, PhantomData)
    }

    /// Finish the compaction cycle for a region.
    /// This requires the same branded lifetime guard as was used to start the compaction
    /// cycle.
    ///
    /// No outstanding Token<T> references can exist across this call; all objects
    /// that should have been kept alive must be "redeemed" (or dropped) in order
    /// to revive them in the compacted region, or else you'll get confusing
    /// lifetime errors.
    pub fn finish<'compact>(&self, guard: Guard<'compact>, allocatable: Compacting<'compact, 'life>) -> Allocating<'life> {
        // we're finished compacting, since there's now no way for any tokens to be
        // redeemed now that we've consumed the guard. we can now blow away the
        // marked metadata bits for the next collection cycle.
        // it's possible there were some tokens that were created but not redeemed,
        // in which case there's no way for their bits to be set by tokenizing
        // again *next* cycle, guaranteeing no leaked objects can survive two
        // compactions.
        self.bitmap.borrow_mut().clear();
        Allocating(allocatable.0)
    }
}

pub struct Compacting<'compact, 'life>(Id<'life>, PhantomData<Id<'compact>>);
pub struct Allocating<'life>(Id<'life>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizing_two() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut a = arena.allocate::<u8>(&state)?;
        let mut a2 = arena.allocate::<[u32;12]>(&state)?;
        assert_eq!(*a, 0);
        *a = 1;
        assert_eq!(a2[0], 0);
        make_guard!(compact);
        let a_tok = arena.tokenize(&compact, a);
        let state = arena.compact(&compact, state);
        let a = a_tok(&arena);
        let state = arena.finish(compact, state);
        assert_eq!(*a, 1);
        Ok(())
    }

    #[test]
    fn tokenizing_vec() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut v = vec![];
        for i in 0..10 {
            let mut item = arena.allocate::<u8>(&state)?;
            *item = i;
            v.push(item);
        }
        make_guard!(compact);
        let mut tok_v = v.drain(..).map(|i| arena.tokenize(&compact, i) ).collect::<Vec<_>>();
        let state = arena.compact(&compact, state);
        let v = tok_v.into_iter().map(|t| t(&arena) ).collect::<Vec<_>>();
        arena.finish(compact, state);
        for i in 0..10 {
            assert_eq!(*v[i], i as u8);
        }
        Ok(())
    }

    #[test]
    fn tokenizing_vec_reclaiming() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut v = vec![];
        for i in 0..10 {
            let mut item = arena.allocate::<u8>(&state)?;
            *item = i;
            v.push(item);
        }
        make_guard!(compact);
        let mut tok_v = v.drain(5..10).map(|i| arena.tokenize(&compact, i) ).collect::<Vec<_>>();
        let state = arena.compact(&compact, state);
        let v = tok_v.into_iter().map(|t| t(&arena) ).collect::<Vec<_>>();
        arena.finish(compact, state);
        for i in 0..5 {
            assert_eq!(*v[i], 5+i as u8);
        }
        Ok(())
    }

    #[test]
    fn tokenizing_out_of_order() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut a = arena.allocate::<u8>(&state)?;
        *a = 0xAA;
        let mut b = arena.allocate::<u8>(&state)?;
        *b = 0xBB;

        make_guard!(compact);
        let tok_a = arena.tokenize(&compact, a);
        let tok_b = arena.tokenize(&compact, b);
        let state = arena.compact(&compact, state);
        // redeem in the opposite order
        let b = tok_b(&arena);
        let a = tok_a(&arena);
        arena.finish(compact, state);
        assert_eq!(*a, 0xAA);
        assert_eq!(*b, 0xBB);
        Ok(())
    }

    #[test]
    fn tokenizing_out_of_order_with_compacting() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut first = arena.allocate::<u8>(&state)?;
        let mut a = arena.allocate::<u8>(&state)?;
        *a = 0xAA;
        let mut b = arena.allocate::<u8>(&state)?;
        *b = 0xBB;

        make_guard!(compact);
        let tok_a = arena.tokenize(&compact, a);
        let tok_b = arena.tokenize(&compact, b);
        let state = arena.compact(&compact, state);
        // redeem in the opposite order
        let b = tok_b(&arena);
        let a = tok_a(&arena);
        arena.finish(compact, state);
        assert_eq!(*a, 0xAA);
        assert_eq!(*b, 0xBB);
        Ok(())
    }

    #[test]
    fn tokenizing_small_gap() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut first = arena.allocate::<u8>(&state)?;
        *first = 0xCC;
        let mut a = arena.allocate::<[u8;2]>(&state)?;
        *a = [0x1,0x2];
        let mut b = arena.allocate::<[u8;2]>(&state)?;
        *b = [0x3,0x4];

        make_guard!(compact);
        let tok_a = arena.tokenize(&compact, a);
        let tok_b = arena.tokenize(&compact, b);
        let state = arena.compact(&compact, state);
        // this doesn't actually compact correctly: if you don't redeem
        // tokens in ascending ordering, than they can't all reclaimed free space.
        // this wouldn't be a problem if Arena was a fixed-size type arena, so. idk
        let b = tok_b(&arena);
        let a = tok_a(&arena);
        arena.finish(compact, state);
        assert_eq!(*a, [0x1,0x2]);
        assert_eq!(*b, [0x3,0x4]);
        Ok(())
    }

    #[test]
    fn tokenizing_leak_token() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut a = arena.allocate::<[u8;2]>(&state)?;
        *a = [0x1,0x2];

        make_guard!(compact);
        let tok_a = arena.tokenize(&compact, a);
        let state = arena.compact(&compact, state);
        let a = Box::leak(Box::new(tok_a));
        arena.finish(compact, state);
        //assert_eq!(*(a(&arena)), [0x1,0x2]);
        Ok(())
    }

    #[test]
    fn arena_alignment() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut first = arena.allocate::<u8>(&state)?;
        *first = 0xCC;
        let mut a = arena.allocate::<u32>(&state)?;
        let mut b = arena.allocate::<u8>(&state)?;
        let mut c = arena.allocate::<u16>(&state)?;

        Ok(())
    }
}
