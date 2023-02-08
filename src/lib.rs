#![feature(unboxed_closures, fn_traits, inline_const, ptr_sub_ptr, pin_macro)]
use generativity::{make_guard, Guard, Id};
use roaring::{RoaringBitmap};
use core::sync::atomic::{Ordering, AtomicUsize};
use std::alloc::Layout;
use core::cell::RefCell;
use core::marker::PhantomData;
use core::pin::{Pin, pin};
use std::ops::{Deref, DerefMut};

struct Arena<'life, const SIZE: usize> {
    current: RefCell<Layout>,
    bytes: Box<[u8; SIZE]>,
    bitmap: RefCell<RoaringBitmap>,
    _token: PhantomData<Id<'life>>,
}

#[repr(transparent)]
struct Item<'life, T> {
    data: T,
    _phantom: PhantomData<Id<'life>>,
}

#[repr(transparent)]
struct Token<'life, T, COMPACT> {
    ptr: *mut T,
    _phantom: PhantomData<Id<'life>>,
    _compact: PhantomData<COMPACT>,
}
impl<'a, 'life, 'compact, T: 'a + Copy, const SIZE: usize> FnOnce<(&'a Arena<'life, SIZE>,)> for Token<'life, T, &Guard<'compact>> {
    type Output = Item<'life, &'a mut T>;

    extern "rust-call" fn call_once(self, arena: (&'a Arena<'life, SIZE>,)) -> Self::Output {
        // XXX: THIS IS WRONG. tokens can be redeemed out of order, which means
        // that you can have a token at 0 and then redeem 1, overwriting the first
        // live object. we need to mark a bitmap on tokenizing for the bounds of
        // memory, and then skip marked tokenized memory when reserving; then, when
        // we redeem the tokens, if self.ptr is less than arena.current, we don't
        // have to move it at all because it's already inside the compacted region.

        // This token is being redeemed, meaning it's an alive object; we move the
        // object to the end of the compacted region.
        //
        // SAFETY: The token is branded with a 'life ID, which must match the Arena
        // that it is paired with in order to call this and redeem the token.
        // Likewise, the token has a borrow for a Guard token for the compaction step,
        // and thus can't be redeemed after compacting has finished.
        let mut data: &mut T = unsafe { core::mem::transmute(self.ptr) };
        // only reallocate if the item isn't already in the compacted region
        if data as *mut _ as usize > arena.0.bytes.as_ptr() as usize + arena.0.current.borrow().size() + 1 {
            println!("moving {:?}", self.ptr);
            // this unwrap shouldn't ever fail, since our memory usage usually shouldn't (can't?)
            // *increase* during a compaction
            let new_loc = loop {
                let start: u32 = arena.0.current.borrow().size().try_into().unwrap();
                let new_loc = arena.0.reserve::<T>().unwrap();
                let end: u32 = arena.0.current.borrow().size().try_into().unwrap();
                // the bitmap is the available *holes*, not alive values, thanks
                // to Arena::compact(). if we're able to clear the entire region,
                // then it was free and we can allocate into it.
                if arena.0.bitmap.borrow_mut().remove_range(start..end) == (start..end).len() as u64 {
                    break new_loc
                }
                // else jump to the next free space
                *arena.0.current.borrow_mut() = Layout::from_size_align(
                    arena.0.bitmap.borrow().min().unwrap() as usize, 1).unwrap();
            };
            unsafe {
                core::ptr::write(new_loc, *data);
                data = core::mem::transmute(new_loc);
            }
        }
        Item { data, _phantom: PhantomData }
    }
}

impl<'a, 'life, 'compact, T> Item<'life, &'a mut T> {
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
    fn new<'a>(id: Guard<'a>) -> (Arena<'a, SIZE>, Allocating<'a>) {
        (Arena {
            current: RefCell::new(Layout::from_size_align(0, 1).unwrap()),
            bytes: Box::new([0; SIZE]),
            bitmap: RefCell::new(RoaringBitmap::new()),
            _token: Default::default(),
        }, Allocating(id.into()))
    }

    fn reserve<'a, T>(&'a self) -> Result<*mut T, Box<dyn std::error::Error>> {
        let start = self.current.borrow().clone();
        let (end, next) = start.extend(Layout::new::<T>())?;
        let end = end.pad_to_align();
        *self.current.borrow_mut() = end;
        println!("item at {}, next at {}", next, end.size());
        unsafe {
            Ok((self.bytes.as_ptr() as *mut u8).add(next) as *mut T)
        }
    }

    fn allocate<'a, T: Default>(&'a self) -> Result<Item<'life, &'a mut T>, Box<dyn std::error::Error>> {
        let item = self.reserve::<T>()?;
        unsafe {
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
        let ptr = (item.data as *mut T as usize) - self.bytes.as_ptr() as usize;
        let ptr_trunc: u32 = ptr.try_into().unwrap();
        println!("marking {}..={} as alive", ptr_trunc, ptr_trunc+(core::mem::size_of::<T>() as u32));
        self.bitmap.borrow_mut().insert_range(ptr_trunc..ptr_trunc+(core::mem::size_of::<T>() as u32));
        Token { ptr: item.data as *mut T, _phantom: PhantomData, _compact: PhantomData }
    }


    fn compact<'compact>(&mut self, guard: &Guard<'compact>, compact: Allocating<'life>) -> Compacting<'compact, 'life> {
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
        *self.current.borrow_mut() = Layout::from_size_align(dif.min().unwrap_or(0) as usize, 1).unwrap();

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

    fn finish<'compact>(&self, guard: Guard<'compact>, allocatable: Compacting<'compact, 'life>) -> Allocating<'life> {
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

struct Compacting<'compact, 'life>(Id<'life>, PhantomData<Id<'compact>>);
struct Allocating<'life>(Id<'life>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizing() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut a = arena.allocate::<u8>()?;
        let mut a2 = arena.allocate::<[u32;12]>()?;
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
            let mut item = arena.allocate::<u8>()?;
            *item = i;
            v.push(item);
        }
        make_guard!(compact);
        let mut tok_v = v.drain(..).map(|i| arena.tokenize(&compact, i) ).collect::<Vec<_>>();
        let state = arena.compact(&compact, state);
        let v = tok_v.drain(..).map(|t| t(&arena) ).collect::<Vec<_>>();
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
            let mut item = arena.allocate::<u8>()?;
            *item = i;
            v.push(item);
        }
        make_guard!(compact);
        let mut tok_v = v.drain(5..10).map(|i| arena.tokenize(&compact, i) ).collect::<Vec<_>>();
        let state = arena.compact(&compact, state);
        let v = tok_v.drain(..).map(|t| t(&arena) ).collect::<Vec<_>>();
        arena.finish(compact, state);
        for i in 0..5 {
            assert_eq!(*v[i], 5+i as u8);
        }
        Ok(())
    }

    #[test]
    fn tokenizing_vec_out_of_order() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut a = arena.allocate::<u8>()?;
        *a = 0xAA;
        let mut b = arena.allocate::<u8>()?;
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
    fn tokenizing_vec_out_of_order_with_compacting() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_, 1024>, _) = Arena::new(guard);
        let mut first = arena.allocate::<u8>()?;
        let mut a = arena.allocate::<u8>()?;
        *a = 0xAA;
        let mut b = arena.allocate::<u8>()?;
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

}
