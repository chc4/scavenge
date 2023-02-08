#![feature(unboxed_closures, fn_traits, inline_const, ptr_sub_ptr, pin_macro)]
use generativity::{make_guard, Guard, Id};
use stack_tokens::{stack_token, RefCellLocalKeyExt};
use core::sync::atomic::{Ordering, AtomicUsize};
use std::alloc::Layout;
use core::cell::RefCell;
use core::marker::PhantomData;
use core::pin::{Pin, pin};
use std::ops::{Deref, DerefMut};

struct Arena<'life, const SIZE: usize> {
    current: RefCell<Layout>,
    bytes: Box<[u8; SIZE]>,
    // The actual unmovable forwarding pointer references
    forwarding: RefCell<Vec<*const ()>>,
    _token: PhantomData<Id<'life>>,
}

#[repr(C)]
struct WithHeader<T> {
    forwarding: *const *const (),
    item: T,
}

impl<T: Default> Default for WithHeader<T> {
    fn default() -> Self {
        Self { forwarding: 0 as *const *const (), item: Default::default() }
    }
}

struct Item<'life, T> {
    data: T,
    _phantom: PhantomData<Id<'life>>,
}

struct Token<'life, T, COMPACT> {
    ptr: *mut T,
    _phantom: PhantomData<Id<'life>>,
    _compact: PhantomData<COMPACT>,
}
impl<'a, 'life, 'compact, T: 'a + Copy, const SIZE: usize> FnOnce<(&'a Arena<'life, SIZE>,)> for Token<'life, T, &Guard<'compact>> {
    type Output = Item<'life, &'a mut T>;

    extern "rust-call" fn call_once(self, arena: (&'a Arena<'life, SIZE>,)) -> Self::Output {
        // This token is being redeemed, meaning it's an alive object; we move the
        // object to the end of the compacted region.
        //
        // SAFETY: The token is branded with a 'life ID, which must match the Arena
        // that it is paired with in order to call this and redeem the token.
        // Likewise, the token has a borrow for a Guard token for the compaction step,
        // and thus can't be redeemed after compacting has finished.
        let data: &mut T = unsafe { core::mem::transmute(self.ptr) };
        // this unwrap shouldn't ever fail, since our memory usage usually shouldn't (can't?)
        // *increase* during a compaction
        let new_loc = arena.0.reserve::<T>().unwrap();
        unsafe {
            core::ptr::write(new_loc, *data);
        }
        Item { data, _phantom: PhantomData }
    }
}

struct Forward<'life, T> {
    ptr: *mut T,
    _life: PhantomData<Id<'life>>,
    _item: PhantomData<*mut T>,
}

impl<'life, T> Forward<'life, T> {
    pub fn with<'a, const SIZE: usize>(&'a self, arena: &'a Arena<'life, SIZE>) -> ForwardTemp<'life, 'a, T, SIZE> {
        ForwardTemp { forwarding: self, arena }
    }
}

struct ForwardTemp<'life, 'a, T, const SIZE: usize> {
    forwarding: &'a Forward<'life, T>,
    arena: &'a Arena<'life, SIZE>,
}

impl<'life, 'a, T: 'life, const SIZE: usize> core::ops::Deref for ForwardTemp<'life, 'a, T, SIZE> {
    type Target = T;
    fn deref(&self) -> &'a Self::Target {
        unsafe {
            core::mem::transmute(self.forwarding.ptr)
        }
    }
}
impl<'life, 'a, T: 'life, const SIZE: usize> core::ops::DerefMut for ForwardTemp<'life, 'a, T, SIZE> {
    fn deref_mut(&mut self) -> &mut <Self as core::ops::Deref>::Target {
        unsafe {
            core::mem::transmute(self.forwarding.ptr)
        }
    }
}

impl<'a, 'life, 'compact, T> Item<'life, &'a mut T> {
    /// Tokenize an allocated item into a forwarding pointer, which removes the
    /// tie to the Arena lifetime until redeemed again.
    ///
    /// The tokens use a "lifetime two phase commit" mechanism to make sure that
    /// 1) there are no outstanding direct Item references when compacting starts, only Tokens
    /// 2) there are no outstanding Tokens when compacting ends, only Items
    ///
    /// let a = arena.create(0xaabb); // Item<'life, 'a>
    /// make_guard!(compacting);
    /// let tok_a = a.tokenize(&compacting); // Token<'life, 'compacting>
    /// // errors if any outstanding 'a lifetimes
    /// arena.start_compacting();
    /// let new_a = tok_a(&area); // Item<'life, 'b>
    /// // errors if any outstanding 'compacting lifetimes
    /// arena.finish_compacting(&mut compacting);
    pub fn tokenize<'fresh>(self, guard: &'fresh Guard<'compact>) -> Token<'life, T, &'fresh Guard<'compact>> {
        Token { ptr: self.data as *mut T, _phantom: PhantomData, _compact: PhantomData }
    }
}

//impl<'a, 'life, T: 'life> Item<'life, &'a mut WithHeader<T>> {
//    /// Convert an Item into a reference that does a forwarding pointer chase on
//    /// every deref, and instead restricts the deref borrow.
//    ///
//    /// The forwarding pointer wrapper in wrapped in a Pin<T>, because the arena
//    /// must keep track of the pointer to update when compacting; likewise, the
//    /// item must be a WithHeader<T> so that it is has space allocated for that
//    /// bookkeeping.
//    ///
//    /// This necessitates that future uses of the forwarding pointer require a
//    /// double-deref in order to reach the item, and that they cause an extra
//    /// heap allocation.
//    pub fn forwarding<const SIZE: usize>(mut self, arena: &'a Arena<'life, SIZE>) -> Pin<Box<Forward<'life, T>>> {
//        let ptr = &mut self.data.item as *mut T as *mut u8;
//
//        // create the pinned pointer to the forwarding pointer
//        let pin = Box::pin(Forward {
//            ptr: ptr as *mut _,
//            _item: PhantomData,
//            _life: PhantomData,
//        });
//
//        // calculate the address of where the forwarding pointer will be and set
//        // it in the item's header
//        self.deref_mut().forwarding = pin.deref().ptr as *const _;
//        pin
//    }
//}

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
        let forwarding = vec![];
        (Arena {
            current: RefCell::new(Layout::from_size_align(0, 1).unwrap()),
            bytes: Box::new([0; SIZE]),
            forwarding: RefCell::new(forwarding),
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

    fn compact<'compact>(&mut self, guard: &Guard<'compact>, compact: Allocating<'life>) -> Compacting<'compact, 'life> {
        // reset the bump pointer; all the objects that are alive will be redeemed
        // via tokens, which will re-allocate the object.
        //
        // we can't have Arena itself be a linear state transitioning type, and have
        // to use a seperate parameter. this is because although we could move self
        // in this, we can't move it in the corrosponding finish() call, due to
        // token redeeming having to re-borrow the Item with the new lifetime after
        // compacting.
        Compacting(compact.0, PhantomData)
    }

    fn finish<'compact>(&self, guard: Guard<'compact>, allocatable: Compacting<'compact, 'life>) -> Allocating<'life> {
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
        let a_tok = a.tokenize(&compact);
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
        let mut tok_v = v.drain(..).map(|i| i.tokenize(&compact) ).collect::<Vec<_>>();
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
        let mut tok_v = v.drain(5..10).map(|i| i.tokenize(&compact) ).collect::<Vec<_>>();
        let state = arena.compact(&compact, state);
        let v = tok_v.drain(..).map(|t| t(&arena) ).collect::<Vec<_>>();
        arena.finish(compact, state);
        for i in 0..5 {
            assert_eq!(*v[i], 5+i as u8);
        }
        Ok(())
    }

    //#[test]
    //fn forwarding() -> Result<(), Box<dyn std::error::Error>> {
    //    make_guard!(guard);
    //    let mut arena: Arena<'_, 1024> = Arena::new(guard);
    //    let mut a = arena.allocate::<WithHeader<u8>>()?.forwarding(&arena);
    //    let mut forwarding_a = a.with(&arena);
    //    assert_eq!(*forwarding_a, 0);
    //    *forwarding_a = 1;
    //    assert_eq!(*forwarding_a, 1);
    //    make_guard!(compact);
    //    let arena = arena.compact();
    //    arena.finish(compact);
    //    assert_eq!(*a.with(&arena), 1);
    //    Ok(())
    //}

}
