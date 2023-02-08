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

struct Token<'life, T> {
    ptr: *mut T,
    _phantom: PhantomData<Id<'life>>,
}
impl<'a, 'life, T: 'a, const SIZE: usize> FnOnce<(&'a Arena<'life, SIZE>,)> for Token<'life, T> {
    type Output = Item<'life, &'a mut T>;

    extern "rust-call" fn call_once(self, arena: (&'a Arena<'life, SIZE>,)) -> Self::Output {
        let data = unsafe { core::mem::transmute(self.ptr) };
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

impl<'a, 'life, T> Item<'life, &'a mut T> {
    /// Tokenize an allocated item into a forwarding pointer, which removes the
    /// tie to the Arena lifetime until redeemed again.
    ///
    /// The tokens use a "lifetime two phase commit" mechanism to make sure that
    /// all tokenized items forwarding pointers are resolved at once, which
    /// allows the compaction to happen all at once without needing to reserve a
    /// forwarding pointer side table.
    pub fn tokenize(self) -> Token<'life, T> {
        Token { ptr: self.data as *mut T, _phantom: PhantomData }
    }
}

impl<'a, 'life, T: 'life> Item<'life, &'a mut WithHeader<T>> {
    /// Convert an Item into a reference that does a forwarding pointer chase on
    /// every deref, and instead restricts the deref borrow.
    ///
    /// The forwarding pointer wrapper in wrapped in a Pin<T>, because the arena
    /// must keep track of the pointer to update when compacting; likewise, the
    /// item must be a WithHeader<T> so that it is has space allocated from that
    /// tracking pointer.
    pub fn forwarding<const SIZE: usize>(mut self, arena: &'a Arena<'life, SIZE>) -> Pin<Box<Forward<'life, T>>> {
        let ptr = &mut self.data.item as *mut T as *mut u8;

        // create the pinned pointer to the forwarding pointer
        let pin = Box::pin(Forward {
            ptr: ptr as *mut _,
            _item: PhantomData,
            _life: PhantomData,
        });

        // calculate the address of where the forwarding pointer will be and set
        // it in the item's header
        self.deref_mut().forwarding = pin.deref().ptr as *const _;
        pin
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
    fn new<'a>(id: Guard<'a>) -> Arena<'a, SIZE> {
        let forwarding = vec![];
        Arena {
            current: RefCell::new(Layout::from_size_align(0, 1).unwrap()),
            bytes: Box::new([0; SIZE]),
            forwarding: RefCell::new(forwarding),
            _token: Default::default(),
        }
    }

    fn allocate<'a, T: Default>(&'a self) -> Result<Item<'life, &'a mut T>, Box<dyn std::error::Error>> {
        let start = self.current.borrow().clone();
        let (end, next) = start.extend(Layout::new::<T>())?;
        let end = end.pad_to_align();
        *self.current.borrow_mut() = end;
        println!("item at {}, next at {}", next, end.size());
        unsafe {
            let item = core::mem::transmute::<*mut T, &'a mut T>(
                (self.bytes.as_ptr() as *const u8).add(next) as *mut T);
            *item = Default::default();
            Ok(Item { data: item, _phantom: PhantomData })
        }
    }

    fn compact(&mut self) {
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizing() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let mut arena: Arena<'_, 1024> = Arena::new(guard);
        let mut a = arena.allocate::<u8>()?;
        let mut a2 = arena.allocate::<[u32;12]>()?;
        assert_eq!(*a, 0);
        *a = 1;
        assert_eq!(a2[0], 0);
        let a_tok = a.tokenize();
        arena.compact();
        let a = a_tok(&arena);
        assert_eq!(*a, 1);
        Ok(())
    }

    #[test]
    fn forwarding() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let mut arena: Arena<'_, 1024> = Arena::new(guard);
        let mut a = arena.allocate::<WithHeader<u8>>()?.forwarding(&arena);
        let mut forwarding_a = a.with(&arena);
        assert_eq!(*forwarding_a, 0);
        *forwarding_a = 1;
        assert_eq!(*forwarding_a, 1);
        arena.compact();
        assert_eq!(*a.with(&arena), 1);
        Ok(())
    }

}
