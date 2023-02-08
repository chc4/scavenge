#![feature(unboxed_closures, fn_traits)]
use generativity::{make_guard, Guard, Id};
use stack_tokens::{stack_token, RefCellLocalKeyExt};
use core::sync::atomic::{Ordering, AtomicUsize};
use std::alloc::Layout;
use core::cell::RefCell;
use core::marker::PhantomData;

struct Arena<'life, const SIZE: usize> {
    current: RefCell<Layout>,
    bytes: Box<[u8; SIZE]>,
    _token: PhantomData<Id<'life>>,
}

struct Item<'life, T> {
    data: T,
    _phantom: PhantomData<Id<'life>>,
}

struct Forward<'life, T> {
    ptr: *mut T,
    _phantom: PhantomData<Id<'life>>,
}
impl<'a, 'life, T: 'a, const SIZE: usize> FnOnce<(&'a Arena<'life, SIZE>,)> for Forward<'life, T> {
    type Output = Item<'life, &'a mut T>;

    extern "rust-call" fn call_once(self, arena: (&'a Arena<'life, SIZE>,)) -> Self::Output {
        let data = unsafe { core::mem::transmute(self.ptr) };
        Item { data, _phantom: PhantomData }
    }
}

impl<'a, 'life, T> Item<'life, &'a mut T> {
    /// Tokenize an allocated item into a forwarding pointer, which removes the
    /// tie to the Arena lifetime until redeemed.
    pub fn tokenize(self) -> Forward<'life, T> {
        Forward { ptr: self.data as *mut T, _phantom: PhantomData }
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
        Arena {
            current: RefCell::new(Layout::from_size_align(0, 1).unwrap()),
            bytes: Box::new([0; SIZE]),
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
    fn allocate_items() -> Result<(), Box<dyn std::error::Error>> {
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
}
