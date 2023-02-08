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
