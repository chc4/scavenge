/* Shared<T> allocates a Header<T>, which is (*const (), T) (forwarding pointer)
 * On tokenize, if item.forwarding == 0, then tab = reserve::<Result(*const (), *const ()> and point the
 * forwarding pointer there, SharedToken is the tab. else just return tab from the
 * forwarding pointer. The tab is always reserved in the slackspace of the arena.
 * (keep the start of the sidetable in arena, mark the entire region used in compact
 * just to be sure we can't overwrite it even with pathalogical padding causing the
 * arena to grow after compaction...?)
 *
 * On redeem, if tab.is_err(), and thus it's pointing
 * at the unmoved object, then we need to move; don't mark the item free, find min(first hole,
 * the same address), move object, set *tab to Ok(new_address), then can mark old object free.
 * this can't overwrite other objects because they have entries in the bitmap, but also
 * doesn't keep objects in the bitmap even after they're moved because the forwarding
 * pointer is stored in memory that should never be used when compacting.
 *
 * only objects that have shared references need a forwarding pointer, and items
 * being redeemed in the wrong order is less of a problem since objects can just
 * move onto themselves.
 * does mean that worst-case memory usage is two words per object, though.
 * (two words and a byte...? can tab be pointer tagged, or does it need byte+pointer)
 * also we could just straight fall a compaction i think, if we do tokenizing and
 * then run out of space for the sidetable we're gonna have a bad time since we
 * don't start compacting until we tokenize everything, at which point we already
 * ran out of space. you could do bitmap hole finding (from high to low...?) but
 * then also those sidetable entries will be marked used when compacting and you'll
 * make things worse actually. if your bitmap is also full than you *actually* ran
 * out of space too, and then compacting wouldn't find much. you could just like
 * have tab = TBI tagged item and just not move it at all so that you don't fail,
 * but also don't make any room.
 *
 * we're already limiting the arena size to u32 in a few places actually, tab
 * entries can just be u32 offsets
 *
 * there's probably some design you could do here where the forwarding pointer
 * points to arena.forwarded which you bump as you find items for forwarding and you
 * keep it reserved through arena.compact, and you only need to allocate
 * the side-table if you redeem tokens out of order
 *
 * this never actually compacts objects spatially does it. if you have an object
 * at 0 it will never move from 0, in contrast with lisps or java where objects
 * that are traversed in spatially adjacent datastructures are all moved together.
 *
 * [A][B][C][D]
 * [A ][  ][C][ ] | [t_A][t_C]
 * [nA][nC][ ][ ] | [nA][nC]
 *
 */
use crate::{Item, Guard, Id, Arena, Allocating, Token, Tokenize, Bool, True};
use core::marker::PhantomData;
use core::pin::{pin, Pin};
use core::ptr::addr_of_mut;

#[repr(C)]
struct WithHeader<T> {
    forwarding: *const (),
    item: T,
}
impl<T> WithHeader<T> {
    fn new(t: T) -> Self {
        WithHeader { forwarding: core::ptr::null(), item: t }
    }
}

impl<T: Default> Default for WithHeader<T> {
    fn default() -> Self {
        WithHeader::new(Default::default())
    }
}

impl<'life> crate::Arena<'life> {
    /// Turn an Item reference into a Forward reference.
    fn forward<'a, 'borrow, 'compact, 'reborrow, T, U>(&'a self, allocating: &Allocating<'life>, item: Item<'life, 'a, WithHeader<T>>)
        -> Pin<Box<Forward<'life, T>>>
        where
            T: Tokenize<'life, 'borrow, 'compact, 'reborrow, Untokenized = U>,
            T::Untokenized: Tokenize<'life, 'borrow, 'compact, 'reborrow>,
            'compact: 'borrow,
            'life: 'reborrow,
            'life: 'compact,
            'life: 'borrow,
            Bool<{ Arena::same_repr::<T, T::Tokenized>() }>: True,
            Bool<{ Arena::same_repr::<T, U>() }>: True,
            Bool<{ Arena::same_repr::<T, U::Tokenized>() }>: True,
    {
        // we have to add the forward pointer to the autoforwarding set in the
        // arena; because we won't be calling tokenize() on the item, it has to
        // be registered as alive by the arena itself on compaction.
        let ptr = (item.data as *mut WithHeader<T>).clone();
        fn tokenize<'life, 'borrow, 'compact, 'reborrow, 'compact2, T, U>(arena: &Arena<'life>, guard: &Guard<'compact2>, me: *mut ())
        where
            T: Tokenize<'life, 'borrow, 'compact, 'reborrow, Untokenized = U>,
            T::Untokenized: Tokenize<'life, 'borrow, 'compact, 'reborrow>,
            'compact: 'borrow,
            'life: 'reborrow,
            'life: 'compact,
            'life: 'borrow,
            Bool<{ Arena::same_repr::<T, T::Tokenized>() }>: True,
            Bool<{ Arena::same_repr::<T, U>() }>: True,
            Bool<{ Arena::same_repr::<T, U::Tokenized>() }>: True,
        {
            let ptr = unsafe { let p = (me as *mut WithHeader<T>); addr_of_mut!((*p).item).as_mut().unwrap() };
            arena.tokenize(guard, Item { data: ptr, _phantom: PhantomData });
        }
        self.autoforward.borrow_mut().insert(ptr as *mut (),
            tokenize::<'life, 'borrow, 'compact, 'reborrow, T, U> as for<'compact2> fn(&Arena<'life>, &Guard<'compact2>, *mut ())->());
        let fwd = Box::pin(Forward { ptr: core::ptr::addr_of_mut!(item.data.item), _life: PhantomData, _item: PhantomData });
        unsafe {
            // write the pinned forwarding pointer we need to update on compaction
            // to the item's header
            (*ptr).forwarding = Pin::get_ref(fwd.as_ref()) as *const _ as *const ();
        }
        fwd
    }
}

fn to_header<'life, 'before, 'borrow, 'compact, 'reborrow, T, U>
    (arena: &Arena<'life>, guard: &'borrow Guard<'compact>, hdr: WithHeader<T>)
    -> WithHeader<T::Tokenized>
    where
        // we need to return a seperate type U as the Token result, so that
        // we can re-parameterize the type over the 'reborrow lifetime
        T: Tokenize<'life, 'borrow, 'compact, 'reborrow,
            Tokenized = Token<'life, 'borrow, 'compact, 'reborrow, U>,
            Untokenized = U>,
        T::Untokenized: Tokenize<'life, 'borrow, 'compact, 'reborrow>,
        U: 'reborrow,
        // make sure the tokenized and result fit in the same allocated space
        // as the input, to guard against incorrect Tokenized impls
        Bool<{ Arena::same_repr::<T, T::Tokenized>() }>: True,
        Bool<{ Arena::same_repr::<T, U>() }>: True,
        Bool<{ Arena::same_repr::<T, U::Tokenized>() }>: True,
        'compact: 'borrow,
        'life: 'reborrow,
        'life: 'compact,
        'life: 'borrow,

        'borrow: 'reborrow,
{
    let item = Item { data: &mut hdr.item, _phantom: PhantomData };
    WithHeader::new(arena.tokenize(guard, item))
}

fn from_header<'life, 'borrow, 'compact, 'reborrow>
    (arena: &'reborrow Arena<'life>, num: Num)
    -> Num
{
    num
}

impl<'life, 'borrow, 'compact, 'reborrow, T, U> Tokenize<'life, 'borrow, 'compact, 'reborrow>
    for WithHeader<T>
    where
        // we need to return a seperate type U as the Token result, so that
        // we can re-parameterize the type over the 'reborrow lifetime
        T: Tokenize<'life, 'borrow, 'compact, 'reborrow, Untokenized = U>,
        T::Untokenized: Tokenize<'life, 'borrow, 'compact, 'reborrow>,
        // make sure the tokenized and result fit in the same allocated space
        // as the input, to guard against incorrect Tokenized impls
        Bool<{ Arena::same_repr::<T, T::Tokenized>() }>: True,
        Bool<{ Arena::same_repr::<T, U>() }>: True,
        Bool<{ Arena::same_repr::<T, U::Tokenized>() }>: True,
        'compact: 'borrow,
        'life: 'reborrow,
        'life: 'compact,
        'life: 'borrow,
{
    type Tokenized = WithHeader<T::Tokenized>;
    type Untokenized = WithHeader<T::Untokenized>;
    const TO: fn(&Arena<'life>, &'borrow Guard<'compact>, Self)
        -> Self::Tokenized = to_header;
    const FROM: fn(&'reborrow Arena<'life>, Self::Tokenized)
        -> Self::Untokenized = from_num;
}

struct Forward<'life, T> {
    ptr: *mut T,
    _life: PhantomData<Id<'life>>,
    _item: PhantomData<*mut T>,
}

impl<'life, T> Forward<'life, T> {
    pub fn with<'a>(&'a self, arena: &'a Arena<'life>) -> ForwardTemp<'life, 'a, T> {
        ForwardTemp { forwarding: self, arena }
    }
}

struct ForwardTemp<'life, 'a, T> {
    forwarding: &'a Forward<'life, T>,
    arena: &'a Arena<'life>,
}

impl<'life, 'a, T: 'life> core::ops::Deref for ForwardTemp<'life, 'a, T> {
    type Target = T;
    fn deref(&self) -> &'a Self::Target {
        unsafe {
            core::mem::transmute(self.forwarding.ptr)
        }
    }
}
impl<'life, 'a, T: 'life> core::ops::DerefMut for ForwardTemp<'life, 'a, T> {
    fn deref_mut(&mut self) -> &mut <Self as core::ops::Deref>::Target {
        unsafe {
            core::mem::transmute(self.forwarding.ptr)
        }
    }
}

mod test {
    use super::*;
    use generativity::{Guard, make_guard};

    fn to_num<'life, 'borrow, 'compact>
        (arena: &Arena<'life>, guard: &'borrow Guard<'compact>, num: Num)
        -> Num
    {
        num
    }

    fn from_num<'life, 'borrow, 'compact, 'reborrow>
        (arena: &'reborrow Arena<'life>, num: Num)
        -> Num
    {
        num
    }

    struct Num(usize);
    impl<'life, 'borrow, 'compact, 'reborrow> Tokenize<'life, 'borrow, 'compact, 'reborrow>
        for Num
        where 'compact: 'borrow,
              'life: 'reborrow,
              'life: 'borrow,
              'life: 'compact,
    {
        type Tokenized = Num;
        type Untokenized = Num;
        const TO: fn(&Arena<'life>, &'borrow Guard<'compact>, Self)
            -> Self::Tokenized = to_num;
        const FROM: fn(&'reborrow Arena<'life>, Self::Tokenized)
            -> Self::Untokenized = from_num;
    }

    #[test]
    fn forward() -> Result<(), Box<dyn std::error::Error>> {
        make_guard!(guard);
        let (mut arena, state): (Arena<'_>, _) = Arena::new(guard, 1024);
        let a = arena.create(&state, || WithHeader::new(Num(1)))?;
        let garbage = arena.create(&state, || Num(0))?;
        let a = arena.forward(&state, a);
        make_guard!(compact);
        let garbage_tok = arena.tokenize(&compact, garbage);
        let state = arena.compact(&compact, state);
        let garbage = garbage_tok(&arena);
        let state = arena.finish(compact, state);
        assert_eq!(a.with(&arena).0, 1);
        Ok(())

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
