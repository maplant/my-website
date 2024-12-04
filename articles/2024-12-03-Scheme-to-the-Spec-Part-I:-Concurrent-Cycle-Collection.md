# Scheme to the Spec (In Rust)

**This is a pre-print article**

Scheme to the Spec is a series on the more complex, often overlooked (at least didactically) aspects of programming language 
implementation.

In this series we will dive deep into my work-in-progress implementation of R6RS scheme, [scheme-rs](https://www.github.com/maplant/scheme-rs),
an implementation designed to integrate seamlessly with the async-rust ecosystem.

Our first article discusses how to implement Garbage-Collected smart pointers that we can use both within the interpreter
and the interfacing Rust code. In later articles we will discuss topics such as tail call optimizations, implementing continuations, 
and syntax transformers.

## Part I: Garbage Collected Smart Pointers with Concurrent Cycle Collection

Ten years ago I wrote an article on how to implement a conservative garbage collector for C, and I got a lot of constructive 
criticism regarding the actual usefulness of the code in that article. It only makes sense to me that on the tenth-year anniversary 
of writing that article I should try to fix my error by writing a garbage collector that is precise, does not make assumptions, and 
actually works (hopefully). 

## The `Gc<T>` Smart Pointer 

Before we can start writing code, we have to figure out what goals we want to accomplish. More specifically, what do we want our 
`Gc<T>` type to _do_? How does it _behave_? What kind of _data can it contain_?

I want the `Gc<T>` type to work as follows:

- The API for the `Gc<T>` should behave similarly to a `Arc<tokio::sync::RwLock<T>>`; that is, it should support interior mutability
  through a read/write lock and it should be clonable and sendable across threads.
- `T` is allowed to be _any_ data type that satisfies `'static + Send + Sync`. This includes `Arc`. 
- When `Gc<T>` no longer has any references to it reachable from the stack, then we should clean it up properly, _including any cycles_. 

Now that we have an idea as to how our smart pointer should behave, we can begin our implementation. Let's get started.

### Scaffolding and allocation 

Our Gc type will be composed of three separate types:

- `Gc<T>`: User-facing type, contains a pointer to our heap allocated memory.
- `GcInner<T>`: The inner data type, which contains `T` and any other information we may need to keep track of the data.
- `GcHeader`: The "any other information we may need to keep track of".

To start out, we have no extra information we need to store in `GcHeader`, so we can knock that one out quickly:

```rust
struct GcHeader;
```

`GcInner<T>` is just the header and the data, so that is equally simple:

```rust
struct GcInner<T> {
    header: GcHeader,
    data: T,
}
```

Now we can put it together and build our `Gc<T>` type. Before we get to allocating, we need to make sure that our 
data structure expresses the following things to the compiler:

##### Variance:
If we have two types `A` and `B` such that `A` is a subtype of `B`, then `Gc<A>` is a subtype of `Gc<B>`. 

Why do we need that? Well, actually, we don't _really_. We'd fine to specify no subtype relationship between two `Gc's`, 
as in we'd say the subtype relationship between two Gcs is _invariant_. Subtyping in Rust practically extends to 
specifying a liveness relationship; i.e., `&'static T` outlives `for<'a> &'a i32`, therefore 
`&'static T` is a subtype of `for<'a> &'a i32` as it can be placed any place the latter is accepted. 

Accepting references for non-stacic lifetimes would dramatically complicated our task, so we are simply not going to allow for them. So variance is _largely_ unimportant.

There is another place subtyping comes up. What about trait objects? Indeed, one can imagine the following example:

```rust
trait A { fn a(&self) -> i32; }

trait B: A { fn b(&self) -> i32; } 

impl A for i32 {
    fn a(&self) -> i32 { *self }
}

impl B for i32 {
    fn b(&self) -> i32 { *self * 2 }
}

let a = Box::new(1_i32) as Box<dyn A>;
let b = Box::new(1_i32) as Box<dyn B>;
```

It would make sense for `Box<dyn B>` to be a subtype of `Box<dyn A>`, as `A` is a super _trait_ of `B`. It
would make sense for this to apply to our `Gc` type as well. 

Unfortunately, this not supported by stable Rust at the current moment. It is possible that this could change
sometime in the future, perhaps soon, but not at the time of writing this article.

But lifetimes can _also_ appear in dyn traits! Basically, while we wait for our _actual_ use case to stabalize,
we are ensuring a covariant relationship in order to support subtype relationships of the following form:

`Gc<dyn FnMut(&'a T)>` is a subtype of `Gc<dyn FnMut(&'b T)>` for `'a: 'b`.

That is largely the reason why were are supporting subtyping. Closures. 

It's also not that difficult; we just need to use a [`NonNull`](https://doc.rust-lang.org/std/ptr/struct.NonNull.html) pointer wrapper instead of a raw `*mut T`, a type which _is_ invariant over `T`. This is what we'd do anyway, as we don't _want_ a `Gc<T>` to contain a pointer that _is_ null.

Now that we have that worked out, we can move onto the second thing we need to express:

#### Drop checking

As specified now as just a `NonNull<T>` and nothing else, the Rust compiler is forced to assume that any 
`Gc<T>` will be strictly out-lived by the underlying data. For our data type this not the case. Although a lot of Gcs 
represent references to the data, _some_ Gcs represent the entire lifetime - they represent data itself. Therefore,
dropping a `Gc` can potentially drop the underlying `T` value. The Rust compiler _needs_ to know about this in 
order to perform its drop check analysis, or else potentially unsound code can be constructed. 

In order to indicate this information, we can use a [`PhantomData`](https://doc.rust-lang.org/std/marker/struct.PhantomData.html), a neat wrapper type that indicates to the compiler that our data type should behave as if it has ownership over
a type `T`.

With these two data types we can put together our Gc<T>:

```rust
/// A Garbage-Collected smart pointer with interior mutability.
pub struct Gc<T> {
    ptr: NonNull<GcInner<T>>,
    marker: PhantomData<GcInner<T>>,
}

impl<T> Gc<T> {
    pub fn new(data: T) -> Gc<T> {
        Self {
            ptr: NonNull::from(Box::leak(Box::new(GcInner {
                header: GcHeader::default(),
                data,
            }))),
            marker: PhantomData,
        }
    }
}
``` 

The `new` code is rather straightforward but worth commenting on. The easiest way to allocate a pointer in 
Rust is to use the `Box::new` to allocates space and copy our data onto the heap, and then use the `Box::leak`
function to prevent the Box from freeing the data on its drop, thus leaving us with a dangling pointer that 
we can later deallocate manually. 

## Thread-Safe Interior Mutability 

Now that we have data, we need to provide a way to modify that data in a thread-safe manner. The very first
thing we have to tell our compiler that we intend to actually do this. By default, Rust assumes that for the
lifetime an immutable reference is held, the data referenced to will not be modified. We would like to opt 
out of that assumption. To that end, we must use the [`UnsafeCell`](https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html) wrapper. 

Obviously, we want to make sure that if someone is trying to read the data behind a a Gc that no one is trying
to modify it at the same time. You are never allowed to do unsound things in Rust, you are only allowed to tell
the compiler that it can't make assumptions about the references that actively exist in your program. 

Additionally, we are going to tell Rust that it should trust us to implement these rules in a thread-safe manner.
Our `GcInner` type now looks like the following:

```rust
pub struct GcInner<T: ?Sized> {
    header: UnsafeCell<GcHeader>,
    data: UnsafeCell<T>,
}

unsafe impl<T: ?Sized + Send + Sync> Send for GcInner<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for GcInner<T> {}

// (also necessary)
unsafe impl Send for GcHeader {}
unsafe impl Sync for GcHeader {}
```

Now we have to figure out how to allow for interior mutability. Well, we want this type to behave like 
a `RwLock<T>`, so how are those implemented?

### Semaphores

A Semaphore is a way to control access to a resource. Essentially, it is an array off N slots that each process
is allowed to claim ownership of. If all N slots are claimed, then processes must queue up and wait for
the processes with ownership to relinquish them.

```
Acquire(Semaphore) -> Option<Permit>
Release(Permit) 
```
(both of these operations are atomic)

The ordering of these slots is irrelevant. Processes are only concerned with having ownership of a slot or
not. If the Semaphore has only one slot, you can make this value behave exactly like a typical Mutex:

```
Lock(Semaphore) -> Permit:
    loop:
        match Acquire(Semaphore):
            Some(Permit) => return Permit,
```

If we were to change our semaphore have N > 1 slots, we need to add another atomic operation in order to properly
mimic our `Mutex`:

```
AcquireN(Semaphore, NumSlots) -> Option<Permit>`
```

Locking is pretty much the same, but instead of attempting to acquire one slot, we attempt to acquire all N.

But we don't have to always lock a variable. In fact, I think I see a way we can mimic Rust's safety rules 
here - we can have an unlimited number of immutable references OR one single mutable reference and never
both. The trick is to have our reads only acquire one slot and our writes acquire all N. 

To implement this, we're going to use [tokio's Semaphore](https://docs.rs/tokio/latest/tokio/sync/struct.Semaphore.html),
which interfaces well async rust. Let's add it to `GcHeader`:

```rust
struct GcHeader {
    semaphore: tokio::sync::Semaphore,
}
```

### Read/Write Guards 

We need some types to represent our acquired resource with. In Rust, these are done with Guards. Guards are 
structs that:

- hold a reference and a permit of use for a resource 
- are treated transparently as a reference to the resource (via Deref and/or DerefMut)
- release the permit when they are dropped
- has the lifetime of the wrapper type (so cannot be returned from a function with the wrapper as a local)

```rust 
pub struct GcReadGuard<'a, T: ?Sized> {
    _permit: tokio::sync::SemaphorePermit<'a>,
    data: *const T,
    marker: PhantomData<&'a T>,
}

impl<T: ?Sized> Deref for GcReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.data }
    }
}

impl<T: ?Sized> AsRef<T> for GcReadGuard<'_, T> {
    fn as_ref(&self) -> &T {
        self
    }
}

unsafe impl<T: Send> Send for GcReadGuard<'_, T> {}
unsafe impl<T: Sync> Sync for GcReadGuard<'_, T> {}

pub struct GcWriteGuard<'a, T> {
    _permit: tokio::sync::SemaphorePermit<'a>,
    data: *mut T,
    marker: PhantomData<&'a mut T>,
}

impl<T> Deref for GcWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.data }
    }
}

impl<T> DerefMut for GcWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.data }
    }
}

unsafe impl<T: Send> Send for GcWriteGuard<'_, T> {}
unsafe impl<T: Sync> Sync for GcWriteGuard<'_, T> {}
```

Again, you will notice that in both of these structs we use the same trick to ensure the desired variance 
(covariance) over each of the guards type parameters.

Since we use the tokio semaphore, we can create futures that await the acquisition of the permit:

```rust
impl<T> Gc<T> {
    /// Acquire a read lock for the object
    pub async fn read(&self) -> GcReadGuard<'_, T> {
        unsafe {
            let _permit = (*self.ptr.as_ref().header.get())
                .semaphore
                .acquire()
                .await
                .unwrap();
            let data = &*self.ptr.as_ref().data.get() as *const T;
            GcReadGuard {
                _permit,
                data,
                marker: PhantomData,
            }
        }
    }

    /// Acquire a write lock for the object
    pub async fn write(&self) -> GcWriteGuard<'_, T> {
        unsafe {
            let _permit = (*self.ptr.as_ref().header.get())
                .semaphore
                .acquire_many(MAX_READS)
                .await
                .unwrap();
            let data = &mut *self.ptr.as_ref().data.get() as *mut T;
            GcWriteGuard {
                _permit,
                data,
                marker: PhantomData,
            }
        }
    }
}
```

Cool! Now we have a heap allocated object with thread-safe interior mutability that lives forever.
Now we need to figure out a way to kill it, that's the fun part. 

## Garbage Collection

There are two main techniques for determining if allocated objects are garbage; tracing and reference counting.
Besides practical differences in performance and memory overhead, they differ in what information they 
take as input:

- Tracing algorithms require a set of roots; values that one is required to go through in order to reach heap
  objects. These would include stack variables and globals.
- Reference counting algorithms require the number of active reference counts to an object.

Determining which objects are roots at any given time in Rust is pretty annoying. It's certainly possible, there
are a few implementations of tracing collectors in Rust (including [rust-gc](https://github.com/Manishearth/rust-gc)),
but it doesn't seem particularly efficient. It involves recursively rooting/unrooting whenever you move an object 
into a new Gc (I think). 

Instead, we will use reference counting, because we _can_ pretty easily determine an object's reference count in 
Rust.



[1] As outlined in (Concurrent Cycle Collection in Reference Counted Systems by David F. Bacon and V.T. Rajan)[https://dl.acm.org/doi/10.5555/646158.680003] (I found the contents of the paper [here](https://pages.cs.wisc.edu/~cymen/misc/interests/Bacon01Concurrent.pdf)).
