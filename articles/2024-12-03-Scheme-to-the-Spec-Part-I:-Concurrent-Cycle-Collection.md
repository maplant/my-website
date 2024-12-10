# Scheme to the Spec (In Rust)

**This is a pre-print article**

Scheme to the Spec is a series on the more complex, often overlooked (at least didactically) aspects of programming language 
implementation.

In this series we will dive deep into my work-in-progress implementation of R6RS scheme, [scheme-rs](https://www.github.com/maplant/scheme-rs),
an implementation designed to integrate seamlessly with the async-rust ecosystem.

Our first article discusses how to implement Garbage-Collected smart pointers that we can use both within the interpreter
and the interfacing Rust code. In later articles we will discuss topics such as tail call optimizations, implementing continuations, 
and syntax transformers. Our final article will be implementing [on-stack replacement](https://www.graalvm.org/latest/graalvm-as-a-platform/language-implementation-framework/OnStackReplacement/) with LLVM.

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

unsafe impl<T: ?Sized + Send> Send for GcInner<T> {}
unsafe impl<T: ?Sized + Sync> Sync for GcInner<T> {}

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

unsafe impl<T: ?Sized + Send> Send for GcReadGuard<'_, T> {}
unsafe impl<T: ?Sized + Sync> Sync for GcReadGuard<'_, T> {}

pub struct GcWriteGuard<'a, T: ?Sized> {
    _permit: tokio::sync::SemaphorePermit<'a>,
    data: *mut T,
    marker: PhantomData<&'a mut T>,
}

impl<T> Deref for GcWriteGuard<'_, T: ?Sized> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.data }
    }
}

impl<T> DerefMut for GcWriteGuard<'_, T: ?Sized> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.data }
    }
}

unsafe impl<T: ?Sized + Send> Send for GcWriteGuard<'_, T> {}
unsafe impl<T: ?Sized + Sync> Sync for GcWriteGuard<'_, T> {}
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
Rust. In Rust, an object's reference count is determined by the difference of clones and drops plus one. This is 
because Rust has an [affine type system](https://en.wikipedia.org/wiki/Substructural_type_system#Affine_type_systems)
for objects that do not implement `Copy`, which means that an object that does not implement `Copy` can be moved 
at _most_ once, which means that moves do not affect the reference count of an object. Now, if we were able to construct
a `Gc` manually, i.e. by manually instanciating its fields with a copied [`NonNull`](https://doc.rust-lang.org/std/ptr/struct.NonNull.html#impl-Copy-for-NonNull%3CT%3E),
our invariant would not hold. But Gc's field is private, and thus we can be assured that a `Gc` can only be created
by user code via the `new` - which instanciates a new `Gc` with a ref count of one - or `clone` functions - which
increment the reference count. Since `drop` only runs when a variable is [no longer live](https://en.wikipedia.org/wiki/Safety_and_liveness_properties),
we know that it corresponds to a decrement in the ref count. Drop is not _guaranteed_ to be invoked for an object, 
a user could call [`forget`](https://doc.rust-lang.org/std/mem/fn.forget.html) with the object and its destructor
will not be run. But practically speaking, the destructor for an object is pretty much always going to be run.

Reference counting does have a pretty key problem: cycles

### Cycles

(fig1)
 
An object can be unreachable but still have a positive reference count. This is because our data structures allow for
_cycles_ (see fig1). Such data structures are pretty common to create, especially in functional languages like Scheme.
But even if they _weren't_, since they _are_, a correct to the spec implementation of Scheme _must_ be able to 
recognize such cyclical data structures and garbage collect them when appropriate. To that end, we will be implementing
[Concurrent Cycle Collection in Reference Counted Systems by David F. Bacon and V.T. Rajan](https://dl.acm.org/doi/10.5555/646158.680003),
and algorithm for automatically detecting and collecting cycles in reference counted data structures. 

### Synchronous Cycle Collection 

Here is the code listing for the synchronous cycle collection algorithm:

<table style="border: 0px">
  <tr>
    <td>
```
Increment(s)
    RC(S) = RC(S) + 1
    color(S) = black
```    
    </td>
    <td>
```    
ScanRoots()
    for S in Roots
        Scan(s)
```        
    </td>
  </tr>
</table>

That's all well and good, but how does this work? Well, I will first say you should read the paper, because it is quite
short and really approachable. But I will briefly explain how it works, leaving out a few key details. 

Cycle Collection relies on this key insight: if you were to perform a drop on every Gc in a cycle, that cycle will be 
garbage if the remaining ref count of every node in that cycle is zero. This kind of makes sense intuitively, what we're 
basically saying here is that if we somehow already knew that a data structure was cyclic, we could just manually sever
an outgoing reference for some random node and the cascade of decrementing reference counts would cleanly free the whole 
data structure. 

With this in mind, we can can construct an efficient algorithm to collect cyclical garbage. We'll color the nodes in 
different colors as they pass through different stages 

- `Decrement`: When we decrement a reference count and the reference count is greater than zero, add it to our list of
possible roots. If we've already added it before performing a collection cycle (it's been marked _purple_), skip this.
- `Mark`: Go through the roots, and perform the test described in the previous paragraph. Practically, this works by 
performing a depth first search on the root, marking each node gray and decrementing the reference count of each child
and repeating the process on them. If the child is already marked gray, we skip them.
- `Scan`: Go through the roots and recursively check their children for reference counts greater than zero. If there is
any greater than zero, it recursively marks all of their children black to indicate the data is live. Otherwise, the 
structure is marked white, to indicate it is ready to be freed.
- `Collect`: Go through the roots marked white and free them. 

There is one thing in particular that is not immediately clear about how we are going to fit this into our system. How do 
we iterate over the children? We haven't put any bounds on the `T` in our `Gc<T>` beyond that it has to be `'static`. Well,
until Rust gains a more powerful reflection story, we are going to have to add a classic Trait plus derive macro combo.

### The Trace trait and derive macros

Let's define the trait we need to implement the above code. Basically, we need a function that matches the following form:

```
for T in children(S):
    F(T)
```

We can extract two type parameters from this statement: S, and F(T) where T == S. Therefore, the function we want will
have the form `fn for_each_children(S, impl FnMut(S))`.

It's not entirely obvious at first glance, but there is another function that we must pay attention to: that is `free`.
The `free` function presented in the above code listing does _not_ correspond to how memory is freed in Rust. That is 
because this code assumes that when we free code we are not running any of its members destructors. But we _do_ want to
run our destructors. At least, we want to run the destructor _if_ the type is _not_ a `Gc`. We're going to have to add 
a `finalize` function to handle a custom drop routine:
 

```rust
unsafe trait Trace: 'static {
    unsafe fn visit_children(&self, visitor: unsafe fn(OpaqueGcPtr));
    
    unsafe fn finalize(&mut self) {
        drop_in_place(self as *mut Self);
    }
}
```

These two things are in fact the _only_  two things our collection algorithm cares about in regards to the Gc's data 
pointer. Therefore, whenever we're in the context of the collection algorithm, we will cast our Gc's into a trait object:

```rust
type OpaqueGc = GcInner<dyn Trace>;
pub type OpaqueGcPtr = NonNull<OpaqueGc>;

impl<T: Trace> Gc<T> {
    pub unsafe fn as_opaque(&self) -> OpaqueGcPtr {
        self.ptr as OpaqueGcPtr
    }
}
``` 

Now that we have our Trace trait, we can implement it for some primitive types, such as those provided by the standard
library (or really, whatever we're using to build our data). I won't provide all of the ones I implemented, but here 
are a few:

```rust
unsafe trait GcOrTrace: 'static {
    unsafe fn visit_or_recurse(&self, visitor: unsafe fn(OpaqueGcPtr));

    unsafe fn finalize_or_skip(&mut self);
}

unsafe impl<T: Trace> GcOrTrace for Gc<T> {
    unsafe fn visit_or_recurse(&self, visitor: unsafe fn(OpaqueGcPtr)) {
        visitor(self.as_opaque())
    }

    unsafe fn finalize_or_skip(&mut self) {}
}

unsafe impl<T: Trace + ?Sized> GcOrTrace for T {
    unsafe fn visit_or_recurse(&self, visitor: unsafe fn(OpaqueGcPtr)) {
        self.visit_children(visitor);
    }

    unsafe fn finalize_or_skip(&mut self) {
        self.finalize();
    }
}

unsafe impl<T> Trace for Vec<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: unsafe fn(OpaqueGcPtr)) {
        for child in self {
            child.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for mut child in std::mem::take(self).into_iter() {
            child.finalize_or_skip();
            std::mem::forget(child);
        }
    }
}

unsafe impl<T> Trace for Option<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: unsafe fn(OpaqueGcPtr)) {
        if let Some(inner) = self {
            inner.visit_or_recurse(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        if let Some(inner) = self {
            inner.finalize_or_skip();
        }
    }
}

unsafe impl<T> Trace for std::sync::Arc<T>
where
    T: GcOrTrace,
{
    unsafe fn visit_children(&self, visitor: unsafe fn(OpaqueGcPtr)) {
         self.as_ref().visit_or_recurse(visitor);
    }
}
```

If you are able to immediately spot the error in the code above, you are much smarter than
I am. I hope you at least have the courtesy to be less good looking. Anyway, this code isn't
correct with our current set of assumptions. And that's because of the implementation for
Arc, which recurses into its data. 

The problem is that we are basically disregarding the reference count of the `Arc`. consideer the
following situation:

(figure 3)

In this case, dropping A results in our immediately dropping of C. Essentially, the `Arc` collapses
all of the incoming references to C into one. 

This is incorrect, it is probably fixable with lots more code and trait functions, but for now
I'm just going to say "`Arcs` are _still_ required to address their own cycles with `Weak`s". 
I'm not giving them _less_ functionality, just not _more_.

Here is the correct code for `Arc`. Part of what makes this code correct is that we added an 
explanation, so make sure to always do that. 

```rust
unsafe impl<T> Trace for std::sync::Arc<T>
where
    T: ?Sized + 'static,
{
    unsafe fn visit_children(&self, _visitor: unsafe fn(OpaqueGcPtr)) {
        // We cannot visit the children for an Arc, as it may lead to situations
        // were we incorrectly decrement a child twice.
        // An Arc wrapping a Gc effectively creates an additional ref count for
        // that Gc that we cannot access.
    }
}
```

I am not going to get too into the Trait derive proc macro, it's not particularly interesting. But it's
important to note what it is actually doing for a given type `T`:

- `visit_children`: for every field, if the type is a `Gc`, call `visitor` on `.opaque_gc`. If the type
is _not_ a `Gc`, recursively call `visit_children` on it. 
- `finalize`: for every field, if the type is _not_ a `Gc`, call `finalize` on it. 

If you would like to see how it was done, the code is available [here](https://github.com/maplant/scheme-rs/blob/main/proc-macros/src/lib.rs).

### Deallocation

The `free` function needs to call [`std::alloc::dealloc`](https://doc.rust-lang.org/std/alloc/fn.dealloc.html) to 
`free` the allocated memory. This function takes a [Layout](https://doc.rust-lang.org/std/alloc/struct.Layout.html),
how do we find this for our opaque `dyn Trace` pointer? Turns out the memory and alignment of the underlying data 
is part of Rust's vtable. So, we can just call `Layout::for_value` on our reference and create one. Here's the resulting
free function:

```rust
unsafe fn trace<'a>(s: OpaqueGcPtr) -> &'a mut dyn Trace {
    &mut *s.as_ref().data.get()
}

unsafe fn free(s: OpaqueGcPtr) {
    // Safety: No need to acquire a permit, s is guaranteed to be garbage.
    let trace = trace(s);
    let layout = Layout::for_value(trace);
    trace.finalize();
    std::alloc::dealloc(s.as_ptr() as *mut u8, layout);
}
```

### Concurrent Cycle Collection
 
We have all the pieces to implement the synchronous cycle collection algorithm, so we're going to move 
right along into implementing concurrent cycle collection. It's a very similar algorithms, and I'll address
the differences as they come along.

Firstly, let's fix our `GcHeader` to have all the information it needs:

```rust
pub struct GcHeader {
    rc: usize,
    crc: usize,
    color: Color,
    buffered: bool,
    semaphore: Semaphore,
}

enum Color {
    /// In use or free
    Black,
    /// Possible member of a cycle
    Gray,
    /// Member of a garbage cycle
    White,
    /// Possible root of cycle
    Purple,
    /// Candidate cycle undergoing Σ-computation
    Red,
    /// Candidate cycle awaiting epoch boundary
    Orange,
}
```

You will notice that the ref counts for our `Gc` type are _not_ atomic. This is because our concurrent 
algorithm requires a constraint: only one thread at a time can _ever_ modify the ref count, or indeed
_any_ field in the `GcHeader`. This gives our garbage collection process exclusive read and write 
access to the header. 

What gives? How does _that_ work? Isn't this algorithm supposed to be concurrent? We don't want
decrementing or incrementing ref counts to wait on the completion of the GC process. That would make
our drop and clone functions blocking, which would gum up our whole system.

The solution is the mutation buffer, and unbounded channel that let's us buffer incremements and 
decrements to be handled by our collection process as it pleases.

This wil allow our collector to stop processing incremements and decrements at any time and effectively
have a snapshot of the system without causing any pauses in other threads. Since the buffer is _unbounded_,
calling `send` is a non-blocking, sync operation:


```rust
#[derive(Copy, Clone)]
struct Mutation {
    kind: MutationKind,
    gc: NonNull<OpaqueGc>,
}

impl Mutation {
    fn new(kind: MutationKind, gc: NonNull<OpaqueGc>) -> Self {
        Self { kind, gc }
    }
}

unsafe impl Send for Mutation {}
unsafe impl Sync for Mutation {}

#[derive(Copy, Clone)]
enum MutationKind {
    Inc,
    Dec,
}

/// Instead of mutations being atomic (via an atomic variable), they're buffered into
/// "epochs", and handled by precisely one thread.
struct MutationBuffer {
    mutation_buffer_tx: UnboundedSender<Mutation>,
    mutation_buffer_rx: Mutex<Option<UnboundedReceiver<Mutation>>>,
}

unsafe impl Sync for MutationBuffer {}

impl Default for MutationBuffer {
    fn default() -> Self {
        let (mutation_buffer_tx, mutation_buffer_rx) = unbounded_channel();
        Self {
            mutation_buffer_tx,
            mutation_buffer_rx: Mutex::new(Some(mutation_buffer_rx)),
        }
    }
}

static MUTATION_BUFFER: OnceLock<MutationBuffer> = OnceLock::new();

fn inc_rc<T: Trace>(gc: NonNull<GcInner<T>>) {
    // SAFETY: send takes an immutable reference and is atomic
    MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .send(Mutation::new(MutationKind::Inc, gc as NonNull<OpaqueGc>))
        .unwrap();
}

fn dec_rc<T: Trace>(gc: NonNull<GcInner<T>>) {
    // SAFETY: send takes an immutable reference and is atomic
    MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_tx
        .send(Mutation::new(MutationKind::Dec, gc as NonNull<OpaqueGc>))
        .unwrap();
}
```

And now we finally can implement `Clone` and `Drop` for `Gc<T>`:

```rust
impl<T: Trace> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        inc_rc(self.ptr);
        Self {
            ptr: self.ptr,
            marker: PhantomData,
        }
    }
}

impl<T: Trace> Drop for Gc<T> {
    fn drop(&mut self) {
        dec_rc(self.ptr);
    }
}
```

This also presents a sturcture for our collection process: wait for some number of increments and 
decrements, process them, then decide if we want to try to perform a collection:


```rust
static COLLECTOR_TASK: OnceLock<JoinHandle<()>> = OnceLock::new();

pub fn init_gc() {
    // SAFETY: We DO NOT mutate MUTATION_BUFFER, we mutate the _interior once lock_.
    let _ = MUTATION_BUFFER.get_or_init(MutationBuffer::default);
    let _ = COLLECTOR_TASK
        .get_or_init(|| tokio::task::spawn(async { unsafe { run_garbage_collector().await } }));
}

const MAX_MUTATIONS_PER_EPOCH: usize = 10_000; // No idea what a good value is here.

async unsafe fn run_garbage_collector() {
    let mut last_epoch = Instant::now();
    let mut mutation_buffer_rx = MUTATION_BUFFER
        .get_or_init(MutationBuffer::default)
        .mutation_buffer_rx
        .lock()
        .take()
        .unwrap();
    let mut mutation_buffer: Vec<_> = Vec::with_capacity(MAX_MUTATIONS_PER_EPOCH);
    loop {
        epoch(&mut last_epoch, &mut mutation_buffer).await;
        mutation_buffer.clear();
    }
}

async unsafe fn epoch(last_epoch: &mut Instant, mutation_buffer: &mut Vec<Mutation>) {
    process_mutation_buffer(mutation_buffer).await;
    let duration_since_last_epoch = Instant::now() - *last_epoch;
    if duration_since_last_epoch > Duration::from_millis(100) {
        tokio::task::spawn_blocking(|| unsafe { process_cycles() })
            .await
            .unwrap();
        *last_epoch = Instant::now();
    }
}

/// SAFETY: this function is _not reentrant_, may only be called by once per epoch,
/// and must _complete_ before the next epoch.
async unsafe fn process_mutation_buffer(
    mutation_buffer_rx: &mut UnboundedReceiver<Mutation>,
    mutation_buffer: &mut Vec<Mutation>
) {
    // SAFETY: This function has _exclusive access_ to mutate the header of
    // _every_ garbage collected object. It does so _only now_.
    mutation_buffer_rx
        .recv_many(mutation_buffer, MAX_MUTATIONS_PER_EPOCH)
        .await;

    for mutation in mutation_buffer {
        match mutation.kind {
            MutationKind::Inc => increment(mutation.gc),
            MutationKind::Dec => decrement(mutation.gc),
        }
    }
}
```
