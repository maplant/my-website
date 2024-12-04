# Scheme to the Spec (In Rust)

**This is a pre-print article**

Scheme to the Spec is a series on the more complex, often overlooked (at least didactically) aspects of programming language 
implementation.

In this series we will dive deep into my work-in-progress implementation of R6RS scheme, [scheme-rs](https://www.github.com/maplant/scheme-rs),
an implementation designed to integrate seamlessly with the async-rust ecosystem.

Our first article is discussing how to implement Garbage-Collected smart pointers that we can be used both within the interpreter
and the interfacing Rust code. In later articles we will discuss topics such as tail call optimizations, implementing continuations, 
and syntax transformers.


## Part I: Garbage Collected Smart Pointers with Concurrent Cycle Collection

Ten years ago I wrote an article on how to implement a conservative garbage collector for C as a didactic exercise. It was a pretty
popular article, you can see the [hacker news link here](https://news.ycombinator.com/item?id=8222487). A lot of people criticized
the vast number of assumptions I made and hand waving I performed to get to the conclusion, including in later re-posts people I 
tremendously respect, so it only makes sense on the tenth-year anniversary of writing that article that I start this blog series with
a garbage collector that is precise, does not make assumptions, and actually works. 

To give some background, [scheme-rs](https://www.github.com/maplant/scheme-rs) is an interpreter for the Scheme programming language
that I am actively working on. You don't need to know much about Scheme for this article in particular, but for this article you do
need to know that Scheme is a garbage-collected language. Scheme-rs is implemented in Rust, and part of the design goal is to be able
to access scheme data within Rust. To that end, we need some way to have data that we can use both within the interpreter and Rust
itself, all while having it be garbage-collected. To do that, we are going to create a smart pointer

## The `Gc<T>` Smart Pointer 

Before we can start writing code, we have to figure out what goals we want to accomplish. More specifically, what do we want our 
`Gc<T>` type to _do_? How does it _behave_? What kind of _data can it contain_?

I want the `Gc<T>` type to work as follows:

- The API for the `Gc<T>` should behave similarly to a `Arc<tokio::sync::RwLock<T>>`; that is, it should support interior mutability
  through a read/write lock and it should be clonable and sendable across threads.
- `T` is allowed to be _any_ data type that satisfies `'static + Send + Sync`. This includes `Arc`. 
- When `Gc<T>` no longer has any references to it _on the stack_, then we should clean it up properly, _including any cycles_. 

Now that we have an idea as to how our smart pointer should behave, we can begin our implementation. Let's get started.

### Scaffolding and allocation 

Our Gc type will be composed of three separate types:

- `Gc<T>`: User-facing type, contains a pointer to our heap allocated memory.
- `GcInner<T>`: The inner data type, which contains `T` and any other information we may need to keep track of the data.
- `GcHeader`: The "any other information we may need to keep track of".

To start out, we have no extra information we need to store in `GcHeader`, so we can knock that one out quickly:

```rust
struct GcHeader {
}
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

Unfortunatley, this largely unsupported by Rust at the current moment. It is possible on unstable rust, but 
at the time of the article it remains unstabalized. 

But lifetimes can _also_ appear in dyn traits! Basically, while we wait for our _actual_ use case to stabalize,
we are ensuring a covariant relationship in order to support subtype relationships of the following form:

`Gc<dyn FnMut(&'a T)>` is a subtype of `Gc<dyn FnMut(&'b T)>` for `'a: 'b`.

That is largely the reason why were are supporting subtyping. Closures. 

It's also not that difficult; we just need to use a [`NonNull`](https://doc.rust-lang.org/std/ptr/struct.NonNull.html) pointer wrapper instead of a raw `*mut T`, a type which _is_ invariant over `T`. This is what we'd do anyway, as we don't _want_ a `Gc<T>` to contain a pointer that _is_ null.

Now that we have that worked out, we can move onto the second thing we need to express:

#### Drop checking

As specified now as just a `NonNull<T>` and nothing else, the Rust compiler is forced to assume that any 
`Gc<T>` will be strictly out-lived by the underlying data. This is however, not the case. Although a lot of Gcs 
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
lifetime an immutable reference is held, it will not be modified. We would like to opt out of that. To that
end, we must use the [`UnsafeCell`](https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html) wrapper. 

Obviously, we want to make sure that if someone is trying to read the data behind a a Gc that no one is trying
to modify it at the same time. But we are going to have uphold that invariant at _runtime_ rather that compile
time. You must always uphold Rust's safety rules, but there are specific escape hatches that we can enable in
order to allow Rust to trust us in certain places. `UnsafeCell` is one of those escape hatches. 

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


## Garbage Collection
