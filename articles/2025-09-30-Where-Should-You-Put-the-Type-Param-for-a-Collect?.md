In order for me to get back into the blogging spirit, here is a quick Rust tip
for you on my lunch break.

Often in Rust you will collect an iterator into a data structure and you will 
need to specify the type of the collection. There's two ways to do this, first
by specifying the type declaration of the variable you're collecting into:

```rust
let variable: Vec<_> = iterator_of_x().map(x_to_y).collect();
```

The second is to use the so-called "turbofish" operator to specify the type 
parameter as part of the call to the collect:

```rust
let variable = iterator_of_x().map(x_to_y).collect::<Vec<_>>();
```

Choosing between the two methods comes up from time to time, and indeed I've
had discussions with teams as to which method is better. I didn't really 
have an opinion until I started combining collects with Results.

One might expect that if you wanted to make the `x_to_y` function in our example
fallible (i.e. have it return a [Result](https://doc.rust-lang.org/std/result/enum.Result.html)),
that we would need to use some sort of function like [try_collect](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.try_collect) 
which indeed exists but is nightly only. Turns out we don't actually need this 
separate function because Result already implements [FromIterator](https://doc.rust-lang.org/std/iter/trait.FromIterator.html),
the trait that collect relies on. This means that turning `x_to_y` into a 
fallible function is as easy as changing our type declaration:

```rust
let variable: Result<Vec<_>, _> = iterator_of_x()
  .map(fallible_x_to_y)
  .collect();
```

This, however, does not play well with the `?` operator. What do we do now if 
we want to return early if there's an error?

```rust
let variable: Result<Vec<_>, _> = iterator_of_x()
  .map(fallible_x_to_y)
  .collect();
let variable = variable?;
```

This illuminates the key difference between the two methods we described: the 
type declaration annotates the _variable_ we're assigning to, while the type 
parameter annotates the _result of the collect_, and thus we can more easily 
chain the result with the latter:

```rust
let variable = iterator_of_x()
  .map(fallible_x_to_y)
  .collect::<Result<Vec<_>, _>()?;
```

This is not the only time one needs to add the type parameter to the collect
rather than annotate the variable; indeed if you wish to use the collect 
result in any purely expression context you will need to. 

Because there will be times you need to use the collect's type parameter rather
than a type declaration, I would argue you should _always_ use the type 
parameter to keep your code consistent. It might add three extra characters, but
I think that's a reasonable trade-off.
