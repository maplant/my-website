This blog post is an introduction to [scheme-rs](https://www.github.com/maplant/scheme-rs).

I really love Rust. I've been using it as my language of choice for about ten years now. I
believe that it is a remarkably well designed language, and that includes the parts that are
often most criticized, most notably async.

But while I thing that async Rust is well designed from the context of the space it occupies,
programming with it is not a particularly great experience once you finish your first draft
of code and want to start iterating. As soon as you run `cargo build`, inconveniences show:
slow compile times, inscrutable error messages with stack traces that are mostly just tokio
functions, and just overall a poor debugging experience make async Rust a frustrating 
experience for rapid iteration.

The thing is, this experience is the result of conscious trade-offs, trade-offs that I believe
were chosen _correctly_. I don't believe that async Rust necessarily needs to change at all,
although I don't doubt that the experience could be improved. In my mind, what async Rust 
really needs is a proper glue language. That's where `scheme-rs` comes in.

`Scheme-rs` is a work-in-progress implementation of [R6RS](https://www.r6rs.org/) that allows
for seamless interoperability with async Rust. All you have to do is add `scheme-rs` to your
`Cargo.toml`, define some bridge functions with the provided proc macros, and `scheme-rs` 
will provide you with a way to dynamically glue your code together, including allowing you to
provide a REPL for debugging, inspecting, or even orchestrating your system.

Why Scheme and not a new language? There are a couple of reasons, the first is that Scheme 
strikes an excellent balance between having a small core and being extremely expressive. 
Choosing Scheme lets one take advantage of the millions of lines of Scheme code already 
written. 

As an aside, R6RS happens to have the best macro system of any programming language ever.
Macros are hygienic by default but let you selectively break free from hygiene in a 
remarkably clean way. Additionally, the homoiconic nature and ease of dealing with 
s-expressions means you rarely have to do any complicated parsing or deal with ASTs. While
`scheme-rs` is very much still a WIP, it implements R6RS macros nearly perfectly, including 
all of the necessary hygiene conditions and ways to bend them, including `syntax-case` and 
`datum->syntax`. 

The second reason I chose Scheme instead of a new language is that I _do_ want to create a 
new language, but I want to be built upon Scheme and to have it utilizes its beautiful 
macro system and syntax while providing a stronger and more useful type system (in particular,
Hindley-Milner with sum types, type classes, and some form of limited subtyping (perhaps 
[Parametric Subtyping](https://blog.sigplan.org/2025/01/29/parametric-subtyping-for-structural-parametric-polymorphism/))).

I call this theoretical language Gouki, and it would sit somewhere in the middle of `scheme-rs`
and Rust. Ideally you would write almost all of your code in Gouki, going down into Rust
when you need to do low-level stuff, and use Scheme occsionally whether it be via REPL or 
incidentally (Gouki is intended to be a superset of Scheme, so every valid Scheme program
is a valid Gouki program).

Such a language is squarely in the design phase, but here's a preview of what it might look
like:

```scheme
;; Enumerations:
(enum Option (Some 'a)
             (None))

(let ([x (Some 5)])
    (match x
       ((Some y) y)
       ((None) (random))))

(fn random () (-> 'a)
    (where (: 'a Distribution))
    (sample (thread-rng)))

(class Eq (self)
    (: = (-> self self bool)))
    
(instance Eq (Number)
    (fn = (lhs rhs) (-> Number Number bool)
        (num-equal lhs rhs)))
```

So, I mentioned twice now that `scheme-rs` is a WIP, how close is it to being usable? Well,
in one sense it is usable right now. Besides missing a large number builtins, in terms of
core language features we're currently missing `dynamic-wind`, error handling, and records
Additionally, the interface is a little bit clunky. But the bones are there: tail calls are 
implemented properly, as is call-by-current-continuation, and as I mentioned earlier the macro 
system is extremely robust. The hard work has been done, and now it's just a matter of adding 
features. 

I spent my free time for the last month converting `scheme-rs` from an interpreter to a 
compiler, one that converts the expanded Scheme program into a CPS mid-level IR, and then
to LLVM SSA for JIT compilation. This actually significantly slowed the performance of 
`scheme-rs`, as the CPS and LLVM outputs are very poorly optimized. But I did not initiate
this huge re-architecture because it would be immediately faster, I initiated it because it
was necessary to eventually make `scheme-rs` competitive performance-wise with more mature
Scheme compilers like `ChezScheme`.

R6RS is a pretty massive spec, but I hope to complete it somewhat soon (although I started a new
job recently, so no idea what that really means), and after that I hope to focus on making the
compiler produce the best code as possible. In the mean time, I hope you consider taking a look
at `scheme-rs`, and contributing if you find the project interesting. 
