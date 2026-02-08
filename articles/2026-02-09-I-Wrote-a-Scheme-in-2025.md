One year minus one week ago to the day I [published an article](https://maplant.com/2025-02-17-Why-I%27m-Writing-a-Scheme-Implementation-in-2025-%28The-Answer-is-Async-Rust%29.html) 
announcing the new Scheme implementation I was writing, [scheme-rs](https://www.scheme.rs). 
Today I am excited to announce that `scheme-rs` has reached its first version, 
`0.1.0`.

Although there's much more work to do (and I will talk about that further down 
the page), after reaching the milestone of completing `2258` tests in the R6RS 
test suite, I've decided that `scheme-rs` is stable enough to commit to a first
release.

It's been a pretty amazing journey, one that I'm excited to continue on. 

## Changes since I initially announced scheme-rs last year:

There have been some changes since I initially announced scheme-rs. The biggest
is that it is no longer exclusively async, and now supports sync as well. 

This change was inevitable; only supporting async would really hamper adoption
(or more importantly my personal use cases for the project). But I'm really 
happy that I was able to implement this change without hamstringing async 
support at all. In fact, it is even possible to use scheme-rs in both async and
sync contexts. 

## Things that are not finished

There are quite a few things that are not quite complete, and I will list them
here:

- The garbage collector has quite a bit of room for improvement.
- Performance in general is Ok but could be made much better.
- There are a lot of missing procedures and syntax from R6RS but also just in
  general, for example I'd really like to add pattern matching.
- There's always more documentation that can be added.
- Debugging could be greatly improved.

But more importantly, I'm very excited to start working on the new language I am
building on top of `scheme-rs`. I mentioned that it would be strongly typed, but
now I think I might want to use calculus of construction. The possibilities are
truly endless!

## An aside on how scheme-rs came to be:

In August of last year I had just lost my job. I wasn't sure exactly what to do.
I looked out on the landscape of software engineering jobs and became 
increasingly uncertain of my place in it. I had been unsatisfied with work for a
number of years at that point, and it felt like I had lost the differentiating 
factor that made me worth hiring in the first place. The idea of getting a job 
that was interesting, one that challenged me, seemed to be completely out of 
reach.

I decided that what I would do is look inward. I would try to rediscover where 
my original love for computer science came from. I had this dinky little project
that I had been working on and off on for a while with no clear vision. I 
decided that I would spend my free time dedicated to that project. I would do 
research and focus on doing things correctly and with a strong backing from 
academics. I'd apply for a few jobs, but if those fell through I'd apply for 
grad school instead. 

I'd buy a printer and read papers.

<img src="./papers.jpeg" style="width: 600px" alt="A collection of three books: (Compiling with Continuations by Andrew W. Appel, Advanced Language Implementation edited by Peter Lee, Revised [6] Report on the Algorithmic Language Scheme edited by Michael Sperber, R. Kent Dybvig Matthew Flatt and Anton van Straten) and four papers: Effective Nondestructive Equality Checking for Trees and Graphs, Concurrent Cycle collection in Reference Counted Systems, Extending the Scope fo Syntactic Abstractions, and Implicit Phasing for R6RS Libraries"/>

That dinky little project was `scheme-rs`, and it's hard to describe just how 
much it changed my life. I feel much more confident in my ability as a software
engineer. I no longer think that a PhD is something that is meant for people 
much smarter than me. And more miraculously it directly lead me to finding a job
that I absolutely love.

`scheme-rs` is not particularly special in this regard. It was simply something 
that challenged me. And if you have a brain at all like mine, I hightly 
recommend you find a project that challenges you like `scheme-rs` challenged me.

If you like being challenged and enjoy functional programming and Rust as much
as I do, check out [the careers page at OneChronos!](https://www.onechronos.com/careers/#open-roles)
