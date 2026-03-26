A couple of days I came across this [interesting article](https://www.matheusmoreira.com/articles/generators-in-lone-lisp)
about implementing generators in the author's LISP. The author eschews using 
delimited continuations in their implementation, since [scheme-rs has delimited continuations](https://www.scheme.rs/Standard%20Libraries/prompts/), 
I wanted to see what an implementation would look like using them. 

The whole thing is less than 20 lines of code:

```scheme
(library (generators (1))
  (export yield generator)
  (import (rnrs) (prompts))

  (define generator-prompt-tag (gensym))

  (define (yield . args)
    (apply abort-to-prompt generator-prompt-tag args))

  (define (generator func)
    (lambda ()
      (call-with-prompt
       generator-prompt-tag
       func
       (lambda (k . returned)
         (set! func k)
         (apply values returned))))))
```

Let's also define a helper function to illustrate how these are used a little 
better:

```scheme
 (define (collect g)
    (let ((next (g)))
       (if (null? next)
           '()
           (cons next (collect g)))))
```

Now that we have this defined, we can define generators:

```scheme
> (define (f) (yield 1) (yield 2) '())
> (collect (generator f))
$1 = (1 2)
```

Neat! 

Scheme-rs's primitive for dynamic delimited continuation is the same as guile
and racket, known as ["prompts"](https://docs.racket-lang.org/guide/prompt.html),
so-called because they resemble a read-eval-print-loop prompt. The idea is that
you wrap some computation in a prompt tag (in scheme-rs's case, just a symbol) 
and a handler. In the computation we can "abort" to this prompt at any time, and
the remaining continuation is passed to the handler along with any values we've
aborted with.

Astute readers may recognize this as an [algebraic effect](https://en.wikipedia.org/wiki/Effect_system).
Indeed, dynamic delimited continuations and prompts are a primitive we can use 
to implement complex effect systems. 

Just like an effect system, the value of an implementation like this is that it
is _composable_:

```scheme
> (define (g) (yield 3) (yield 4) '())
> (collect (generator (lambda () (f) (g))))
$2 = (1 2 3 4)
> (collect (generator (lambda () (g) (f))))
$3 = (3 4 1 2)
```
