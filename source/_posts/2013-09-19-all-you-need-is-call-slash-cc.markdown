---
layout: post
title: "All you need is call/cc"
date: 2013-09-19 1:30
comments: true
categories: Puzzle
---
I was going to post this as a comment on
[r/lisp](http://www.reddit.com/r/lisp/comments/1mkqvj/why_monads_have_not_taken_the_common_lisp_world/ccb5sor),
but I feel it's grown a bit too much for the platform.  For one time
only, this blog will host some Racket code.  I apologise in advance to
any Schemer reading this.  I needed an easy way to play with control
operators, but I've never really Schemed, so there's probably a dozen
issues with the code and its style.

So, why is the
[continuation monad the mother of all monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html)?
The short answer is that, by enabling transparent inversion of
control, it eliminates the need to sprinkle hooks for monad-specific
code everywhere; normal (as much as anything involving delimited
continuations can be "normal") evaluation rules will be subverted as
needed.  Here's how.

First, some boilerplate (boilerfoil may be the more appropriate term).

    #lang racket
    (require racket/control) ; for shift/reset
    
    (define (run thunk return . arguments)
      (reset (return (apply thunk arguments))))

This definition is all that is needed to execute arbitrary Racket code
in a monad: the only thing to specify is how a ground value should be
moved in the monad.  `bind` will be defined implicitly, through the
code for monad-specific behaviour.  The body of `run` defines a
context to determine where to stop capturing the continuation, and
executes the form `(return (apply thunk arguments))` in that context:
the thunk is called with any argument provided, and the result is
`return`ed into the monad.

For the sake of generalisation, I'll start with a trivial example: the
Maybe monad.  First, I'll quickly define structure types.  In
practice, a distinguished "nothing" value would suffice, but this way
parallels Haskell more closely.

    (struct Maybe ())
    (struct Nothing Maybe ())
    (struct Some Maybe (value))

The constructor `Some` also doubles as `return`.  In order to abort
out, `nothing` must unwind the continuation and return a `Nothing`
object.

    (define (nothing)
      (shift k (Nothing)))

    > (run (lambda () 42) Some)
    #<Some>
    > (Some-value (run (lambda () 42) Some))
    42

The function `run` obviously works as it should in these trivial
examples.  It's also not surprising that `nothing` works, because it's
the obvious implementation of unwinding with delimited continuations.

    > (run (lambda () 42 (nothing) #t) Some)
    #<Nothing>

In the List monad, `return` is just `list`. `run` can be called with a
thunk and `list` as the second argument, and indeed, the result is a
normal computation that returns its value as a singleton.

    > (run (lambda () (+ 4 5)) list)
    '(9)

The useful thing to do with the List monad is to specify multiple
return values, and have the computation fork (lazily in Haskell,
eagerly here, because I'm working with strict lists) on each
choice. That's a one-liner:

    (define (inject-values . values)
      (shift k (append-map k values)))

This function captures the continuation up to `reset`, unwinds the
current continuation up to that point, and binds the captured
delimited continuation to `k`.  Then, it passes each possible value to
the continuation, and appends the results together.

Here's a first example:

    > (run (lambda () (+ (inject-values 1 2) (inject-values 3 4))) list)
    '(4 5 5 6)

That is, reassuringly, the four possible sums: `1 + 3 = 4`, `1 + 4 = 5`,
`2 + 3 = 5`, and `2 + 4 = 6`.  The magic is that all Scheme code,
by virtue of supporting the capture (and unwind) of delimited
continuation, is now "in" the List monad.  It is certainly the case
for that uninstrumented thunk.  The pre-defined function `map`
provides a more convincing example.

    > (run (lambda ()
             (map cons
                  (inject-values '(1 2) '(4 5))
                  (inject-values '(3 4) '(5 6))))
           list)
    '(((1 . 3) (2 . 4))
      ((1 . 5) (2 . 6))
      ((4 . 3) (5 . 4))
      ((4 . 5) (5 . 6)))

I've taken the liberty of line-wrapping the return value, and clearly,
`(map cons ...)` has been called with all four pairs of lists...  but
all the special monadic operations happens outside `map`.  Moving
`inject-values` inside the mapped function is a much stronger evidence
that arbitrary uninstrumented Scheme code is implicitly lifted in the
monad: `map` is a predefined (pre-compiled even) library function.

    > (run (lambda ()
           (map (lambda (x y) ((inject-values + -) x y))
                '(1 2 3)
                '(4 5 6)))
         list)
    '((5 7 9)     ; + + +
      (5 7 -3)    ; + + -
      (5 -3 9)    ; + - +
      (5 -3 -3)   ; + - -
      (-3 7 9)    ; - + +
      (-3 7 -3)   ; - + -
      (-3 -3 9)   ; - - +
      (-3 -3 -3)) ; - - -

At each call to the mapped function, the computation explores both the
branch in which the arguments are added and the one in which they are
subtracted.  The result, for 3 pairs of triplets, is a list of 8
triplets.

Neither of these implementations is surprising or new; I believe
they're standard undergraduate exercises in a few universities.  The
insight in
[Filinski's work](http://www.diku.dk/hjemmesider/ansatte/andrzej/papers/RM-abstract.html)
is that both `nothing` and `inject-values` share the same structure
and can be defined in terms of the monad they help implement.  Because
I dislike scrolling, their definitions are copied here.

    (define (nothing) ; Maybe
      (shift k (Nothing)))

    (define (inject-values . values) ; List
      (shift k (append-map k values)))

Instead of returning the (monadic) value `(Nothing)` or `values` (a
list), both capture and unwind the delimited continuation, bind it to
`k`, and then do something more to the value.  Some squinting reveals
that that something more is calling `bind` with the continuation `k`
as the next step.  In the Maybe monad, `Nothing >>= k` evaluates to
`Nothing`.  In the List monad, `values >>= k` becomes `foldr ((++)
. k) [] values`, which is basically `(append-map k values)`.  The
general form for any monad is then to implement any operator that
doesn't just `return` a value as

    (define (operator ...)
      (shift k (bind [special stuff] k)))

As code, this gives

    (define (make-operator bind operator)
      (lambda arguments
        (shift k (bind (apply operator arguments) k))))

after adding support for variadic operators.  For example, choosing
between multiple items in a list is

    (define (list-bind x k) (append-map k x))
    
    (define inject-values2 (make-operator list-bind list))

Some sketching on paper will show that the transformation is generic
and correct, with a proof that mostly goes through repeated
applications of the monad laws.  Capturing the continuation moves the
whole stack of functions that will eventually receive results into a
single function (the continuation), and that function can then be
passed to `bind`.  The monad laws guarantee that this associative
reordering of nested `bind`s preserves the meaning of the program.

We can also mimic `do`-notation more closely and implement `join`, an
"anti-return".  Given that operator, not `return`ing a value can instead
be implemented as `join`ing it after the fact.  One definition is
`(make-operator bind identity)`, but I feel it's simpler to just do it
longhand.

    (define (make-join bind)
      (lambda (value)
        (shift k (bind value k))))

    (define join-list (make-join list-bind))

    > (run (lambda () (+ 1 (join-list '(1 2 3)))) list)
    '(2 3 4)

Of course all this also works when `operator` is equivalent to
`return`; it's just pointless.  The `shift`/`return`/`bind` dance is
then a convoluted way to demand regular Scheme evaluation rules.

And that is how the continuation monad is universal.  When code is in
the continuation monad (or in a language with delimited
continuations), there is a mechanical way to have that code execute in
almost any other monad.  There are technical restrictions on the monad
for the transformation to work, but I think any monad that can be
implemented in (pure) Haskell qualifies.

I feel like sigfpe's presentation was hobbled by the fact that he used
the Continuation monad in Haskell, making it harder to see that the
Continuation monad of the implementation is completely independent of
the emulated one.  Really, the idea is one of these nice insights that
formalise and generalise old hacks, and seem obvious in retrospect.

The post's title refers to the fact that delimited continuations can
themselves be implemented in terms of `call-with-current-continuation`
(and a single mutable cell).  There are many ways to interpret the
corollary that `call/cc` suffices to transpose code into arbitrary
monads.  It certainly seems like a victory for the minimalist crowd.
On the other hand, I believe that the power of programming languages
and paradigms lies as much in what they enable as it does in what they
forbid (or make inconvenient, at least).  From that point of view,
it's not obvious whether the universality of `call/cc` makes a strong
case for or against the feature.

This result also provides a tentative explanation for the low traction
of monads among Lispers: perhaps many would rather directly hack the
features in, with (ersatz) continuations.
