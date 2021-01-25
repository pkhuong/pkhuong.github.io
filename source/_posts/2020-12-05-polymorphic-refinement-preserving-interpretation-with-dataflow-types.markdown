---
layout: post
title: "Polymorphic refinement-preserving interpretation with dataflow types"
date: 2020-12-05 20:35:39 -0500
draft: true
hidden: true
comments: false
categories: 
---

Many years ago, I started looking into
[Carette, Kisyelov, and Shan's tagless-final EDSL](http://okmij.org/ftp/tagless-final/):
it felt like a nice hack to bootstrap a library of type-safe
yet lightweight program transformers
(e.g., [partial evaluators](https://en.wikipedia.org/wiki/Partial_evaluation))
without having to worry about type checking failures at runtime...
in fact, simple cases shift all the type checking before runtime,
so there's no need to embed any type checker at all.

Combine that with the implicit function in
http://okmij.org/ftp/tagless-final/course/optimizations.html#reassoc,
and it's pretty compelling for simple runtime rewriting.

However, the simple types classically handled by tagless-final
embedding isn't really useful to catch the type of issues I find
interesting in the sort of code I like to play with.  For things like
proving the absence of integer overflow, or safely optimising away
bound checks, [refinement types](https://en.wikipedia.org/wiki/Refinement_type) seem more appropriate... but it's not obvious how to extend
the tagless-final style to also handle refinements: refinement
type systems tend to trade power and regularity in order to ensure
decidability (a pragmatic choice if I ever saw one).  In particular,
higher-order quantified SAT seems really hard (:

It took until 2018 for me to grok a promisingly polymorphic refinement
typing approach: [Cosman and Jhala's Fusion Types](https://arxiv.org/abs/1706.08007).
I eventually realised that the key to that polymorphism approach was
exploiting forall polymorphism to approximate dataflow. For example,
given `compose :: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c`, we know the
only way this can be implemented is as the composition of two
functions  (or error out); i.e., `compose f g x = g(f x)`.  We can
use that information to derive a refinement for `(compose f g)`,
without inlining `compose`.  For example, if we have `f :: {v:int | v >= 0} -> {r: int | r = v + 1}`, and `g :: {v:int | v >= 1} -> {r:int | r = v - 1}`,
Fusion typing would approximate the return value of `f` with
`\exist v >= 0 s.t. r = v + 1` (i.e., `r >= 1`), check that it
satisfies `g`'s precondition, and find a return value for g
that satisfies `\exists v >= 0 s.t. r = v + 1 - 1` (i.e., `r >= 0`).

After wrestling with the paper for a couple months, I realised we
could do better by [paying closer attention to the dataflow](https://github.com/pkhuong/palladium);
in particular, keeping track of when bound values were still in scope
lets us derive a strong refinement, `h = (compose f g) :: {v:int | v >= 0} -> {r:int | r = v}`.

Once a type is polymorphic enough, it only has one (pure) inhabitant,
and so nothing is lost by reconstructing dataflow from types.
However, that approach doesn't work for code that isn't polymorphic
over types, but is polymorphic over refinements (e.g., a `compose`
function that only accepts integer-valued functions).  A dataflow
approach addresses that problem.

Playing with the type system approach also convinced me that abstract
interpretation is a much easier *refinement checking* approach for me:
I think the UX of refinement types clearly hits a useful local optimum,
but there's no need to stick to the type viewpoint when checking them,
especially when we require programmers to annotate toplevel or recursive
definitions, eliminating the need for type synthesis.

A dataflow view of refinement polymorphism
------------------------------------------

Let's say we had an "integer compose" function
`int_compose :: (int -> int) -> (int -> int) -> int -> int`.

From a type perspective, either natural implementation `int_compose f
g x = g(f x)` or `f(g x)` works... as well as many more, like
`int_compose f g x = 0`: a less polymorphic type has many more
inhabitants.

Let's annotate the ground types `int` with some dataflow tags
(I don't know what a good/standard syntax for this looks like):
`int_compose :: ({int @ a} -> {int @ b}) -> ({int @ b} -> {int @ c}) -> {int @ a} -> {int @ c}`.

This says we must pass the third argument (it's the only argument
or funarg return value [... generalise to valence]) tagged with `a`
to the first function (its argument is tagged with `a`), and we
must pass the result of that function call to the second function,
in order to compute the final return value.

Dataflow information is another way to describe subtyping constraints,
and subtyping for refinement types is type equality combined with
predicate implication.

When using this dataflow information to type check he
`int_compose f g x`, we would first check that the types (`int` everywhere)
match, and then that the refinement predicate on `x`
implies the predicate for `f`'s argument... and we can
then decide how hard we want to go when checking `g`'s
argument, depending on purity assumptions, and how much complexity
is reasonable in a type checker.  What's important is that the dataflow
information lets us reduce that check to confirming that the
refinement predicate for `f`'s return value implies that the predicate
for `g`'s argument is always satisfied.  Subtle design choices only
relate to how we handle bound values in the return value's predicate,
and I'm pretty sure that's irrelevant for the 
[tagless-final use case](http://okmij.org/ftp/tagless-final/JFP.pdf#page=17)

What's more interesting is that we can use the same dataflow approach
to typecheck `int_compose` over arbitrary refinements.  Let's say we had
`int:compose :: ({v: int | 'a(v) } -> {w: int | 'b(w)}) -> ({x: int | 'b(v) } -> {y: int | 'c(w)}) -> {z: int | 'a(v)} -> {s: int | 'c(s)}`,
where `'a`, `'b` and `'c` are forall-quantified refinement predicates.

These predicates are arbitrary (they could even always be vacuously
true or false), so the implementation of `int_compose` can't do
anything with them.  However, since they're arbitrary, we also know
it's impossible to construct a value `v` that satisfies, e.g.,
`'a(v)`; we must receive one as an argument or as a return value from
one of our function arguments.  Rather than directly checking
entailment of SMT formulae with higher-order quantification, we can
simply check dataflow conditions: confirm that only values tagged with
`a` flow into arguments tagged with `a`, and similarly for `b` and
`c`.  That's a huge simplification!

For predicates like `v = x + y /\ a(x) /\ b(y)`, we can
peel off the constraints from the toplevel expression, and
convert `a(x)` to `x = a_value_1 \/ a_value_2 ...`, where
these `a_value`s are SMT variables for values that were generated
with an `a` dataflow tag.

This gets dicier for constraints like `(if x a(v) b(v))`, but
we should be able to rephrase that (e.g., `(if x (= v t) (= v u)) /\ (x => a(t)) /\ (-x => b(u))`), and I'm pretty sure we don't need stuff
like that for our partial evaluation use case.

I think we can replace quanified predicates on values flowing into the
function-under-typechecking with explicit dataflow tags, and
macroexpand the same predicates when the FUT generates a value with an
equality check.

What does this mean for final-tagless EDSL?
-------------------------------------------

Let's look at [a simple method from the Symantics module](http://okmij.org/ftp/tagless-final/JFP.pdf#page=17),
`add : ('c,int,int) repr -> ('c,int,int) repr -> ('c,int,int) repr`.

This type is very free-form, and doesn't give us enough "grip" to have
any form of refinement preservation guarantee.  Let's pretend the type
constructor `repr` could also be parameterised over refinement
predicates; we might have

`add : ('c,int,int,{v|a(v)}) repr -> ('c,int,int,{w|b(w)}) repr -> ('c,int,int,{r | \exists x,y s.t. a(x) /\ b(y) /\ r = x + y}) repr`

Now, `repr` is of course free to do what it wants with the predicate,
including discarding it.  However, whatever it does, it has to be
consistent about it since `repr` can't deconstruct or otherwise
examine its predicate argument (just like type arguments).

This means we can typecheck with refinements as though we were using
the metacircular implementation of `Symantics` (e.g., `add x y = x + y`),
and then know that any implementation will preserve that, modulo
whatever its `repr` does to type predicates.

Checking a specific implementation is also tractable.  Let's start with
the type constructor 
`type (’c,’sv,’dv) repr = {st: ’sv option; dy: (’c,’dv) code}` from
the final-tagless partial evaluator.  We might extend that to handle
predicates as
`type (’c,’sv,’dv,p) repr = {st: {v:’sv | p(v)} option; dy: (’c,{v:’dv | p(v)}) code}`.

This yields the following type for `add: {st: {v:’sv | a(v)} option; dy: (’c,{v:’dv | a(v)}) code} -> {st: {w:’sv | b(w)} option; dy: (’c,{w:’dv | b(w)}) code} -> {st: {r:’sv | \exists x,y s.t. a(x) /\ b(y) /\ r = x + y} option; dy: (’c,{r:’dv | \exists x,y s.t. a(x) /\ b(y) /\ r = x + y}) code}`

Let's now try and check each arm of

```
let add e1 e2 = match e1, e2 with
                | {st = Some 0}, e | e, {st = Some 0} -> e
                | {st = Some m}, {st = Some n} -> int (R.add m n)
                | _ -> pdyn (C.add (abstr e1) (abstr e2))
```

The zero-eliminating case `{st = Some 0}, e -> e`
should work: we find `a(0)` and `b(w)` hold, and thus `r = w = w + 0`
satisfies `\exists x,y s.t. a(x) /\ b(y) /\ r = x + y`.

Similarly, `{st = Some m}, {st = Some n} -> int (R.add m n)` is satisfied
with plain dataflow on predicates `a` and `b`.

The catch-all case `_ -> pdyn (C.add (abstr e1) (abstr e2))` works if
the constructor `C.add` is also polymorphic over refinement
predicates.  Alternatively, one could also fully drop the predicates
for residualised expressions... the key is that `repr` must do the
same thing for arguments as for return values, so it's internally
consistent.

Refinement preservation can catch logic bugs in our partial
evaluator(!), and can be checked without having to think about
predicate quantification, or potential scope issues in the predicates.

We keep things maximally polymorphic in specific implementations,
and can check finally encoded programs however we want, assuming
that `('c,'sv,'dv,p) repr = {v:'dv | p(v)}`.

The `lam` function is more complex (as always); we want to do
something like the following,

```
val lam : ((’c,’sa,’da,p) repr -> (’c,’sb,’db,q) repr as ’x)
-> (’c,’x,{v:’da | p(v)} -> {r:’db | q(r)}) repr
```

but that doesn't match the pattern.  Does it make sense to assume the
dynamic value type always has the refinement predicates applied?  A
priori, yes, since `dv` only exists for residual terms, and we want
those to preserve refinements.

We could also explode the parameter list even more and have both `dv`
and `rawdv` (without refinements).

The one key thing for refinement preservation is the way we handle
`if`:

```
val if_ : (’c,bool,bool) repr
          -> (unit -> ’x) -> (unit -> ’x) -> ((’c,’sa,’da) repr as ’x)
```

doesn't guarantee we'll use the correct arm, never mind that the
`then` arm will only be evaluated if the `bool` argument is true.

We could try to impose that the correct arm is returned with

```
val if_ : (’c,bool,bool,{?}) repr
          -> (unit -> ('c,'sa,'da,{p(v)}) repr) -> (unit -> ('c,'sa,'da,{q(v)})) -> (’c,’sa,’da, {(if ??? p(v) q(v))}) repr
```

but it's not clear how to refer to the selector.  Let's first use
dataflow to guaranee that the return value is one of the arms:

```
val if_ : (’c,bool,bool,{?}) repr
          -> (unit -> ('c,'sa,'da,{p(v)}) repr) -> (unit -> ('c,'sa,'da,{q(v)})) -> (’c,’sa,’da, {p(v) \/ q(v)}) repr
```

Now that the only way to generate the return value is clearly o invoke
one of the two arms, let's make sure they're invoked when the selector
has the correct value!

```
val if_ : (’c,bool,bool,{s(v)}) repr
          -> ((’c,bool,bool,{v /\ s(v)}) repr -> ('c,'sa,'da,{p(v)}) repr) -> ((’c,bool,bool,{-v /\ s(v)}) repr -> ('c,'sa,'da,{q(v)})) -> (’c,’sa,’da, {(ite selector p(v) q(v))}) repr
```

This type is satisfied by the constant-folding half of `if_`, and the
residualised version should be fine as as long as the constructor is
polymorphic over refinements... or we can give the option to just drop
refinements, with an additional `rawdv` type.

It's a bit annoying that we can't detect when both arms evaluate to
the same thing: we can't invoke either arm to see what's hiding
underneath it all without a witness that the selector has the correct
value (true or false).

Maybe we can solve that with an intensional equality check for
functions (smth like `if x == y -> Some(x | y)`), but interesting
cases of identical arms will tend to require extensional equality...
(?)

What if we just provide an axiom:

`fold_if : bool -> (bool -> a) -> (bool -> a) -> a option`,

which returns something iff applying the two functions returns the
same thing?

Problem is evaluating the static part could easily rely on a
precondition enforced by the branch, and then it all goes to shit.

DON'T ALLOW THAT! A Symantics implementation may not assume
preconditions hold?  Or must be written in safe subset that
throws on errors (kind of sucks for embedding in C programs)?

However, we can do something with the residual value, which construct
terms. Let's add the axiom that, if the residuals are the same when
passed a residual variable, then we can extract their value... no,
that doesn't work: the unoptimised static part might still be unsafe!

Let's say we add magic to compare functions... we could do something
fun, but only if the body of lambdas are pre-reduced.

... or we pass in a "function eq" non-standard evaluation functor,
which does arbitrary rewriting before spitting back a fingerprint, and
use that as our equality test.  More axioms, but it's not a surprise
that testing expressions for equality is hard.

http://okmij.org/ftp/tagless-final/cookbook.html#simple-staging / http://okmij.org/ftp/meta-programming/let-insertion.pdf has some ideas
to generate typed closures instead of quotations. (http://okmij.org/ftp/meta-programming/calculi.html#metafx)

OK OK, what about moving more logic to the embedding DSL?
---------------------------------------------------------

We want to reduce under the lambda mostly to determine when the
lambda's argument is ultimately irrelevant to its body.

That's something we can do with a variant of the partial evaluator
that can deal with "missing" dynamic values (in which case
missing-ness propagates to the result).

In fact, we could reuse the same PE, with a side condition that
the result is never None when its inputs aren't?

We'd then have `let f = (\x . ...) in lam f (maybe_reduce f)`?

So... this might work, but super-linear complexity, since we traverse
nested lambdas multiple times :\

Potential avenues: see https://link.springer.com/chapter/10.1007/BFb0033849

We could then reduce under lambda and ask if the remaining functions
are identical?

Other problem: generating typed closures has overhead
-----------------------------------------------------

https://link.springer.com/article/10.1023/A:1010058213619 ?

But then https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.48.1653&rep=rep1&type=pdf ?!

This isn't necessarily an issue for runtime performance, since I can
see us fully lowering to, e.g., a Forth for the final performance
sensitive execution.  However, it would be nice to be able to
specialise some code, then pass it around some more for further
specialisation.  This doesn't work as well if the result of
specialisation (into code) adds linear overhead for each pass.

That overhead stems from the need to flatten thunks `() -> value`,
e.g., `\_ -> (a ()) + (b ())` instead of `a + b`.

Would that be fixed by non-strict evaluation under lambda?

Would call-by-need suffice? 

Evaluation under lambda seems promising, but I think we can make this
less magical by extending lambdas with a special operator that gives us
the value if the argument is irrelevant.

In other words, we'd have `remove_binding: ('a -> 'b) -> 'b option`,
as an axiom.  That'd be enough to detect when an `if` can be converted
to a `select`; unclear if we can also detect two equivalent but
residualised arms, or even if that's a useful transform.

Again, we run into performance issues if we implement the
`remove_binding` operator without integrating it into the global
evaluation strategy.  We might instead look into 
`remove_binding: ('a -> 'b) -> 'b option * ('a -> 'b)`,
where the second return value is equivalent to the argument function,
but with some stuff pre-reduced?

This means we assume purity and non-strict evaluation, which also
obviates the reason for passing a witness of the selector bool's
value.

Non-strict evaluation means we can't rely on refinement predicates in
the embedded language for the safety of static evaluation... probably
a good idea anyway: we don't *have* to constant fold operators, and
those that are folded should probably be done in an obviously safe
manner (:

https://web.archive.org/web/20201111222712/http://thyer.name/phd-thesis/thesis-thyer.pdf ??

See Augustsson's `small` language.

Thyer claims victory with slowdown linear in the tower's height
(rather than geometric)... and we already have that with regular final
encodings to typed closures.

So...

1. call-by-need with some aggressive re-merging of the expression graph?
2. use de bruijn indices and tag subgraphs with variables that are used?
   See Xi's evaluation under lambda; we only need one depth number?

Take a step back
----------------

We simply want to evaluate to a static value when we don't know the
static argument.  Instead of a static value of type `'a -> 'b`, we
simply need `'a option -> 'b option`!

Should we extend the `repr` type constructor to also take a `repr
option -> repr option` type?  This additional type represents the
option to evaluate under a lambda without knowing the argument's
value.

In fact, this is all we need for our direct-style PE use case, but a
CPS transformation might want the non-option case.  Another angle is
to add more complexity with an "arrow" type constructor.  I think we
can type check that by macro expansion, while abstracting with
polymorphic types and refinement predicates?

OK, so we just have to switch to de bruijn indexes http://okmij.org/ftp/tagless-final/course/PE-dB.html ??

Let's make sure we can make that work with refinement preservation??

OR OR OR... we can make the environment bind to a special Option type,
where instances can't be constructed directly, and thus None is only
accessible if we found a None binding.  If all bound values are
populated, then we can't get a "None" value out... ?



