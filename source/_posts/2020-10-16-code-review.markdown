---
layout: post
title: Code review, sensemaking, and what even is an abstraction?
date: 2020-10-16 13:36:16 -0400
comments: false
hidden: true
draft: true
categories: 
---

When I review code, my main objective is to make sense not of the
system we actually obtain when running that code, but of the system
the code is trying to describe.  This
[sensemaking](https://en.wikipedia.org/wiki/Sensemaking_(information_science))
process can be slow; it's essentially unscalable in the worst case,
and responds non-linearly to some small changes in the source.
However, once constructed, the result--something like a theory of
operations for the software system in question--lets me quickly plan
and evaluate changes.

When faced with a large piece of production code that is difficult for
me to make sense of, I can only ask for documentation, which I'll
usually find lacking, and start interfacing with that code in an
inefficient manner.  Over time, I'll either work so little with that
code that it won't matter, or I'll create sense where I couldn't find
it: that's the main driver for refactors on my end.

I don't expect much out of documentation for running software that's
hard to make sense of because it's difficult to fully write down
complex and inconsistent rules; at best, the result is often
descriptive, but not usefully prescriptive.  Unless there are dodgy
power dynamics between coding and documenting, an attempt to document
code that's hard to document would result in first making the code
easier to document, thus making it easier to understand, and then in
documentation for that modified codebase.

This also gives us a good rule of thumb for avoiding "personal
preference" refactors: does the refactor come with documentation that
better describes the code (the system described by the code)? In
particular, can the documentation help me determine more quickly how
to implement a given change, or whether a certain implementation is
certainly incorrect?  If that's a no, the diff has better be a trivial
change.

More frequently, I will review patches to modify pre-existing code
(assuming a "make the change easy to make, then make the change"
workflow).  These reviews are easier because I don't have to make
sense of the code, I simply have to convince myself that the changes
are "sense-neutral," except for what the commit message or cover
letter says.

The real issue comes with patches that add new modules, functions, or
larger regions of code that I now have to make sense of.  I could ask
for documentation, but no one writes documentation detailed enough,
and we mostly don't have the vocabulary to efficiently do that anyway.
One exception is resource management: computer programmers do share
words and *concepts* to describe common patterns (e.g., ownership,
reference count, garbage collection, destructor, cleanup label, ...)
and classes of bugs (e.g., leak, double-free, use after free, reclaim
race, goto fail). Thread-safety and shared memory concurrency is
another exception: we have enough vocabulary to describe a few useful
ways to structure systems, and programmers accept that usage patterns
that are hard to describe are probably bug-prone and should be avoided
altogether. This may in fact be an argument for microservices:
programmers seem to be cultivating a shared understanding of SRE-level
concerns in networked services.[^style-guide]

[^style-guide]: In a way, I found Google's C++ style guide helps make it easier to describe more properties than just resource management.  Some of it the addition of more vocabulary (e.g., thread-hostile and thread-compatible), but mostly because we have one recommended way to do $X, so we don't have to document how we do $X.  Rather than trying to make sense of code that does something like $X in a bespoke manner, I, as a reviewer, can ask to do $X how the style guide mandates it be done.

So I instead have to extract an internally consistent theory of
operations (ToP) out of the new code, see that the code matches the
ToP, and then try to evaluate whether the ToP is something we want.

A major failure mode here is when a patch includes both modifications
to pre-existing code and new functionality in the same commit: it
means I have to engage the heavier sense-making process for
everything, instead of fast-pathing most changes to pre-existing code
to my internal "sense-neutrality-modulo-commit-message" checker.

When I see a patch like that, I tend to reject it purely on the
technical reason that it should have been split in more diffs.

How my sensemaking goes wrong
-----------------------------

I like to use resource management in hypotheticals, even though
resource management is usually easy to handle: the rich vocabulary we
share for resource management makes it easy to setup clear
hypothetical situations.

### Inference for resource management rules

Let's look at a few very similar patches that add a pointer (a
resource) field to a struct (class), and directly releases that
pointer.

    foo.h:
    struct foo {
        ...
        ...
        char *buf;  /* <- new field added here. */
    };


    bar.c:
    existing_function_a()
    {
        struct foo tmp;
        ...
        tmp.buf = ...;  /* <- new statement */
        ...
        free(tmp.buf);  /* <- new statement */
        ...
    }
    
I would tend to accept such a diff with a request to better document
that `buf` isn't owned by `struct foo`.  If it were `const char *buf;`,
I wouldn't even ask for that.

Here's another slighly more complex case, with the same change to
`struct foo` in `foo.h`.

    baz.c:
    existing_function_b(struct foo *foo)
    {
        ...
        foo->buf = ...;  /* <- new statement */
        ...
    }
    
    existing_function_c(struct foo *foo)
    {
        ...
        free(foo->buf);  /* <- new statement */
        ...
    }
    
Here I would definitely ask for more comments *in `struct foo`* on how
the `buf` field is initialised, and how it should be managed.  It's
very important for me that the memory management discipline for
`foo::buf` be described in `struct foo` because that's how I can scale
my understanding of the code: I'll read the definition of `struct
foo`, along with the comments on how it should be used, then look at
users of `struct foo` to make sure they match the definition and
expected discipline.  This process may then lead me to ask for more
documentation on how `existing_funcion_b` and `existing_funcion_c`
are used to satisfy the usage discipline for `foo::buf`.

One last example, which would lead me to immediately ask for changes
and stop reviewing, because it's likely a waste of time.

    quux.c:
    existing_function_d(struct foo *foo)
    {
        ...
        free(foo->buf);  /* <- new statement */
        foo_destroy(foo);  /* <- pre-existing statement */
        ..
    }

In this case, I can't really make sense of the code as it is: it's too
ambiguous whether `foo::buf` is an arbitrary field that users for
users to stash their opaque pointer (in which case, `void *buf;` may
be more appropriate), or if it's actually owned by `foo` and should be
freed in `foo_destroy`.  I would probably assume the latter and
suggest that the `free` call be moved to `foo_destroy` and to document
the ownership... but any further review of the code will be cursory,
because all I really know is that I don't know what the code is trying
to do.

I can see how the range of responses can be confusing and frustrating
from the outside: the hypothetical patches are of the same complexity
and describe very similar code, yet the first is quickly rubber
stamped, while the last fully interrupts the review process with a
barrage of questions.

The reason for this non-linear response is that the comments on a
review are the result of an internal sensemaking repair and inference
process.  When I offer concrete suggestions for the code, that's
because the process succeeded.  When I instead have a lot of
questions, the process stalled, and I don't have suggestions (yet).
That's usually fixed by changing the code to make it clearly
inferrable for me, since we lack a shared vocabulary to efficiently
discuss domain-specific abstractions, preconditions, invariants... and
the process of making the code transparent to my mental analyses also
tends to highlight what I would have flagged during a review.

I suppose it would be less surprising to consistently ask for all the
documentation I'm usually able to infer, but I think that would be
consistently unproductive.

### The difference between what and how

In the resource management examples above, the first thing I ask for
tends to be documentation, not code changes.  That's because I can't
know if the code should be changed until I know what it wants to be.

For resource management or locking, documentation is easy enough:
programmers share a common vocabulary to describe usage patterns, so
people naturally describe a discipline from which I can infer
invariants, preconditions, etc.  That's not the case for pretty much
anything else.

Common trigger words for me are "context," "info," "process" (as a
verb), and "misc" (as a file or module name).  They tend to accompany
bags of data or functions that are described by how they currently
are, and not what they're trying to be.  Sometimes, we also get a
description of how they're used, which still isn't enough.

The common "context" antipattern goes as follows.  We create a new
struct/tuple/object type, and we stick values in there.  Usually, each
field is well described, but the struct itself isn't.  For example, if
we populate the struct with database queries, we might have a
`DbContext`.  That tells me how it's populated, not *why*, and
certainly doesn't help me determine whether a new value I also happen
to extract from the database should go in that `DbContext`, or
somewhere else.

It usually helps to look at the users.  If we only pass `DbContext` to
`$Verb$Noun`, then maybe it's really a `$VerbInfo`.  Of course, this
heuristic can fail when a project is young, and there is only one
using function because the rest haven't been created yet.

This tends to lead to frustrating conversions that would go something
like in a kitchen setting:

PK: What does this do?

X: It cracks two eggs, beats them, and pours the mixture in a pan

PK: That's just a prose reformulation of the code, what does this actually do?

X: ... cracks two eggs, beats them, and pours the mixture in a pan!

PK: OK, let's try another angle. How is this function used?

X: Easy peasy. It's called every morning at 8 AM, except Sundays.

PK: Let me rephrase: when does it make sense to call this function?

X: Call this whenever you're hungry.

PK: Oh I see. This is the breakfast routine? Let's say I develop a
    craving for blueberries.  Where in this function do I insert
    "sprinkle a generous handful of blueberries?"
    
X: Oh, no no no. This only works if you're making an omelet.

PK: Ah. So the answer to "what does this do?" is "it makes an omelet,"
    and we should only call it with a pre-heated pan.

That's important because we can now see that while a three egg omelet
option can be accomodated, my blueberry muffin ask should probably
live somewhere else.  We also improved the naming, and found a
pre-condition to document explicitly.

### Why I care so much about the [rule of three](https://wiki.c2.com/?RuleOfThree)

The difference between "what" (how) something does and "what" (why)
something does is nowhere near as apparent as in generic code.

In generic code, the answer to "what" (how) is essentially "anything,"
or "whatever you put in that method or callback."

And sometimes, when you're writing Haskell, that's really what you
mean.

Usually, though, we have more assumptions (c.f. monad laws), and
spelling them out makes it easier to understand what the generic code
should do and what its specific instantiations mean (what as in why,
not how).

What I'm looking for is clear documentation on the discipline for the
concepts introduced by the new generic code.  However, getting that
out of programmers is like pulling teeth: I'll find documentation that
tells me things like "pass in callbacks foo and bar; foo is called
when X happens, bar is called Y times a second."  This merely restates
the code, and thus fails at actually creating an abstraction.  While
the generic may save characters, it doesn't help the sensemaking
process; whenever we encounter an instantiation of that generic code,
we have to monomorphise the code and analyse it inline, in its
context, and that's not scalable.

A good abstraction is the opposite: knowing what each component is
supposed to do, we can check that, e.g., each function argument
matches the contract, and apply something like a program algebra to
quickly summarise the meaning of the instantiation.  One example of
such an abstraction is MapReduce over monoids.  Faced with an
instantiation of the pattern, we can ask what the input and output
monoids are, and check that the reduction function is associative.
After that, we know at a high level what the MapReduce instance
computes, without having to look at the specific implementation of the
generic MapReduce driver.  In the other direction, we can also check
that an implementation satisfies its contract without having to look
at every instantiation.

I believe that's the main reason we try to convince people to use well
known abstractions instead of creating new ones: it's not that new
ones are hard to understand, but that we expect they will be badly
documented.  Bad documentation for a known abstraction (e.g.,
MapReduce) isn't as much of a problem, since "what does it do?" is
already common knowledge.

If we must use a bespoke abstraction, and we find the documentation is
lacking, we're kind of stuck: the answer to "what is this function
argument/extension point" is "anything," and documenting by restating
the generic code defeats the purpose of abstraction.

That's why I like to wait for three potential users before creating a
new generic abstraction.  If we have multiple users, we can try to
improve the documentation by looking at what the function arguments or
extension points (methods) actually do in practice.  Having multiple
users means we can more easily look for commonalities, and try to
figure out what's accidental and what's essential.

The c2 wiki will tell us we should wait for multiple concrete
instantiations because we often find that interfaces don't split
naturally where we expect them to.  That's often true, but sometimes
the interface is obvious.  I find it's more important to delay
abstraction because having multiple users gives us access to a more
effective heuristic (look at n >= 3 instances) to come up with
descriptive names and to document what the new abstraction means.

It's only then that abstraction is useful.  In the words of Dijkstra,
"The purpose of abstraction is not to be vague, but to create a new
semantic level in which one can be absolutely precise."

My beef with "low level code" isn't that it's too detailed, but that
it lacks information in practice: Programmers, having spelled out
so many domain-independent (-irrelevant) details, think they've
specified how the system defined by the code behaves, when in reality
we only have a description of how it does whatever it is that it does.

<p><hr style="width: 50%"></p>
