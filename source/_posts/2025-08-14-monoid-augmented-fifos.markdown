---
layout: post
title: "Monoid-augmented FIFOs, deamortised"
date: 2025-08-14 17:05:22 -0400
published: true
comments: true
draft: true
hidden: true
categories:
---

Augmented FIFOs come up frequently in streaming analytics.
For example, to compute the sum of the last \\(k\\) values observed in a stream
(or more generally, in the [turnstile model](https://en.wikipedia.org/wiki/Streaming_algorithm#Turnstile_and_cash_register_models)),
we can increment an accumulator by the entering value's value when it is observed (it's pushed onto the FIFO),
and decrement the accumulator by the value's value (increment by the value's additive inverse) when it's popped off the FIFO.

This simple increment/decrement the accumulator structure works because the underlying algebra is a group
(addition is associative, and we have inverses).
Unfortunately, a lot of time, we want windowed aggregates with operators that don't have an inverse (e.g., windowed min or max),
but are still associative; we want to work with monoids.[^semigroup]

[^semigroup]: Keeping only associativity yields a semigroup, but we can trivially upgrade a semigroup to a monoid with a sentinel 0 value (e.g., `Option<T>` instead of `T`).

There's a cute and simple construction in the purely functional data structure folklore for a FIFO queue augmented with a monoid.
The construction builds on two observations:

1. It's trivial to augment a *stack* with a monoid such that we can always get the product of all the values in the stack (multiply the previous product with the new value when pushing, keep a pointer to the previous stack; pop simply dereferences that pointer).
2. We can construct an amortised queue from a pair of stacks, an ingestion stack and an excretion stack (popping from stack A and pushing onto stack B ends up reversing the contents of A on top of B).

Unfortunately, we hit a wall when we try to deamortise: it's clear that we want to add some sort of work area while keeping the number of stacks bounded, but what should we do when the work area has been fully reversed before the earlier excretion stack is empty?
That leads to a clearly wasteful mess of copies, redundant push/pop, and general bookkeeping overhead.

Earlier this week, [Shachaf](https://gts.y.la/@shachaf/statuses/01K287S10263ASXE5H97DZ2T8N) linked to an [IBM research report, "Constant-Time Sliding Window Aggregation,"](https://hirzels.com/martin/papers/tr15-rc25574-daba.pdf) that describes DABA, a simple deamortised algorithm.
The key insight: despite (?) its cleverness, the dual-stack construction is an intellectual dead end.
Unfortunately, I found the paper a bit confusing (I just found this [follow-up, which might be clearer](https://arxiv.org/abs/2009.13768)).
I hope the alternative presentation in this post is helpful to others.

Rethinking the amortised augmented FIFO
---------------------------------------

In [the DABA paper](https://hirzels.com/martin/papers/tr15-rc25574-daba.pdf), we actually want to think of the dual stack data structure as a pair of:
1. an ingestion list that also computes a running product of its contents (in the [cash register model](https://en.wikipedia.org/wiki/Streaming_algorithm#Turnstile_and_cash_register_models))
2. an excretion list with a precomputed suffix product (in fact, as [the same authors' follow-up](https://arxiv.org/abs/2009.13768) points out, we *only* need that suffix product)

Concretely, all new values enter the ingestion FIFO and update a running product of the ingestion FIFO's contents.
We pop from a separate excretion list; that list holds pairs of value and the suffix product of the current value and all younger values (values that will be popped later) in the excretion list.

This approach is illustrated by the ASCII diagram below.
The windowed product for `a ... w` is the product of the suffix product at the head of the excretion list (`a*b*c*...*g*h`) and the running product of the ingestion list (`i*j*k*...*w`), `a*b*c*...*g*h*i*j*k*...*w`.
Pushing a new value `x` on the FIFO appends to the ingestion list and updates the running product to `i*j*k*...*w*x`.
Popping from the FIFO pops the first value from the excretion list (`a`), and leaves a new windowed product `(b*c*...*g*h)*(i*j*k*...*w)`.

```
   .----- excretion -----.      .--- ingestion ---.
  /                       \    /                   \
 [ a   b    c  ...  g    h ]  [ i j k ...          w ]
 [ a   b    c       g    h ]    product: i*j*k*...*w
   *   *    *       *
   b   c   ...      h
   *   *    *
   c  ...   g
   *   *    *
  ...  g    h
   *   *
   g   h
   *
   h
```

Towards deamortisation
----------------------

Thinking in terms of ingestion/excretion lists is helpful because
it's now trivial to append any prefix of the ingestion list to the excretion list at any time,
even when the excretion list is non-empty:
concatenate the two lists, and recompute the suffix product for the resulting excretion list
(the 2020 follow-up notes that we can do that for the old excretion list without keeping the raw values around:
we only have to multiply the old excretion list's suffix product with the product of all newly appended excretion values).

```
[ a   b   c ] + [ d   e   f ]
[ a   b   c ]
  *   *
  b   c
  *
  c
```

Turns into

```
[ a   b   c   d   e   f ]
[ a   b   c   d   e   f ]
  *   *   *   *   *
  b   c   d   e   f
  *   *   *   *
  c   d   e   f
  *   *   *
  d   e   f
  *   *
  e   f
  *
  f
```

Where, for example, `b * c * d * e = (b * c) * (d * e)`,
the product of the *previous* suffix product at `b` (`b * c`),
and the total product for the newly appended values (`d * e`).

The interesting part for deamortisation is figuring out what invariants hold in the middle of recomputing the suffix product.

Let's call the newly appended values `[d e f]` the staging list.

Early during the suffix product update, we have

```
          write cursor (moves left)
              v
[ a   b   c   d   e   f ]  staging product: d*e*f
[ a   b   c       e   f ]
  *   *           *
  b   c           f
  *
  c
```

While the write cursor is in the staging list,
values in the staging list to the left of the write cursor have a garbage suffix product,
and those *to the right* of the write cursor have a suffix product equal to the product of all that value and all values to their right (within the excretion list).
At the same time, values in the old excretion list have a suffix product that considers only the old excretion list.
Fortunately, that's easy to fixup in constant time: just multiply the old suffix product with the product of values in the staging list (then the product for the ingestion list).

Things are a bit subtle later in the suffix product update, once the write cursor is deeper in the old excretion list:

```
    write cursor (moves left)
      v
[ a   b   c   d   e   f ]
[ a   b   c   d   e   f ]
  *   *   *   *   *
  b   c   d   e   f
  *       *   *
  c       e   f
          *
          f
```

Now that the write cursor is in the old excretion list, values to the left of the write cursor have a suffix product that's correct for the new excretion list,
while other values (to the right or at the write cursor) have a suffix product that considers only the old excretion list (and must thus be fixed up to include
the staging product).

This is interesting for deamortisation because we now have useful invariants at all stages of the suffix product recomputation,
even while we're updating the old excretion list.
That in turn is useful because we can now update the old excretion list incrementally until the suffix product has been fully recomputed;
at that time, we're back to a single excretion list and no staging list, and are ready to accept the ingestion list as the new staging list.

The only question left for deamortisation is scheduling: when to perform incremental suffix product updates and when to turn the ingestion list into a staging list.

Scheduling for constant work
----------------------------

We're looking for constant work (suffix product updates) per operation (push and pop)
without ever getting in a situation where we'd like to pop a value, but the suffix product's write cursor is still in the staging list (i.e., we still have garbage suffix product values).

It's easy to guarantee we'll never pop a value and find the write cursor is still in the staging list:
advance the write cursor by \\( \left\lceil \frac{\\# \texttt{staging}}{ \\# \texttt{old_excretion}} \right\rceil \\) values for each pop.

Obviously, the goal is to minimise the size of the staging list in order to guarantee, e.g., \\( \\# \texttt{staging} \leq \\# \texttt{old_excretion}. \\)
We should thus promote the whole ingestion list to staging as soon as the suffix product is fully computed
(once the write cursor all the way past the oldest value in the excretion list).

We want to keep the staging to old excretion (ingestion to excretion) ratio to at most 1:1,
so we must advance the suffix sum by at least one value whenever we push a new value to the ingestion list.
This guarantees that, by the time the suffix sum is fully recomputed, the ingestion list is never bigger than the new excretion list.

Now that we have a bound on the staging-to-old-excretion ratio, we can also advance the suffix sum by one value whenever we pop a value.

Sample code
-----------

I [implemented the data structure in Python](/images/2025-08-14-monoid-augmented-fifos/monoid-fifo.py) with the improvement from the [follow-up paper](https://arxiv.org/abs/2009.13768),
where we store only a value *or* a suffix product for each slot in the FIFO.

The state is just a bunch of indices in an arbitrary windowed store (e.g., a ring buffer).

```
class MonoidFifo:
    def __init__(self, combiner, zero, trace=False):
        self.combiner = combiner
        self.zero = zero
        self.trace = trace
        self.store = dict()  # int -> value or suffix product

        # values in [pop_index:push_index)
        self.pop_idx = 0
        self.push_idx = 0
        # write cursor goes down toward pop_idx (write_cursor >= pop_idx),
        # and the suffix product is up to date *at* write_cursor inclusively.
        self.write_cursor = 0

        # staging list in [first_staging_idx:first_ingestion_idx)
        self.first_staging_idx = 0
        self.staging_product = zero # product for the staging list

        # ingestion list in [first_ingestion_idx:push_index)
        self.first_ingestion_idx = 0
        self.ingestion_product = zero # running product for the ingestion list
```

We can `push` by pushing to the underlying windowed store,
updating our state to take the new value into acount,
and calling the `maintain` method.

```
    def push(self, value):
        assert self.push_idx not in self.store
        self.store[self.push_idx] = value
        self.push_idx += 1
        self.ingestion_product = self.combiner(self.ingestion_product, value)
        self.maintain()
```

The `peek` method shows how we reassemble up to 3 partial products,
depending on where the pop index lives (before or after the write cursor).

```
        if self.pop_idx == self.push_idx:
            return self.zero
        ret = self.store[self.pop_idx]
        if self.pop_idx < self.write_cursor:
            ret = self.combiner(ret, self.staging_product)
        ret = self.combiner(ret, self.ingestion_product)
        return ret
```

Finally, we can `pop` by updating the windowed store,
advancing our `pop_idx`, and calling the `maintain` method.

```
    def pop(self):
        ret = self.peek()
        del self.store[self.pop_idx]
        self.pop_idx += 1
        self.maintain()
        return ret
```

Now the `maintain` method itself, where all the complexity hides:

1. advances the suffix product (with one call to the `combiner`) if `write_cursor > pop_idx`
2. promotes the ingestion list to staging list when the suffix product is fully computed (`write_cursor <= pop_idx`)

Each `push` or `pop` call thus makes exactly one call to the `maintain` method,
and the `maintain` method itself makes at most one call to the monoid operator (`combiner`).

```
    def maintain(self):
        if self.write_cursor > self.pop_idx:
            self.advance()
        if self.write_cursor <= self.pop_idx:
            self.promote()

    def advance(self):
        self.write_cursor -= 1
        curr = self.store[self.write_cursor]
        if self.write_cursor < self.first_staging_idx:
            # outside the staging list, we update the precomputed suffix product
            update = self.combiner(curr, self.staging_product)
        else:
            # in the staging list, we compute a regular suffix product
            update = self.combiner(curr, self.store[self.write_cursor + 1])
        self.store[self.write_cursor] = update

    def promote(self):
        # pop_idx ... first_staging_idx ... first_ingestion_idx ... push_idx
        #                                  write_cursor
        if self.first_ingestion_idx == self.push_idx:  # optional, but might as well
            assert self.ingestion_product == self.zero
            return
        self.staging_product = self.ingestion_product
        self.ingestion_product = self.zero
        self.first_staging_idx = self.first_ingestion_idx

        self.write_cursor = self.push_idx - 1 # one free combine with zero
        self.first_ingestion_idx = self.push_idx
```

This is pretty complex, so I tested the code by exhaustively enumerating
all small push/pop sequences for the free (list append) monoid; see
[the bottom of the implementation file](/images/2025-08-14-monoid-augmented-fifos/monoid-fifo.py).

It seems to work (and manually mutating the implementation did flag all the changes I tried),
and I even think it would be possible to implement this so every operation is constant time (modulo memory caches)!

<p><hr style="width: 50%"></p>
