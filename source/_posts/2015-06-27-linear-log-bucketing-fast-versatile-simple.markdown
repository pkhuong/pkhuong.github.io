---
layout: post
title: "Linear-log bucketing: fast, versatile, simple"
date: 2015-06-27 17:05:00 -0400
comments: true
categories: 
---

_There's a couple code snippets in this post (lb.lisp, bucket.lisp, bucket-down.lisp, bin.c).  They're all [CC0](https://creativecommons.org/publicdomain/zero/1.0/)._

What do memory allocation, histograms, and event scheduling have in
common?  They all benefit from rounding values to predetermined
buckets, and the same bucketing strategy combines acceptable precision
with reasonable space usage for a wide range of values.  I don't know
if it has a real name; I had to come up with the (confusing) term
"linear-log bucketing" for this post!  I also used it twice last
week, in otherwise unrelated contexts, so I figure it deserves more
publicity.

I'm sure the idea is old, but I first came across this strategy in
[jemalloc](https://github.com/jemalloc/jemalloc)'s binning scheme for
allocation sizes.  The basic idea is to simplify allocation and reduce
external fragmentation by rounding allocations up to one of a few bin
sizes.  The simplest scheme would round up to the next power of two,
but experience shows that's extremely wasteful: in the worst case, an
allocation for \\(k\\) bytes can be rounded up to \\(2k - 2\\) bytes,
for almost 100% space overhead!  Jemalloc further divides each
power-of-two range into 4 bins, reducing the worst-case space overhead
to 25%.

This sub-power-of-two binning covers medium or large allocations.  We
still have to deal with small ones: the ABI forces alignment on every
allocation, regardless of their size, and we don't want to have too
many small bins (e.g., 1 byte, 2 bytes, 3 bytes, ..., 8 bytes).
Jemalloc adds another constraint: bins are always multiples of the
allocation quantum (usually 16 bytes).

The sequence for bin sizes thus looks like: 16, 32, 48, 64, 80, 96,
128, 150, 182, 256, 320, 384, ... (0 is special because malloc must
either return NULL [bad for error checking] or treat it as a full
blown allocation).

I like to think of this sequence as a special initial range with 4
linearly spaced subbins (0 to 63), followed by power-of-two ranges
that are again split in 4 subbins (i.e., almost logarithmic binning).
There are thus two parameters: the size of the initial linear range,
and the number of subbins per range.

Assuming both parameters are powers of two, we can find the bucket for
any value with only a couple x86 instructions, and no conditional jump or
lookup in memory.  That's a lot simpler than jemalloc's
implementation; if you're into Java,
[HdrHistogram](http://hdrhistogram.org/)'s binning code is nearly
identical to mine.

Common Lisp: my favourite programmer's calculator
-------------------------------------------------

As always when working with bits, I first doodled in SLIME/SBCL: CL's
bit manipulation functions are more expressive than C's, and a
REPL always helps exploration.

Let `linear` be the \\(\log\sb{2}\\) of the linear range, and `subbin`
the \\(\log\sb{2}\\) of the number of subbin per range, with 
`linear >= subbin`.

The key idea is that we can easily find the power of two range (with a
`BSR`), and that we can determine the subbin in that range by shifting
the value right to only keep its `subbin` most significant (nonzero)
bits.

I clearly need something like \\(\lfloor\log\sb{2} x\rfloor\\):

{% codeblock "lb.lisp" %}
(defun lb (x)
  (1- (integer-length x)))
{% endcodeblock %}

I'll also want to treat values smaller than `2**linear` mostly as
though they roughly `2**linear` in size.  We'll do that with

    n-bits := (lb (logior x (ash 1 linear))) === (max linear (lb x))

We now want to shift away all but the top `subbin` bits of `x`

    shift := (- n-bits subbin)
    sub-index := (ash x (- shift))

For a memory allocation, the problem is that the last rightward shift
rounds *down*!  Let's add a small mask to round things up:

    mask := (ldb (byte shift 0) -1) ; that's `shift` 1 bits
    rounded := (+ x mask)
    sub-index := (ash rounded (- shift))

We have the top `subbin` bits (after rounding) in `sub-index`.  We
only need to find the range index

    range := (- n-bits linear) ; n-bits >= linear

Finally, we combine these two together by shifting `index` by
`subbin` bits

    index := (+ (ash range subbin) sub-index)

Extra Extra! we can also find the maximum value for the bin with

    size := (logandc2 rounded mask)

Assembling all this yields

{% codeblock "bucket.lisp" %}
(defun bucket (x linear subbin)
  (let* ((n-bits (lb (logior x (ash 1 linear))))
         (shift (- n-bits subbin))
         (mask (ldb (byte shift 0) -1))
         (rounded (+ x mask))
         (sub-index (ash rounded (- shift)))
         (range (- n-bits linear))
         (index (+ (ash range subbin) sub-index))
         (size (logandc2 rounded mask)))
    (values index size)))
{% endcodeblock %}

Let's look at what happens when we want \\(2\sp{2} = 4\\) subbin per
range, and a linear progression over \\([0, 2\sp{4} = 16)\\).

    CL-USER> (bucket 0 4 2)
    0 ; 0 gets bucket 0 and rounds up to 0
    0
    CL-USER> (bucket 1 4 2)
    1 ; 1 gets bucket 1 and rounds up to 4
    4
    CL-USER> (bucket 4 4 2)
    1 ; so does 4
    4
    CL-USER> (bucket 5 4 2)
    2 ; 5 gets the next bucket
    8
    CL-USER> (bucket 9 4 2)
    3
    12
    CL-USER> (bucket 15 4 2)
    4
    16
    CL-USER> (bucket 17 4 2)
    5
    20
    CL-USER> (bucket 34 4 2)
    9
    40

The sequence is exactly what we want: 0, 4, 8, 12, 16, 20, 24, 28, 32, 40, 48, ...!

The function is marginally simpler if we can round down instead of up.

{% codeblock "bucket-down.lisp" %}
(defun bucket-down (x linear subbin)
  (let* ((n-bits (lb (logior x (ash 1 linear))))
         (shift (- n-bits subbin))
         (sub-index (ash x (- shift)))
         (range (- n-bits linear))
         (index (+ (ash range subbin) sub-index))
         (size (ash sub-index shift)))
     (values index size)))
{% endcodeblock %}

    CL-USER> (bucket-down 0 4 2)
    0 ; 0 still gets the 0th bucket 
    0 ; and rounds down to 0
    CL-USER> (bucket-down 1 4 2)
    0 ; but now so does 1
    0
    CL-USER> (bucket-down 3 4 2)
    0 ; and 3
    0
    CL-USER> (bucket-down 4 4 2)
    1 ; 4 gets its bucket
    4
    CL-USER> (bucket-down 7 4 2)
    1 ; and 7 shares it
    4
    CL-USER> (bucket-down 15 4 2)
    3 ; 15 gets the 3rd bucket for [12, 15]
    12
    CL-USER> (bucket-down 16 4 2)
    4
    16
    CL-USER> (bucket-down 17 4 2)
    4
    16
    CL-USER> (bucket-down 34 4 2)
    8
    32

That's the same sequence of bucket sizes, but we round down in size
instead of up.

The same, in GCC
----------------

{% codeblock "bin.c" %}
static inline unsigned
lb(size_t x)
{
        /* I need an extension just for integer-length (: */
        return (sizeof(long long) * CHAR_BIT - 1) - __builtin_clzll(x);
}

/*
 * The following isn't exactly copy/pasted, so there might be
 * transcription bugs.
 */
static inline size_t
bin_of(size_t size, size_t *rounded_size,
    unsigned int linear, unsigned int subbin)
{
        size_t mask, range, rounded, sub_index;
        unsigned int n_bits, shift;

        n_bits = lb(size | (1ULL << linear));
        shift = n_bits - subbin;
        mask = (1ULL << shift) - 1;
        rounded = size + mask; /* XXX: overflow. */
        sub_index = rounded >> shift;
        range = n_bits - linear;

        *rounded_size = rounded & ~mask;
        return (range << subbin) + sub_index;
}

static inline size_t
bin_down_of(size_t size, size_t *rounded_size,
    unsigned int linear, unsigned int subbin)
{
        size_t range, sub_index;
        unsigned int n_bits, shift;

        n_bits = lb(size | (1ULL << linear));
        shift = n_bits - subbin;
        sub_index = size >> shift;
        range = n_bits - linear;

        *rounded_size = sub_index << shift;
        return (range << subbin) + sub_index;
}
{% endcodeblock %}

What's it good for?
-------------------

I first implementated this code to mimic's jemalloc binning scheme: in
a memory allocator, a linear-logarithmic sequence gives us minimal
alignment and bounded space overhead (bounded internal fragmentation),
while keeping the number of size classes down (controls external
fragmentation).

[High dynamic range histograms](http://hdrhistogram.org/) use the same
class of sequences to bound the relative error introduced by binning,
even when recording latencies that vary between microseconds and
hours.

I'm currently considering this binning strategy to handle a large
number of timeout events, when an exact priority queue is overkill.  A
timer wheel would work, but tuning memory usage is annoying.  Instead
of going for a hashed or hierarchical timer wheel, I'm thinking of
binning events by timeout, with one FIFO per bin: events may
be late, but never by more than, e.g., 10% their timeout.  I also
don't really care about sub millisecond precision, but wish to treat
zero specially; that's all taken care of by the "round up" linear-log
binning code.

In general, if you think that dispatching on the bitwidth of a number
would mostly work, except that you need more granularity for large
values, and perhaps less for small ones, linear-logarithmic binning
sequences may be useful.  They let you tune the granularity at both
ends, and we know how to round values and map them to bins with simple
functions that compile to fast and compact code!
