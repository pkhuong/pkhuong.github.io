---
layout: post
title: "Optimal overhead byte stuffing"
date: 2018-08-04 13:42:44 -0400
draft: true
hidden: true
comments: true
categories: 
---

Earlier this year, I started looking into 
[Duda's Asymmetric numeral systems](https://arxiv.org/abs/1311.2540)
([rANS](https://fgiesen.wordpress.com/2014/02/02/rans-notes/), in
particular) to compress short blocks with domain-specific probability
models. The
[consensus on \(r\)ANS for short blocks is that it's a bad idea](https://twitter.com/rygorous/status/974353149518475264).
Short blocks don't work well with rANS because rANS's space usage only
correlates well with entropy once its internal state is large
enough. The classic solution is to initialise the internal state to
always be in that regime, but that's also more bits for the encoder
to output. Charles Bloom suggests we
[set the initial state to 1](http://cbloomrants.blogspot.com/2015/05/05-11-15-ans-minimal-flush.html),
and adapt the decoder to handle the end of the stream specially.
However, as Fabian points out in the comments, when the internal state
is below the normalised range, encoding a new symbol grows the state
proportionally to the precision of the probability estimate, rather
than to the entropy (\\(-\log p\sb{\mathrm{symbol}}\\)).

Intuitively, there's one situation where this downside does not apply:
when we *know* that we have incompressible symbols at the beginning of
compression, we can minimise the precision, and the state growth is
then proportional to the number of incompressible bits. We could also
stash that incompressible data in the initial compressor state.
Letting the initial state be denormalised is more flexible: the
approach naturally handles there being too many or too few
incompressible bits (at the expense of suboptimal coding in the latter
case), and we avoid implementing another codec for data in the
internal state.

I like to solidify my intuition by experimenting with toy problems.

My first attempt was to encode a stream of bytes (all with equal
probability \\(256\sp{-1}\\)) to another stream of bytes. That proved to
be a bit *too* simple: everything falls neatly in the right place
and the problem gives us no insight.

What I found more interesting was to encode the same stream of
equiprobable bytes to a longer stream of bytes in \\([0, 254]\\), i.e.,
re-encode an arbitrary bytestream while avoiding the reserved value 255.
The state of the art for this "byte stuffing" problem is 
[Consistent overhead byte stuffing (COBS)](http://www.stuartcheshire.org/papers/cobsforton.pdf),
which adds a fixed overhead of one byte, plus at most one encoded byte
for every 254 byte of initial data (i.e., \\(< 0.394\%\\)).[^1]

[^1]: Assuming bytes are uniform i.i.d., the expected variable overhead is a much better \\((1 - 1/256)\sp{254} \cdot 0.394\% \approx 0.146\%\\).

The entropy bound is \\(\log\sb{255}256 - 1 < 0.0707\%\\), less than
one fifth as much. Using rANS (and paying attention to the constant
factors) to implement this encoding gets us very close to the
theoretical bound:
\\[\left\lceil \frac{1 + (\log\sb{255} 256 + \varepsilon)n}{8}\right\rceil\\]
bytes in total to "stuff" \\(n\\) arbitrary bytes, with 
\\(\varepsilon \leq 2\sp{-24}\\) for a reasonable implementation.

Better: while COBS is fundamentally byte-oriented, and the main way to
accelerate it is to "stuff" longer words, rANS can be tuned to
performs well on contemporary micro-architectures, regardless of
whether we stuff bytes, longs, etc.  For example, decoding can reach
XXX of the speed of memcpy (XXX faster than COBS) on my computer.

What is byte stuffing good for?
===============================

The canonical use case for byte stuffing is communications over a
low-level medium that does not provide its own framing, e.g., a serial
port, or a phone line.  Once a special byte is picked as a packet
delimiter, it becomes possible to find where packets begin even if bytes are
lost, either because the sender was temporarily disconnected, or the
receiver attached in the middle of a packet.  In fact, having this
special byte even makes it possible to handle bit-level frame shift
errors: if there is never any zero byte in a packet, and we use a pair
of zeros to delimit packets, we will never observe a train of 16
consecutive zero *bits* in a valid packet.  Of course, we might
observe anything in a corrupt packet.  Once byte stuffing tells us
where a packet might begin or end, we must still apply some
error detection scheme to determine if the packet is valid.

I also find byte stuffing useful when logging records to persistent
storage. When corruption hits data at rest, it is often local, and we
can hope to detect a couple bad bytes with a per-record checksum…
unless the bad bytes overwrote the size field in a packet's header!
That's more probable than we might expect: too many filesystems bubble
up any read error as a corrupt (zero-filled) *page*.  More commonly,
software is buggy, crashes, and leaves a partially written record at
the end of a file, which induces a frame shift error.  I've seen too
many hacks where a special "highly unlikely" string is inserted at
regular intervals in log files.  It seems more elegant and reliable to
use a self-synchronising encoding which (like
[UTF-8](https://research.swtch.com/utf8)) guarantees that any
corruption---point or frameshift---only affects records surrounding
the corruption, and that readers can always recover from that
corruption.

Byte stuffing gives us such self-synchronisation guarantees. We can
always scan for the magic byte to determine where to start reading
again after a bad record (and when any valid record ends). This
suffices to recover not only from corruption after a successful write,
but also from short writes: Unix's [write(2)]() guarantees that
writes are always in full byte increments, even if the syscall
ultimately fails. Again, this assumes that badly framed or otherwise
corrupt records can be detected. Saltzer's, Reed's, and Clark's
[classic end-to-end arguments paper](http://web.mit.edu/Saltzer/www/publications/endtoend/endtoend.pdf)[^2]
reminds us that instrumenting records to detect data corruption is a
good idea, even when building on top of "reliable" abstractions like
TCP or file storage.

Consistent overhead byte stuffing is also attractive because of its
low memory requirements for the sender (a buffer of 254 bytes) and for
the receiver (no buffer at all), which makes it well suited to the
small systems that are likely to use dumb media like a serial
port. Optimal byte stuffing with rANS does not have similarly
attractive buffering requirements: decoding and encoding are in
reverse order, so at least one of decoding or encoding must hold the
full data in memory… but then again, that's also a requirement for
detecting checksum mismatches before passing bad data to readers.

[^2]: See [Moors's critical review](https://www.csd.uoc.gr/~hy435/material/moors.pdf) for a somewhat dissenting retrospective… although I feel like the wheel is currently turning against smartness embedded in the network.

The building blocks of range ANS
================================

All ANS variants share the same structure: an integer accumulator
serves as a stack where input data is pushed with reversible
operations, and the accumulator spills or reloads from the encoded
stream as needed.

For range ANS, the reversible encoding operation must accept a
denominator (e.g., 256 if symbol probabilities sum to 256), a base value
(e.g., 4), and a scaled probability (e.g., 17). Given the same
denominator argument as was used during encoding, the decoding operation
will yield a value in the half-open interval between the base and the
sum of the base and its probability (e.g., in \\([4, 4 + 17)\\)), and
return the integer accumulator to its value before the encoding.

The encoding function for the range 
\\([\mathtt{base}, \mathtt{base} + \mathtt{frequency})\\) follows.

{% codeblock encode_symbol.py %}
def encode(accumulator, denominator, base, frequency):
    return (denominator * (accumulator // frequency)
            + base
            + (accumulator % frequency))
{% endcodeblock %}

The inverse-ish function for that is composed of two steps: the first
finds the symbol encoded at the top of the accumulator, by returning a
value in \\([\mathtt{base}, \mathtt{base} + \mathtt{frequency})\\),
and the second actually undoes the effect of `encode`, given the same
total frequency denominator, base, and frequency.

{% codeblock decode_symbol.py %}
def extract_symbol(accumulator, denominator):
    return accumulator % denominator

def decode(accumulator, denominator, base, frequency):
    return (frequency * (accumulator // denominator)
            - base
            + (accumulator % denominator))
{% endcodeblock %}

I think it's pretty clear that `encode() % denominator` yields back
`base + (accumulator % frequency)`, which is indeed in the range
\\([\mathtt{base}, \mathtt{base} + \mathtt{frequency})\\), but it
can't hurt to double-check:

{% codeblock check_roundtrip.py %}
from hypothesis import given
from hypothesis.strategies import data, integers

@given(integers(min_value=0), integers(min_value=1), data())
def encode_extract_symbol_roundtrip(accumulator, denominator, data):
    # Hypothesis provides accumulator and denominator directly. 
    # Generate the rest while taking constraints into account.
    base = data.draw(integers(min_value=0, max_value=denominator - 1),
                     label="base")
    frequency = data.draw(integers(min_value=1,
                                   max_value=denominator - base),
                          label="frequency")
    # The accumulator is always non-negative.
    encoded = encode(accumulator, denominator, base, frequency)
    assert encoded >= 0
    extracted = extract_symbol(encoded, denominator)
    # extract_symbol gives us something in [base, base + frequency).
    assert base <= extracted
    assert extracted < base + frequency
{% endcodeblock %}

Calling `encode_extract_symbol_roundtrip()` in IPython doesn't hit any
counter-example. What if that's because our property, or even the test
harness, is broken? XXX Who tests the testers?

One answer to this question is mutation testing. First, let's check
that we can find bugs when we purposefully insert some. Good news!
[Hypothesis](https://hypothesis.works/) quickly hits an assertion
failure if I swap `+ base` and `- base` in `encode` or
`extract_symbol`, or if I exchange `frequency` and `denominator`.  We
can also apply mutation testing to the assertions or the
preconditions.  Hypothesis also finds trivial counter-examples if I
strengthen either side of the asserted inequality, but does less well
when I widen the data generation interval [^3] for `frequency`.
Adding a property for `decode` should detect this erroneous precondition.

{% codeblock check_roundtrip_2.py %}
@given(integers(min_value=0), integers(min_value=1), data())
def encode_decode_roundtrip(accumulator, denominator, data):
    base = data.draw(integers(min_value=0, max_value=denominator - 1),
                     label="base")
    frequency = data.draw(integers(min_value=1,
                                   max_value=denominator - base),
                          label="frequency")
    encoded = encode(accumulator, denominator, base, frequency)
    assert encoded >= 0
    extracted = extract_symbol(encoded, denominator)
    assert base <= extracted
    assert extracted < base + frequency
    # Also check that we can recover the initial accumulator.
    decoded = decode(encoded, denominator, base, frequency)
    assert decoded == accumulator
{% endcodeblock %}

The additional property at the end detects that something is off when
we let `frequency` take values that exceed `denominator -
base`. That's not too surprising: making either `extract_symbol` or
`decode` round-trip correctly isn't that hard. The magic lies in
making both work simultaneously.

[^3]: Generating test data just outside the specified interval is a trivial mutation. I don't know why that's not available out of the box, to help us test our tests.

The approximate inverse relationship between `encode` and
`extract_symbol` / `decode` lets us treat the non-negative integer
`accumulator` like a stack of encoded symbols.  That's not as
convenient as a `FIFO` queue, but seems to be the fundamental
downside we must accept to benefit from ANS's simplicity.

We still have to show that the encoding takes space proportional to
the first-order entropy of the symbols stored in `accumulator`, i.e.,
that \\(\log\sb{2}\ \mathtt{accumulator} \approx \sum\sb{s\in\mathrm{symbols}} -\log\sb{2} P(s)\\), and to bound the gap between the two.

Encode is

    def encode(accumulator, denominator, base, frequency):
        return (denominator * (accumulator // frequency)
                + base
                + (accumulator % frequency))

Given that \\(\mathtt{denominator} \geq \mathtt{frequency}\\), that's
never more than

    def encode_ub(accumulator, denominator, base, frequency):
        return (denominator * (accumulator // frequency)
                + base
                + denominator * (accumulator % frequency) / frequency)


which is equivalent to

    def encode_ub(accumulator, denominator, base, frequency):
        return (denominator / frequency * accumulator
                + base)


The first part, `denominator / frequency * accumulator`, is exactly
what we expect in order to track entropy: we multiply the accumulator
by \\(\mathtt{denominator} / \mathtt{frequency} = P\sp{-1}\\). Or, in
the log domain, we increment the \\(\log\\)-accumulator by \\(-\log P\\).

Since `encode_ub` is *an upper bound* on `encode`, it's clear that the
encoding can be super-optimal when `base` is small, e.g., when
\\(\mathtt{base} = 0\\). Nothing can be super-optimal everywhere, so
that's a good hint that the encoding is suboptimal for high values of
`base`.
