---
layout: post
title: "Plan B for UUIDs: double AES-128"
date: 2022-06-20 15:12:27 -0400
comments: true
hidden: true
draft: true
categories: 
---

It looks like internauts are having another go at the
["uuid as primary key"](https://brandur.org/nanoglyphs/026-ids)
debate.  I guess I'm lucky that the systems I've worked on mostly fall
in two classes:

1. those with trivial write load (often trivial load in general),
   where the performance implications of UUIDs for primary keys are
   irrelevant.

2. those where performance concerns lead us to heavily partition the
   data, by tenant if not more finely... making information leaks from
   sequentially allocated ids a minor concern.

Of course, there's always the possibility that a system in the first
class eventually sees a much higher load.  Until roughly 2016, I
figured we could eventually switch to 
[one of the many k-sorted alternatives](https://www.ietf.org/id/draft-peabody-dispatch-new-uuid-format-03.html#section-1-6.1.1)
created by web-scale companies.

By 2016, I felt comfortable assuming [AES-NI](https://en.wikipedia.org/wiki/AES_instruction_set#x86_architecture_processors)
was available on any x86 server,[^also-arm] and that opens up a different option:
work with nice "leaky" ids internally, and encrypt/decrypt them at the
edge (e.g., by printing a [user-defined type](https://www.postgresql.org/docs/current/xtypes.html) in the DB server).

[^also-arm]: ARMv8's cryptographic extension offers [similar AESD/AESE instructions](https://developer.arm.com/documentation/ddi0596/2021-12/SIMD-FP-Instructions/AESE--AES-single-round-encryption-).

I'm not certain why that option has so little mindshare, but I think
part of the reason must be that developers tend to have an outdated
mental cost model for strong encryption like AES-128.

Hardware-assisted AES: not not fast
-----------------------------------

Intel shipped their first chip with AES-NI in 2010, and AMD in 2013.
A decade later, it's anything but exotic, and is available even in
low-power Goldmont "Atoms".

The core of the AES-NI extension to the x86-64 instruction set is a
pair of instructions to perform one round of AES encryption (`AESENC`)
or one round of decryption (`AESDEC`) on a 16-byte block.

[Andreas Abel's uops.info](https://uops.info/table.html?search=aesenc&cb_lat=on&cb_tp=on&cb_WSM=on&cb_ICL=on&cb_ADLP=on&cb_ADLE=on&cb_ZENp=on&cb_ZEN3=on&cb_measurements=on&cb_aes=on)
shows that the first implementation, in Westmere, had a
6-cycle latency for each round, and that Intel and AMD
have been optimising the instructions so their latencies
are now down to 3 (Intel) or 4 (AMD) cycles per round.

That's pretty good (on the order of a multiplication), but each
instruction only handles one round. The schedule for AES-128 (the
fastest option) consists of 10 rounds: 
[an initial whitening xor, 9 `aesenc` / `aesdec` and 1 `aesenclast` / `aesdeclast`](https://www.intel.com/content/dam/doc/white-paper/advanced-encryption-standard-new-instructions-set-paper.pdf#page=17).
Multiply 3 cycles per round by 10 "real" rounds, and we find a latency
of 30 cycles (+ 1 for the whitening `xor`) on recent Intels and 40
cycles on recent AMDs, assuming the key material is already available
in registers or L1 cache.

This might be disappointing given that [AES128-CTR can achieve more than 1 byte/cycle](https://2013.diac.cr.yp.to/slides/gueron.pdf#page=20),
but that's because pipelining lets conteporary x86 server chips start
executing *two* rounds per cycle, even while prior rounds are in flight.

Still, 35-50 cycles latency to encrypt or decrypt a single 16-byte
block with AES-128 is similar to a L3 cache hit... really not that bad
compared to the slow operations involved in a durable DDL statement.

A trivial encryption scheme for sequential ids
----------------------------------------------

AES works on 16 byte blocks, and 16-byte randomish external ids are
standard.  The simplest approach to convert sequential ids to
something random looking probably goes as follows:

1. Fix a global AES-128 key.
2. Let primary keys consist of a sequential 64-bit id and a randomly
   generated 64-bit integer.[^not-strictly]
3. Convert a primary key to an external id by encrypting the
   primary key's 128 bits with AES-128, using the global key (each
   global key defines a unique permutation from 128 bits input to 128
   bit output).
4. Convert an external id to a potential primary key by decrypting
   the external id with AES-128, using the global key.

[^not-strictly]: The output isn't *incorrect* if the second integer is always 0 or a table-specific value.  However, losing that entropy makes it easier for an attacker to correlate ids across tables.

```
__m128i encryption_key[11];
__m128i decryption_key[11];

struct primary_key
{
    uint64_t sequence;
    uint64_t nonce;
};

struct external_id
{
    uint8_t bytes[16];
};

void derive_decryption_key(void)
{
	decryption_key[10] = encryption_key[0];
    for (size_t i = 1; i < 10; i++)
        decryption_key[10 - i] = _mm_aesimc_si128(encryption_key[i]);
    decryption_key[0] = encryption_key[10];
    return;
}

struct external_id encode(struct primary_key pk)
{
    struct external_id ret;
	__m128i temp;

    /* Convert to little-endian bytes. */
    memcpy(&temp, &pk, sizeof(temp));
    temp = _mm_xor_si128(temp, encryption_key[0]);

    #pragma GCC unroll 10
    for (size_t i = 1; i < 10; i++)
        temp = _mm_aesenc_si128(temp, encryption_key[i]);
    
    temp = _mm_aesenclast_si128(temp, encryption_key[10]);
    memcpy(&ret, &temp, sizeof(ret));
    return ret;
}

struct primary_key decode(struct external_id eid)
{
    struct primary_key ret;
    __m128i temp;

    memcpy(&temp, &eid, sizeof(temp));
    temp = _mm_xor_si128(temp, decryption_key[0]);
    
    #pragma GCC unroll 10
    for (size_t i = 1; i < 10; i++)
        temp = _mm_aesdec_si128(temp, decryption_key[i]);
        
    temp = _mm_aesdeclast_si128(temp, decryption_key[10]);
    /* Convert from little-endian bytes. */
    memcpy(&ret, &temp, sizeof(ret));
    return ret;
}
```

XXX concrete perf (compare with getrandom(2), and a AES128-CTR DRBG).

This works if our external APIs can expose with arbitrary 16-byte ids.
AES-128 defines permutation, so we could also run this in reverse to
generate sequence/nonce pairs for preexisting rows to avoid changing
their external id too much (e.g., pad integer ids with zero bytes).

However, it's sometimes important to generate valid UUIDs... or to at
least save one bit in the encoding as an escape hatch for a versioning
scheme.  We can do that, with [format-preserving encryption](https://en.wikipedia.org/wiki/Format-preserving_encryption).

Controlling one bit in the external encrypted id
------------------------------------------------

We view our primary keys as pairs of 64-bit integers, where the first
integer is the sequential id.  Realistically, the topmost bit of that
sequential id will always be zero (i.e., the first 64-bit integer's
value is less than \\(2^{63}\\)).  Let's ask the same thing of our
external ids.  The code in this post assumes a little-endian encoding,
for simplicity (and because the world runs on little endian), but the
same logic works for big endian.

[Black and Rogaway's cycle-walking method](https://www.cs.ucdavis.edu/~rogaway/papers/subset.pdf#page=7)
can efficiently fix one input/output bit:
we just keep encrypting the data until bit 63 is zero.

When decrypting, we know the initial (fully decrypted) value had a
zero in bit 63, and we also know that we only re-encrypted when the
output did *not* have a zero in bit 63.  This means we can keep
iterating the decryption function (at least once) until we find a
value with a zero in bit 63.

```
bool is_valid_pkey(struct primary_key pk)
{
	return pk.sequence <= INT63_MAX;
}

struct external_id encode_with_zero_bit(struct primary_key pk)
{
    union {
        struct primary_key pk;
        struct external_id eid;
    } temp = {
        .pk = pk,
    };
    
    assert(is_valid_pkey(pk));
    
    do {
        temp.eid = encode(temp.pk);
    } while (!is_valid_pkey(temp.pk));
    
    return temp.external;
}

struct primary_id decode_with_zero_bit(struct external_id eid)
{
    union {
        struct primary_key pk;
        struct external_id eid;
    } temp = {
		.eid = eid,
    };
    
    assert(is_valid_pkey(temp.pk));
    
    do {
        temp.pk = decode(temp.eid);
    } while (!is_valid_pkey(temp.pk));
    
    return temp.pk;
}
```

This approach terminate after two rounds of encryption (`encode`) or
decryption (`decode`), in expectation.

XXX concrete perf.

That's not bad, but some might prefer a deterministic algorithm.  More
importantly, the expected runtime scales exponentially with the number
of bits we want to control.  That's not ideal for UUIDv4, where only
122 of the 128 bits are defined as payload: we can expect to loop 64
times in order to fix the remaining 6 bits.

Controlling more bits with a Feistel network
--------------------------------------------

A [Feistel network](https://en.wikipedia.org/wiki/Feistel_cipher)
derives a permutation over tuples of values from hash functions over
the individual values.  There are
[NIST recommendations for general format-preserving encryption](https://nvlpubs.nist.gov/nistpubs/specialpublications/nist.sp.800-38g.pdf)
with Feistel networks, but they need 8+ calls to AES to encrypt
one value.

They're also solving a much harder problem than we are: we only have
64 bits (not even) of actual payload, the rest is just random bits.
Full format-preserving encryption must assume everything in the input
is meaningful payload that must not be leaked.

Our situation is closer to a 64-bit payload (the sequential id) and
a 64-bit IV (the randomly generated nonce).  It's tempting to simply
`xor` the payload with the low bits of AES-128 (or a [PRF](https://en.wikipedia.org/wiki/Pseudorandom_function_family)
like [BLAKE3](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE3))
applied to the nonce:

```
Broken(id, nonce):
    id ^= AES_k(nonce)[0:len(id)]
    return (id, nonce)
```

Unfortunately, random 64-bit values *could* repeat on realistic
database sizes (a couple billion rows).
When an attacker observes two external ids with the same IV, they can
`xor` the encrypted payloads and find the `xor` of the two plaintext
sequential ids.  This might seem like a minor information leak, but
clever people have been able to amplify similar leaks and fully break
encryption systems.

Intuitively, we'd want to also mix the randomly generated 64 bits with
something before returning an external id.

That sounds a lot like a Feistel network, for which
[Luby and Rackoff](https://inst.eecs.berkeley.edu/~cs276/fa20/notes/Luby_Rackoff_paper.pdf)
have shown that 3 rounds are pretty good:

```
Encrypt(A, B):
    B ^= PRF_k1(A)[0:len(b)]  # e.g., AES
    A ^= PRF_k2(B)[0:len(a)]
    B ^= PRF_k3(A)[0:len(b)]
    
    return (A, B)
```

If we let A be the sequentially allocated id, and B the 64-bit nonce,
we can also observe that `xor`ing the uniformly generated B with a a
pseudorandom function's output is the same as generating bits
uniformly.  In our case, we can skip the one round of the Feistel
network: that means we *deterministically* only need exactly 2 AES values.

```
HalfEncrypt(id, nonce):
    id ^= AES_k1(nonce)[0:len(id)]
    nonce ^= AES_k2(id)[0:len(nonce)]
    return (id, nonce)
```

This is a minimal tweak to fix `Broken`: hide the value of `nonce`
before returning it, in order to make it harder to use collisions.
That Feistel network construction works for arbitrary splits between
`id` and `nonce`, as long as each component is large enough (more than
20 or 30 bits) and at most 128 bits (the AES block size).

We can work within the
[layout proposed for UUIDv8](https://www.ietf.org/id/draft-peabody-dispatch-new-uuid-format-03.html#v8)
and assign \\(48 + 12 = 60\\) bits for the sequential id
(row id or timestamp), and 62 bits for the uniformly generated value.[^uuidv7]

[^uuidv7]: Or follow [UUIDv7](https://www.ietf.org/id/draft-peabody-dispatch-new-uuid-format-03.html#v7) with a 48-bit timestamp and 74 bit random value.

```
XXX C code
```

XXX concrete perf

Sortable internal ids, pseudo-random external ids: also not not fast
--------------------------------------------------------------------

With hardware-accelerated AES (a version of Blake 3 specialised for
short inputs might perform similarly), encrypting and decrypting
128-bit ids takes less than 100 cycles on contemporary x86-64
servers... faster than a load from main memory!

This post only addressed the question of runtime performance.
I think the real challenges with encrypting external ids have to do
with making it hard for programmers to accidentally leak internal ids.
While I don't know how that would go, because I've never had to use
this trick in a production system, it seems like it can't be harder
than doing the same in a schemas that have explicit internal primary
keys and external ids on each table.  I'm also hopeful that one could
do something smart with user-defined types and views.

Either way, I believe the runtime cost of encrypting or decrypting ids
is a non-issue for the vast majority of database workloads.

<p><hr style="width: 50%"></p>
