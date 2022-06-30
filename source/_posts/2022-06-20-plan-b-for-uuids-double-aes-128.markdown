---
layout: post
title: "Plan B for UUIDs: double AES-128"
date: 2022-06-20 15:12:27 -0400
comments: true
hidden: true
draft: true
categories: 
---

It looks like internauts are having another go at the ["UUID as primary key"](https://brandur.org/nanoglyphs/026-ids) debate,
where the fundamental problem is the tension 
between nicely structured primary keys that tend to improve spatial locality in B-tree operations,
and unique but otherwise opaque identifiers that avoid running into [Hyrum's law](https://www.hyrumslaw.com/) when communicating with external entities and generally prevent [unintentional information leakage](https://en.wikipedia.org/wiki/German_tank_problem).[^state-of-sin]

[^state-of-sin]: I'm told I must remind everyone that sharing internal identifiers with external systems is a classic design trap, because one day you'll want to decouple your internal representation from the public interface, and that's really hard to do when there's no explicit translation step anywhere.

I guess I'm lucky that the systems I've worked on mostly fall in two classes:[^really-performance-sensitive]

1. those with trivial write load (often trivial load in general), where the performance implications of UUIDs for primary keys are irrelevant.

2. those where performance concerns lead us to heavily partition the data, by tenant if not more finely... making information leaks from sequentially allocated ids a minor concern.

[^really-performance-sensitive]: There's also a third class of *really* performance-sensitive systems, where the high-performance data plane benefitted from managing a transient (reallocatable) id space separately from the control plane's domain-driven keys... much like one would use mapping tables to decouple internal and external keys.

Of course, there's always the possibility that a system in the first class eventually handles a much higher load.
Until roughly 2016, I figured we could always sacrifice some opacity and switch to [one of the many k-sorted alternatives](https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-1-6.1.1) created by [web](https://github.com/segmentio/ksuid)-[scale](https://www.mongodb.com/docs/manual/reference/method/ObjectId/) [companies](https://firebase.blog/posts/2015/02/the-2120-ways-to-ensure-unique_68).

By 2016-17, I felt comfortable assuming [AES-NI](https://en.wikipedia.org/wiki/AES_instruction_set#x86_architecture_processors) was available on any x86 server,[^also-arm] and that opens up a different option:
work with structured "leaky" ids internally, and encrypt/decrypt them at the edge (e.g., by printing a [user-defined type](https://www.postgresql.org/docs/current/xtypes.html) in the database server).
Assuming we get the cryptography right, such an approach lets us 
have our cake (present structured keys to the database's storage engine),
and eat it too (present opaque unique identifiers to external parties),
as long as the computation overhead of repeated encryption and decryption at the edge remains reasonable.

[^also-arm]: ARMv8's cryptographic extension offers [similar AESD/AESE instructions](https://developer.arm.com/documentation/ddi0596/2021-12/SIMD-FP-Instructions/AESE--AES-single-round-encryption-).

I can't know why this approach has so little mindshare, but I think part of the reason must be that developers tend to have an outdated mental cost model for strong encryption like [AES-128](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard).[^poll]

[^poll]: On the other hand, [when I asked twitter to think about it, most response were wildly optimistic](https://twitter.com/pkhuong/status/1530678742447579136), maybe because people were thinking of throughput and not latency.

Hardware-assisted AES: not not fast
-----------------------------------

Intel shipped their first chip with AES-NI in 2010, and AMD in 2013.
A decade later, it's anything but exotic, and is available even in low-power Goldmont "Atoms".
For *consumer* hardware, with a longer tail of old machines than servers, the May 2022 [Steam hardware survey](https://web.archive.org/web/20220619045520/https://store.steampowered.com/hwsurvey/) shows 96.28% of the responses came from machines that support AES-NI (under "Other Settings"), an availability rate somewhere between those of AVX (2011) and SSE4.2 (2008).

The core of the AES-NI extension to the x86-64 instruction set is a pair of instructions to perform [one round of AES encryption (`AESENC`)](https://www.felixcloutier.com/x86/aesenc) or [one round of decryption (`AESDEC`)](https://www.felixcloutier.com/x86/aesdec) on a 16-byte block.
[Andreas Abel's uops.info](https://uops.info/table.html?search=aesenc&cb_lat=on&cb_tp=on&cb_WSM=on&cb_ICL=on&cb_ADLP=on&cb_ADLE=on&cb_ZENp=on&cb_ZEN3=on&cb_measurements=on&cb_aes=on) shows that the first implementation, in Westmere, had a 6-cycle latency for each round, and that Intel and AMD have been optimising the instructions to bring their latencies down to 3 (Intel) or 4 (AMD) cycles per round.

That's pretty good (on the order of a multiplication), but each instruction only handles one round. The schedule for AES-128 (the fastest option) consists of 10 rounds:
[an initial whitening xor, 9 `aesenc` / `aesdec` and 1 `aesenclast` / `aesdeclast`](https://www.intel.com/content/dam/doc/white-paper/advanced-encryption-standard-new-instructions-set-paper.pdf#page=17).
Multiply 3 cycles per round by 10 "real" rounds, and we find a latency of 30 cycles (\\(+ 1\\) for the whitening `xor`) on recent Intels and \\(40 + 1\\) cycles on recent AMDs, assuming the key material is already available in registers or L1 cache.

This might be disappointing given that [AES128-CTR can achieve more than 1 byte/cycle](https://2013.diac.cr.yp.to/slides/gueron.pdf#page=20).
This gap between throughput and latency exists because pipelining lets contemporary x86 chips start executing *two* rounds per cycle, while prior rounds are still in flight.

Still, 35-50 cycles latency to encrypt or decrypt a single 16-byte block with AES-128 is similar to a L3 cache hit... 
really not that bad compared to executing a durable DML statement, or even a single lookup in a [big hash table stored in RAM](https://memcached.org/).

A trivial encryption scheme for sequential ids
----------------------------------------------

AES works on 16 byte blocks, and 16-byte randomish external ids are generally accepted practice.
The simplest approach to turn sequential ids into something that's provably difficult to distinguish from random bits probably goes as follows:

1. Fix a global AES-128 key.
2. Let primary keys consist of a sequential 64-bit id and a randomly generated 64-bit integer.[^not-strictly]
3. Convert a primary key to an external id by encrypting the primary key's 128 bits with AES-128, using the global key (each global key defines a unique permutation from 128 bits input to 128 bit output).
4. Convert an external id to a potential primary key by decrypting the external id with AES-128, using the same global key.

[^not-strictly]: The first 64-bit field can be arbitrarily structured, and, e.g., begin with a sharding key. The output also isn't *incorrect* if the second integer is always 0 or a table-specific value.  However, losing that entropy makes it easier for an attacker to correlate ids across tables.

<iframe width="800px" height="500px" src="https://godbolt.org/e?readOnly=true&hideEditorToolbars=true#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAMzwBtMA7AQwFtMQByARg9KtQYEAysib0QXACx8BBAKoBnTAAUAHpwAMvAFYTStJg1AB9U8lJL6yAngGVG6AMKpaAVxYMQAZi%2BkHAGTwGTAA5dwAjTGJvADZSAAdUBUJbBmc3D29fROSbAUDgsJZI6K84y0xrVKECJmICdPdPHwtMKzyGGrqCAtCIqNiLWvrGzJaFYZ6gvuKBsoBKC1RXYmR2DgBSACYvLBpggGpjAHEQuWMhAHk5ACVHbA2NAEFtryDkNywDja9HPBYWEECMQggA6BA/B7PV7vT6Yb6/IEEACe8UwCnBkMeLx2sNcXx%2Bjgm6HCqBcmK8UJxbwYH3x8MJxL2FKpMNpcIRRII%2BEELOxbLpBN%2BxNsfOhuPZ9M5ExBRjF1LxQqJyIUAHoFCrRLRaPKBRzCa4GHhibrnqYWFwtgAOPAHRjIYiojrGADWmGRGwArAAhLhcL0AER%2B3ux5stNoOWAdTtSrvdXt9/s9Qa8IehzxlrmsB3iIJYdWRcY90IA7GmXhoAJyuIExSTGAgHJQAR1c9swwf5VZrgjrDYODAEa07pZT5exmezmFUBCizFoxjw6H5Za71aBVv74WRs4UCa4MUDI5eJbHXaek8b09nxHni/QdtpGEwEEvObzBaLOZd8xX46rb7XnOYj3gcxCYAQx6PJWYbWras4sPEUHppWqoAFQHM4DAAG5RI2RAHLQhAEPQAC0Dh4IYBzbruoIHGhqprmwLDIPEyIQNsMQIfEpDfFsMTxC6vHJAAXpgqBUBA3HzL%2BqZrtxCIBkcALGKoJDGMk4ZSZgiG8fajrxM6boej6GiBrJ/7UrmTDAPmBzHI4jgHIaxAuLQBxcGZKH8MQByvngYn9raPxKf6qYHMFvweWZ4V4NsIZbN6v4odBCkhcpLDGEw6L2hpeBadxem0gZRnxj6cXJhZ56pTp8SKRlWU5eyTATHlBW1UV0aGbGxn7mZlXIZWzGsexnHgQQvGcYVTYBeJknjTJg3jSsDBgRBUGnueb65v8n7GZGlTPq%2BwJZleM7AQuS52kuyUnv%2BlbbR%2BjpfuNg2wRG3HIRWQ06SNHH8dNnGYEuwmzRJ2mIYtckoWlXhKcYKlqcQbXWhDPEHV1pUmd6/UBlV6ZWcQNl2Q5TkuW50Vrj5fmiZgQX1WF3oRZylOxfF8W3dBNWIfVCOZdlChRijVpo7xUYlT1ZXehVeNfdzdXpXzjWC5UBitZpqPTeLMYCEWfXmYN6GYQIuH1AcVCuSwhHEWRFFUTR6J0QxTG/Wx/0xONk0Ax1M1ieDC3499y23mtkHQyeQYExMTA2MgTa0/2wAQUTDDoKgLAQNhqBXWh4SuFQoOBY2edUPQDC8UC5sGMACic6uKHB6tGoKFqtAQEIACaQjGEnwKGGnLC8SXQ/52XvGlzZtcbZHFZPFnV1YCCuHGNr3W68ZmfZ%2Bgdf3avWP68m9X6TrDB66ZR7h9B1P%2BUXzPpYzd9RZ5wYReziWc1W0F75L2OeQcpGv0PorFSAt/jIGFhAY%2Ba9T69XKgbS%2BVZv7rylrjI%2BxUT5n19LjJaEEVrT3PPPB8CBpzoHcPECAaAGATAOMgBAdR6K5kwDQVQvFKHUMIfRdAMcmCFzpo2cu1EyTuWCAAdyIsEHea42GNkNMkYAwQHy0PobnHc6J6pcNqHLKseAqB%2BUYcwg4YAwDpTOP4fwH9KzQR2oISS2wtjbE9HuLYWxeL6LwKoQOVMSA01mvTdKMUmaRScgwF%2BcVEoczXFY2UBBbHOIcRoLYqg7FD1UXuOBA0EHfR0X5UR4jMAWKiUCWJ9jPSOBCc4zxVZG74IJpXfMQQICSJSvXb60EFAJ0bMAVAYdLJcyrF0xs6Ve4pwHu7KBWNeHg3GT/RYBx0ITxriFBiGhKmWO0bo6A3TDHGLhr7OakD0HQKLItFKn8qxMFJPURpg0NibQJl/KIeBl5IJge6a5mToLENUKQxC/0tjTIEAcYyIA%2BIuLXAcCFkKIVA0ORMvZUzYUzN4uhXJ0wlmqhOvkwaXyfnkLsS8oF7oQXJPBVCyFnEXlFkmZJSlxlZkoswGItFcMGKYsqd9R6u1nr7UEvVW5TMuC8UkN8TaCCHonSnOdW8IErr2mfA%2BdKcqsAQEEqszl%2BZuXugxvK9Rh1lVKswNvLRlZrExL%2BYJYlWxPR2OUDcAAkq4OsyS%2BLWucbah1Tq4mlPKS478oIWxtlpJgVxLpQSDiDasnFZC/kGvQJa31MK0BYHQNSg5SbDX0rQqi4I6K2WDVNcUqM8rLWuq2O6x1khnUOJtfaitdivRlOdUW5N/rMCtnbGLPVhqw1DixWKxuBwAkrhnhwRYtBOCel4J4DgWhSCoE4E5BQyxVgMh2DwUgBBNCjsWC6EAkhKygkrEe49J6T36E4JIKdW652cF4AoEAGgN1bsWHAWAMBEAoHTvEOgURyCUDQIhH90RkDAEtIKmgtAbz3ogOEa94QggFk4OugDbBBCXAYLQZE16sD5iMOIGdvB8DgSqLhe9BG/CqEqK4Wc16gRtGvURcIRNHTOCwNe4E/wkOjr4NXBQAA1PAjLLhomneu/gggRBiHYFIGQghFAqHUOR3QgqDBGBAKYYw5hGP3sgIsVA0CyOkUuFsf%2B%2BZ0R3uXWsPQwJMDrB4GOidV7yPzo4KoK0MRSJ1gOMAZAcdLSgi4H5RwvFcCEG8a8QVxtAP0F8hF%2BYvBN0EZkqQXd%2B7D2noy0e89HBL2kBYCAT0j7p2zpc3eh9T6kukFfR%2B5YBB4jUb/RQr9QGQisHWG5jzXmfN%2Ba2AF3ghqwsgjjeB2QknxAyfE/IJQahr3KdICIom8QuMOY4JO0gxXeAucuNR%2BrjYJIHA6554V3WPK9cCxAZw0Woh8S8FweLFWtDJeIUwRelAVu5fy4V9b17SsWHK4lx7O690Hsy5l7LXgnMldvQ97d2WtiQ829DgHsPTbJDsJIIAA%3D%3D"></iframe>

<small>[source: aes128.c](/images/2022-06-30-plan-b-for-uuids-double-aes-128/aes128.c)</small>

The computational core lies in the `encode` and `decode` functions, two identical functions from a performance point of view.
We can estimate how long it takes to encode (or decode) an identifier by executing `encode` in a tight loop, with a data dependency linking each iteration to the next. 

[uiCA predicts 36 cycles per iteration on Ice Lake](https://bit.ly/3nMxIKV).
On my unloaded 2 GHz EPIC 7713, I observe 50 cycles/`encode` (without frequency boost), and 13.5 ns/`encode` when boosting a single active core.
That's orders of magnitude less than a syscall, and [in the same range as a fast L3 hit](https://www.anandtech.com/show/16529/amd-epyc-milan-review/4).

<iframe width="800px" height="400px" src="https://godbolt.org/e?readOnly=true&hideEditorToolbars=true#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAMzwBtMA7AQwFtMQByARg9KtQYEAysib0QXACx8BBAKoBnTAAUAHpwAMvAFYTStJg1AB9U8lJL6yAngGVG6AMKpaAVxYMQAZgDspBwAyeAyYAHLuAEaYxCCSAJykAA6oCoS2DM5uHt5%2Byak2AkEh4SxRMfEWmFYFDEIETMQEme6evpXV6XUNBEVhkdGxCQr1jc3ZbcPdvSVlgwCUFqiuxMjsHACkAExewchuWADU616OeCwswQTEwQB0CMfY6xoAgls7DHuuh8eOw%2BhYVDuDyer22u32mCOJz%2BlyBXkeLze4K%2BkJ%2BMNQcIRoPen2%2B0II6FoeAimJBSI%2BEKhvyuwWApMRL1MLC4mwAHHgDoxkMQAJ6JGrGADWmB56wArAAhLhccUAEWOErJL2GxFc1gOiWuLAaPKFIrJPkVDLirkuADZJMYCAclABHVxczAKpUm82W60MASrZ2InzyrxG17Kq5q62YVQEaLMWjGPDoA2Bp6uwSsq0HCI8yMKcVSs1yn2vP0FkGTGzIG0h9XhyPEaOx9Ccj4YTAQFWhjVanV6nkawVzBMuttViNRsT1g7ETAEYsaOJMlnsg6RliJGdBuIAegAVAdnAwAG7Ra1EA5EggEegAWgceEM6czmAUNwOW43LrYLGQiR5EC2ZuXiSkEcmxmokgpAakABemCoFQEAAXM/YBi6AFQrKBzGOcxiqCQxipAu8GYCuQFcry/LpN2OYaHKSGJoi2yakwwDagcADijiOAcrgMMQLi0AcXDUca/DEAcrZ4NBaYcsc6EygGBzSScAnUfJeBbIqmwSv2xpJqhMkYVhTCPlyeF4ARAEkR8ZECsKoqSmpYqyrRSrrnpXjoZhLDGEZSgUkwwymeZRGAY23J8jZ%2BqSoJNEznEH5fj%2Bf6TgQQF/hZNoSTBcHJYhsXJcsDATlOxZFgyzymEw57XBEriRqYUCesERIhIhFaquqmpnF2tkglyzbGLQqCoIkraVtanXary3a9hBmVppOiSYJV2mFnRJoMOkRyGi6SZDuNnZTbZvaxbtY2ciOtZjnGnJxidfoVpVqLuVta1JjcYFoTNO1FshZVJiJYlQZgUmfSpEoKVSRWLZVCoKep6krUmp2PTcmDXfpfVYKNKNgc5f2zvltYPZG72CiV8plaWeDlk1wSQqaggWlaILAFOVrU4KCjGFEwDBBADMEEz1pbmgK50NE3MNNc0SI9txoC14mxpoNQEIGpv1Buu/ksAc%2B4uJV4u/psmxfqa8Zio4DDio407Gy6BwO47TuO1smzEOgBAKMgrv287fsHCAwGbDJTCuxAg0LEHMnxsbEBq5HPuacQYci6gYv0MQkvENLxBzCAICJ2KxARKorupXbmxF8gpfG6Qvv%2B07rsfiQoq10HyDe8beProThVQALQtzGrVI/AcitzFtnGDeTLlUzTDDNfTbrMy8rMEOzyCc8YDgQHrcay2tCtK9aKsHGra5Jtruv6zY9BG27Hte6uFtWxbtvB8aDfO03qD7sBYpbDFEtVQ5cAGSmno5a2r8bY%2B0/l/F2xsWC/3/oAzA6AQEoMlOfSBL9rbv3rvA12ptbp2zgfAgORxwGoDlFHdyydY4R1Slghy6EfZ0LDvHfOhdiBMBrpscuwdK7uz4QIwBxcRFRyEdXMuBCv5NyIi3Mu7dO6bG7kmXuYl%2BbLwIEPRSjhR7j0nqeahGt1ilU1nPBSggDjIBXMYVwFoIBoAYMMa%2B10tz7gwc41xe8GweJ5AfF03jrQD3dC%2BVQn1PGxWCVxbRL4ez6X3KKUxxo8BUDEluCJMl9JbgCTtAmU4CoHDBi6DREBMmQ1yRPY4AAxA4l4uAULkomcxZJtgAjpgcUIAAlbAyhlIaEGUJbEHSQhdIAJIABVsDdIGcMkElwDjaj5oE40ljQlpiqEwRIShzYSh6X0/MKTkwEFTNaHqkogawQgKRcKFFbL9kgSk9cANxKSWtIpdCYMIajyuXBB5sM1KaQRvkuIFyJQsM%2BurOi654rfiNmaW55EBDdnLmaWys1oLXKRRFAJajhIkEBnND5oNAWQwOcoQF8NNLVLluuU67V9pdUOiKXsn0zHgzwEBaSP0YVI1nBs60PNgifTXhvLewqGAIrAjcO0DoPiYHxXEJMH0MZNiwANIaI0wJAVCFMmZSrL4KBYPfV2FDE4ShYGHXGhqBVxIcKKtmZYt473xfS2cWydloJzJCtVDZLzpkwLzK2xyzEU01kmW0CgSAEBuQYL16BMVZTjds3ZE8NwZSxXBT1uyqI0TrmQx2fyU0JrzY5SOtjEj2ItG6lV1xBBwULjcLw6TAHNtbZXdtUCZGFsdhALgNwNAvk5PG3NkoKUHAzYJaijlJ0TOmd0hYsiHb9sHcOnN3rx29P6Rm4Os6M16oXUu3tK6B1Dp3BuvZE6A2NIDROqdgyaEHv1YupVGiSm%2BnDRwBYtBOBil4J4DgWhSCoE4JxaNyxVjAS8DwUgBBNDfoWIKQYNw4hofQxhjD%2BhOCSAAwhkDnBeAKBABoODCGFhwFgEgUWiRxZkAoE4tOtGM4oGACyLgfA6A1mIxACI%2BGIjBB1JwWDos2CCAAPILx5PhrA2ojDiCA7wfAk5rB4EPMRxT/hVCYGQLVNYwHLhVHw0SCIPDeTOCwPhmkLBhPfr4AYYACgABqeBMAAHdxOLUA7B/gggRBiHYFIGQghFAqHUJp3QHGDBGBAKYYw5gTPEcgAsYaNQNOXnE5sep2pHxEaqDpmo9gGBOBcC0PQgQ6YzAGBxvIaQBBjE8DVlIdWGDTH6DEDjlgCudBGE0Ur2ROv5dUwILojQ2ulGqxYXrDW9CTDG5V9rEgFgQZWIFuDk41g8B/X%2BvDmnQMcFUKyM0l4LQHGAB3ASmwbiNIgI4ICuBCCEreBx3cTG6PQa4HMXg8HFOIVIMh%2BIqHMPA7Q9hjguHSCAeA/tojJGyO/e2xwTYu3oeEfh1oP7h5iCpDsJIIAA"></iframe>

<small>[source: aes128-latency.c](/images/2022-06-30-plan-b-for-uuids-double-aes-128/aes128-latency.c)</small>

This simple solution works if our external interface may expose arbitrary 16-byte ids.
AES-128 defines permutation, so we could also run it in reverse to generate sequence/nonce pairs for preexisting rows that avoid changing their external id too much (e.g., pad integer ids with zero bytes).

However, it's sometimes important to generate [valid UUIDs](https://datatracker.ietf.org/doc/html/rfc4122),
or to at least save one bit in the encoding as an escape hatch for a versioning scheme.
We can do that, with [format-preserving encryption](https://en.wikipedia.org/wiki/Format-preserving_encryption).

Controlling one bit in the external encrypted id
------------------------------------------------

We view our primary keys as pairs of 64-bit integers, where the first integer is a sequentially allocated identifier.
Realistically, the top bit of that sequential id will always be zero (i.e., the first integer's value will be less than \\(2^{63}\\)).
Let's ask the same of our external ids.

The code in this post assumes a little-endian encoding, for simplicity (and because the world runs on little endian), but the same logic works for big endian.

[Black and Rogaway's cycle-walking method](https://www.cs.ucdavis.edu/~rogaway/papers/subset.pdf#page=7) can efficiently fix one input/output bit: we just keep encrypting the data until bit 63 is zero.

When decrypting, we know the initial (fully decrypted) value had a zero in bit 63, and we also know that we only re-encrypted when the output did *not* have a zero in bit 63.
This means we can keep iterating the decryption function (at least once) until we find a value with a zero in bit 63.

<iframe width="800px" height="500px" src="https://godbolt.org/e?readOnly=true&hideEditorToolbars=true#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAMzwBtMA7AQwFtMQByARg9KtQYEAysib0QXACx8BBAKoBnTAAUAHpwAMvAFYTStJg1AB9U8lJL6yAngGVG6AMKpaAVxYMQAZg2kHAGTwGTAA5dwAjTGIQAA4uUgAHVAVCWwZnNw9vXySUmwFA4LCWSOi4i0wrfIYhAiZiAgz3Tx8KqrTa%2BoJC0Iio2PiFOoamrNahrp7i0oGASgtUV2Jkdg4AUgAmLywaYIBqYwBxELljIQB5OQAlR2w1jQBBTa8g5DcsPbWvRyYFJQaAHQIL53R7PV7vTCfb60PAsQgKIEg%2B5PLYQ1wfL6OOHwwTEIJIryg1EvBhvDFQrFBAgEACeCUwiOBRJR4LJkOhjiG6HCqBchOJbPJmO%2B3J2AtZaPZFM53OpErBUuFlNFBHwqAVJPRIq5BHxRk1Qo5WIUtIUAHpTQpRLRaIalcbvq4GHhuYbHqYWFwNjE8HtGMhiPTqsYANaYWlrACsACEuFxowARL4xlGe72%2BvZYQPBtJhiPRuMJqPJrypsGPIbEVzWPYJfEseq0/ORsEAdnLTw0AE5XNSAGySYwEPZKACOrgDmBTrJ7fcEg%2BHewYAhWM/bpc7KKrNZHmFUBCizFoxjw6FZHdnvepMSX4Vph4Uha4/aT66ebc3s4eExsyFHeq7v6B5HmIp7oP6ZIYJgEA7rW9Zwk2LZ1qGswXluPZwXuIHEMe4F7MQmAEO%2B9zdumPp%2BoeLAJCRFbduaABUezOAwABuUQjkQeywjS9AALQOHghh7Pej4AnsDHmlebAsMgCS0hAmz9lRCSkJ8Gz9gkoZqSkABemCoFQEAqbMaFlleKnQomBwsCwxiqCQxgpBmxmYNRakBkGCQhuGkaxhoSZmRhJL1kwwCNnshyOI4ezOsQLi0HsXABXR/DEHssF4PpS5%2Bl81kJmWey5d8SUBYVeCbKmGwxmhdGkZZeU2XZTCMgGTl4C5KkeWSXk%2BQWsYVSWQXfvVbkJFZTXGC1Sjsr8BDtZ1Y3dTm3l5r5z4BUNtHdjJckKUphEEGpSldaOWUGUZh2mdth1LAwBFESRn7fr%2BeD/lhdYNkhvlZpgaBYLBgG1vuh64WBZ7%2BmetUfhh3YfQhjZBshh3beRmYqbRXY7W5e2KRpp1KZgZ46edhmudR13mXRDVeNZxi2fZjnOT65Oqb9K19X5MabYmw0ViFxBhRFUUxXFCWlVeaUZXpmA5RNBUxkVnIS%2BVlWVdDpGjdRE3081jLZgtLOndmvVrf1MaDbzmNa%2BNjW61N%2Bt/QYQyGzErNqSbuYCC2G2BdtjHMQI7ENHsVDxSw3GEAQ/GCcJomMuJknSTj8l4/2h3HfjS1nfpZNXXzWO3bhD3EVTH7Jvzr3/ryLhFQoxisWIZ7GFpEaA9W8FfUjP1aRrl50UX91aQC46TmSKqOI1ACSIQACqLgAsg8AAaT0V12P5A9hoN4RDAbQcYADuhAIMY%2BnxcY4SEO3QEI99EYoX3sPOmknz91jpHw13zY96G22f1vYCO9wYQSJueMumtPx7BptZNY79NY9gBFpCaWlSBXjgV%2BfmpFfj/AIBAV0Dcm7oBbr5CAvcC5XnQKgN%2BsMbYAjARNfeAMVJINQv/KBh8EB0ChBAMAYACGN1hMQ1uCkWHkOtj2Qe0Cxr0LPGvF6gC77dwftmA%2Bx8CCn3PqgS%2B18Pog1AiePeUN0JXhfgIGh6DMKKO/shLS/8rEd23gY/CYD2HWRgRYuqiCGGNTAWguqz0IF0RwRxfB9dBHNxEazVhlNgqkSoZ4j%2BPYxGhgmqo5hMiwEUJ7BgvYnDuEZT4QIohJC24pNiSNSRRE7rSOoqw%2BRlc6h/lHDLJcwAiKCwYFQlgEBWKoAhgxcIrgqAk2yiOIZVB6AMDUtSUOBhgAKCfleKRVobS0AgEIAAmkIYw7S9SGG6WpCZRzhlTLUpMsKiyGkbz6RDLA%2BJ2LGE9qtb2pDbnoCWXRZ5nNfYlkYT1L2DAfb%2BTfEE7sUtMpjKVo1BW0KSrJRTEVNW1UNY5J7N8s2XNkp7D4kiv5dsGbTThMgV2EBPKAuBRbP2YKMWvPNjzf5HNMW/MwYXapuFrnbiaW9PY7yUQIH3OgdwCQIBoAYEMPYyAED1AkvWTANBVBqTFRK95El0BMDqKM2WI5pkiT5IlYIh9YTBE%2BVjZVI5nQpGAMECCUqZWDIfIyNJGqmASO7HgKgGU5UKr2HwxqJx/D%2BFRd2UiCFBBGU2BsTYUYnwbA2Gpb1eBVDZKxhC1pI5irWTKorYqMUGCIoqtVdWljuxhoIBGuN0aNAbFUJGo5jqnwDWpXEnsHqMqGuNZgYNob9TlrxlGqMjh81xuyd2QenLHjUhRI2IIEBTWQNoZhdNexgCoFLi2kNPZV0jkanszp3S07kpeUC3yWqyZHs5vMPYjELkLLypJDQo7SJtugGu31YBGoy3PQC49LZrpeM3d2JgvIGhzrcZU7s9y8CPNpSetuKbSICtUEK6i/aL2v18iAdS8arx7Dw/hvDhMf2czPUZdDdLaRXsYh2oI05aaSUAl27aSGUMisjbBvYmHsP%2BKxgRgjSlYMtlIxAQTvkqMMRo8Ee95pGMIYcbfGxv8JpwMVvEPYkg36soAY4oBzi95QSwBBXxBmYK922nonCu9QEmeIeozRURqHGf%2BrLOzZ8HM6LwWZsFX9ELKNpOzaCRnaYBYBkwzAHzzPWN8z/FRf1AtHxPm5%2BKaS4tYASxopL2ir54LC7ZxLWi5Olt7RWjYWksPRsjcoK4U9XCDjrepKMlXqu1ckJG6MQ76vD1HlOBNoYAQrnHoVljwq0M2fK3GzO/ZcvCdy%2BJyTdHEwMerExsFw3UORty3kk%2BewtHjZw3RPj%2BGiPObyxlrRM2bPpfs/FObmAjW0ek7Jt1ZaSvpPC%2BNxrcaqs1bqxNhrTWfutcrYO4d8aQvhZHpgCcPXwfoH66uFbsMXv9rexBOzO2HMfYBy1%2BrFWvvNd%2BwOjrE3cOHdR1dzLkPofjw9ql8LFOtHw8Gzddl91s0XnXhweYtBOBRl4J4DgWhSCoE4DFBQixliUi2DwUgBBNBc/mKGEAUZfA844JIfn8vhecF4AoEAvg5eC656QOAsAYCIBQKgai3CyAUFFVbhINuUDAG9PEGgtBQZ64gOELXV9mBBk4DLtAtlGAEHOAwWgtItdYEbEYcQRvSD4EItYaDjItf7j%2Bq4Q8WvqSVC17CcIgsgzOCwFrvUcJA/G9vQoAAangO75wGQC5l/wQQIgxDsCkDIQQigVDqAT7oeIBgjAgFMMYcwBe9eQHmKgY9euOB8XOBsHFjZGS68qH9ao9gumjE8PEAItHpj9HiLkVIAhd96FP9UKYfRoiDA3yngQnQRguGaHoSwm%2BOjDG6If2/7/v8X6DDf434lDH7zDi5LArB6B6iYCrA8Dc686a4J4i4cCqAxD9h8SDgrrID/jegAhcAZSOBqS4CEAkDqReBqbODW70DpTPBcCzC8CG5aCmSkBK4q76CcAa6kAsDK6%2BAC5C4oG6766y7y7zCm4W4Z7IBZ4kDkCUD1ALLKCGCVBCAICoCHzN68DB6O4GDVCKHBC0AqFqH8GaEO5O7IAu4bDxBaE27nBZ6GHqHp6qB/QPDEALI65%2BBOHIC1D4AC68Ct7CA2id7SD%2BG95qBa6D76CGAmBmD6B4DhBT5zrC5z6cCL7L58Sr4KC64S5QFAE%2BF6HKGqEOHcCMGERwG8CHyCwJCV4IEcB86kDGHa4cDYCeHSHpRoEYFYHAA4FJQbD4GEHEH4BEC0HS4MEiFG4sECpMD3KUA1FcE8HsENGCEWDCFMEK6sG8EcEcBeBIECHuGrEsFq4bA7G8BLH7HzDBwpB2CSBAA"></iframe>

<small>[source: aes128-cycle-walk.c](/images/2022-06-30-plan-b-for-uuids-double-aes-128/aes128-cycle-walk.c)</small>

This approach terminate after two rounds of encryption (`encode`) or decryption (`decode`), in expectation.

That's not bad, but some might prefer a deterministic algorithm.
More importantly, the expected runtime scales exponentially with the number of bits we want to control, and no one wants to turn their database server into a glorified shitcoin miner.
This exponential scaling is far from ideal for [UUIDv4](https://datatracker.ietf.org/doc/html/rfc4122#section-4.4), where only 122 of the 128 bits act as payload:
we can expect to loop 64 times in order to fix the remaining 6 bits.

Controlling more bits with a Feistel network
--------------------------------------------

A [Feistel network](https://en.wikipedia.org/wiki/Feistel_cipher) derives a permutation over tuples of values from [hash functions](https://en.wikipedia.org/wiki/Pseudorandom_function_family) over the individual values.
There are [NIST recommendations for general format-preserving encryption](https://nvlpubs.nist.gov/nistpubs/specialpublications/nist.sp.800-38g.pdf) with Feistel networks, but they call for 8+ AES invocations to encrypt one value.

The recommendations solve a much harder problem than ours: 
we only have 64 bits (not even) of actual information, the rest is just random bits.
Full format-preserving encryption must assume everything in the input is meaningful information that must not be leaked, and supports arbitrary domains (e.g., decimal credit card numbers).

Our situation is closer to a 64-bit payload (the sequential id) and a 64-bit IV (the randomly generated nonce).
It's tempting to simply `xor` the payload with the low bits of (truncated) AES-128, or any [PRF](https://en.wikipedia.org/wiki/Pseudorandom_function_family) like [SipHash](https://en.wikipedia.org/wiki/SipHash)[^compliance] or [BLAKE3](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE3) applied to the nonce:

[^compliance]: Most likely an easier route than AES in a corporate setting that's likely to mandate key rotation.

```
BrokenPermutation(id, nonce):
    id ^= PRF_k(nonce)[0:len(id)]  # e.g., truncated AES_k
    return (id, nonce)
```

Unfortunately, random 64-bit values *could* repeat on realistic database sizes (a couple billion rows).
When an attacker observes two external ids with the same IV, they can `xor` the encrypted payloads and find the `xor` of the two plaintext sequential ids.
This might seem like a minor information leak, but clever people have been known to amplify similar leaks and fully break encryption systems.

Intuitively, we'd want to also mix the 64 random bits with before returning an external id.
That sounds a lot like a Feistel network, for which [Luby and Rackoff](https://inst.eecs.berkeley.edu/~cs276/fa20/notes/Luby_Rackoff_paper.pdf) have shown that [3 rounds are pretty good](https://en.wikipedia.org/wiki/Feistel_cipher#Theoretical_work):

```
PseudoRandomPermutation(A, B):
    B ^= PRF_k1(A)[0:len(b)]  # e.g., truncated AES_k1
    A ^= PRF_k2(B)[0:len(a)]
    B ^= PRF_k3(A)[0:len(b)]
    
    return (A, B)
```

If we let A be the sequentially allocated id, and B the 64 random bits, we can observe that `xor`ing the uniformly generated B with a pseudorandom function's output is the same as generating bits uniformly.
In our case, we can skip the first round of the Feistel network;
we *deterministically* need exactly two [PRF](https://en.wikipedia.org/wiki/Pseudorandom_function_family) evaluations, instead of the two *expected* AES ([PRP](https://en.wikipedia.org/wiki/Pseudorandom_permutation)) evaluations for the previous cycle-walking algorithm.

```
ReducedPseudoRandomPermutation(id, nonce):
    id ^= AES_k1(nonce)[0:len(id)]
    nonce ^= AES_k2(id)[0:len(nonce)]
    return (id, nonce)
```

This is a minimal tweak to fix `BrokenPermutation`: we hide the value of `nonce` before returning it, in order to make it harder to use collisions.
That Feistel network construction works for arbitrary splits between `id` and `nonce`, but closer (balanced) bitwidths are safer.
For example, we can work within the [layout proposed for UUIDv8](https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#v8) and assign \\(48 + 12 = 60\\) bits for the sequential id (row id or timestamp), and 62 bits for the uniformly generated value.[^uuidv7]

[^uuidv7]: Or copy [UUIDv7](https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#v7) with a 48-bit timestamp and 74 bit random value.

<iframe width="800px" height="500px" src="https://godbolt.org/e?readOnly=true&hideEditorToolbars=true#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAMzwBtMA7AQwFtMQByARg9KtQYEAysib0QXACx8BBAKoBnTAAUAHpwAMvAFYTStJg1AB9U8lJL6yAngGVG6AMKpaAVxYMQAJi%2BkHAGTwGTAA5dwAjTGIQADYAVlIAB1QFQlsGZzcPb19k1JsBQOCwlkjo%2BItMKwKGIQImYgJM908fSur0uoaCItCIqNiEhXrG5uy24e7ekrLBgEoLVFdiZHYOAFIvAGYsGmCAamMAcRC5YyEAeTkAJUdsdY0AQU2toOQ3LH31rccmBSVGgA6BDfe5PF5vD6YL4/Wh4FiEBTA0EPZ7bSGuT7fRzwhGCYhBZFbMFo14Md6Y6HYoIEAgAT0SmCRIOJqIh5KhMMcw3Q4VQLiJJPZFKxPx5u0FbPRHMpXJ5NMl4OlIqpYoI%2BFQitJGNF3IIBKMWuFnOxCjpCgA9GaFKJaLQjcqTT9XAw8DyjU9TCwuF4ABx4faJYhUYwAa0wdI06ziACEuFxowARb4x1Fen3%2BwPBsMRhOx%2BNJlNspU7TB7aFCbAARTk2BCd2MAFlHgANfZQLhyfz%2BLnY/YxDRzfYAWn2XDmUtL5f2IQu9ewTdb7Ygne7vZ%2B/a8Q9H4%2BLj2GxFc1iz8IadJzdLZAHZU%2BCNABOVw0mKSYwEfZKACOrkYqyLd8fZ9X3fBgBD/LZb2eK9kwgvcDyPd9MFUAgomYWhjDwdBr0gh5AMEX0332cI6RQhRozjGJC1g8FoP/Z4ngtAAqfYABVD3JJgUPQfZHmwIRhwzfY/iE/ZlGuAAxfYqGIVAWH2VwX32Ih5JfQF9kYi1UUmGxkHkoDCKDKgICfQQX0I8JEVIfY0AYYZDmMb0/QDcNL1jbS8F0gs4kTCcaJwh90ycpTMBYRIYUTQ4WBYYwlAIYxMESPAX1UCANCsiyCAUCdqPo%2Bj7xQ0Lwsi6LVBIGK8AzCACsSKyXPIqNvOy/zSSDJhgBYJh9iORxHHkhgZLtMcowA/hiHbVIAC9MEIgNvgihMIP2WaNy4KNFrwTZUy8GNfLy3DqqKhzoqYJlf3KyrqtqiNyI2xq6OLfKQrCubiuME6lA5P44tSC6nqu1y4wanz7ofYhMAIZYGGC0L6qonDaLvR4mNRdT9mwcliAZTKvgHRJQ3WAd9gAdwQOhoUYBRliCYAlIQaEaGIOyIDhWl6GHBw8EMIcFMkKS8CqbighR5iCDp/Zf0xxIuP2MHKdod83SE98WBSd9KxrOsG2bFsrMMbjRehJQbPQZjOOF/YVbs2d50XFtAWF82WLFoJElcd8CY0PGPYt1w7IUTi3SoOlacN1hoWIQxgGhGyDyYGkkWFzSnng48kJQ4g0Iw7jRo6uLfwwTAIBT98g1PTGL0DUNdvWG8HpddIvlrgDcOL8XkNQsQs/FzCQfvFv9QQk8OvLlzK97muIoOl6a/8vuH0BPGirx0gHonkG8r%2BAECCqp6F9DQEvx/clVUcF71drG3taah7N6ibfqr3wFQOP9cIutrXW2vxH9t3vGD8wb8v4qRxHuFsCKhkd6hUfs/VYVlDIXkHF8LwMR9jn01gubWvcH5/xgcA0B4DgyQMSI/Q%2BQC4HZhcuOJBKD34YNbOvXCt9GhEJIQAo%2BqxX6oOrBfD%2BLYv4b3%2BHfFhOCwInxerQ22/CHpgwhhnaGxDMA9xymvRGyMniowAJIMAAG53wUDjDQOdOLxXJAXD29t1EaS0gPY8pdh7nlHkYuKWA0BYCLjYxC7cM6d0wt3dA1cm55XrgIRus9%2B6HlTl4zOvjFFYWUQ%2BcJg87Fngrl7eJfdoLyKKjPVe89YlFViSvZutF4l5WwfvXBXwQEvQgeU/%2BgDj7kJDJQocmwaFzl4Vg3%2B%2B9SEv2jPgrMRk6m4KaQg1pyCuEa0vvQ0puEZGQ3kXve6CNcruV0v8PAU1CJR31HrWSEBtGoF8YxcIrgqBWUmtNd8pyqD0AYFZGkUkDDACythaR4MFnWltLQCAQgACaQhjA7IjgwdAsl0pnIhbcxgVlbltVecolZ1iA66UOT3J4dNVDoHcIkCAMd3zIAQA0dSQYyx4FUFZfF%2Bw0XcUYugTiTALmbKufse5RF%2BS0FZZgImcJggBNnlSl0qRgDBG4oS4lJySJMiKvS%2BoDCHx4CoO2UlNBVD7DAGAcRXZ/DVwSQ%2BUuggjKbC8JsOIZE2hZjJaoKRI0SDjWZTNIqa0YxLS5KylMS1NqbV1XPe8BqCBGp8KajQXhVDGvSlKsisZbrA1mQqpVEBgg8qCJgH1uF/WBpNXERwDBjU2vvPMjOyzkyIxpKiDqQQID8tXoE3CLdLnbNQAQeVvrgBNqKsCvZLAIBtPgS5NKH5mWoCMn2iMg4rJMThS8uaGlBzj3je2Nt74NUvUucOiAo7IxzDTXq%2B8TA%2BTMJtXtB8S6O3gxBWC7tvaKG5iZVNddm7xwTsYlOsiYDZ35twoqxd7aV1gMHfekdN66Tjh3b6/dJBt6fqRQBTF2LQo9p8JujQIAkG%2BGochu9mAH3AfHfsJiSbeWYBnRaAeqbe5wZxYhrwj7UPhow8BrgWGcPNNzAsfDjFCMppI2R/NrdkkjwjJXbJN4xxWV5mvdJrc04d3QjE0xWBuIvScSY1xhc8Z8Y8UPFJo8XEFyU/%2BlTem3H50U0e9NBoA3UbxnRrwcRjViQ0TzejpqHPXCcy%2BY10Yc30b/r02Blcn6iPMw%2BSjCHjWmcwOgWz6G2mRfQMxoy8X2MEe5URnjh5yNxr9ZZzNxmou2fsz4RzzmLWueK%2B50rWafMWvy%2Bgep7DMBWTq0F4%2B%2BbC1Q2ddeEtjwOALFoJwOIvBPAcC0KQVAnBeoKCWCsKk2weCkAIJoPrCxQwgDiGlAbHBJDDeW%2BNzgvAFAgDSkt0bfXSBwFgDARAKBZKJXoGQCgeK7tk2iMgYAPomM0HllEI7EBwh7YsswTGnAFtoCiowAgFwGC0DpHtrAHUjDiDO6QfAYNrB4F0UdlHSFMDIDdmsMbNIqh7bhOECOmNnBYD2/qeEoPzuvoAGr8yJhcRkI2Fv8EECIMQ7ApAyEEIoFQ6gUe6CYwYIwIBTDGHMGTo7kAFioClukbHw4LheBHB1Jkh2qh45qPYUFYxPBMYCCmmYAwmN5DSAII3egrc1GmP0aITHLB686CMJoLgWh6FdxjgQXRGiO9KBbiwHvbcu490H2YT7pvLFWHofUmA1g8H64N3bKOJscFUL6GIw5FLAGQJ5LwgIuDtkcFZXAhA7UvCY/sZwoVXtIK2OOXgp2tDbtIGtjb%2BhOA7dICwdbaURtjcz4d47i3lsLEuzd3H%2BOiCPcoA0F5yhDBVCEAgVARMOe8HB/dgOAgV/BFoOvzfw%2Bd8vYeygD7XgmO79excN2J%2Bt97dn48YgLyDt%2BFUHjuo%2BARu8C52EFtD52kEAKFzUD2zF30EjilzMH0DwHCHlyrXG2VwEFV3V013ekOxm3jwjz/0PzXw32f24FbzBmT14CJgjkSHp1Tw4CG1IDP32w4GwG/znztWz1z3z0LzHGL1LwgHLzRnwHnybxbwnzOw7zpiYCwGiGQK2z7wH270YNHwsHHzbxW070Hx7w4C2HTxH0/zUI7y2y8F0N4GUIMIWF0UZnSBAEkCAA"></iframe>

<small>[source: aes128-feistel.c](/images/2022-06-30-plan-b-for-uuids-double-aes-128/aes128-feistel.c)</small>

Again, we can evaluate the time it takes to encoding (or symmetrically, decoding) an internal identifier into an opaque UUID by [encoding in a loop, with a data dependency between each iteration and the next](https://godbolt.org/#z:OYLghAFBqd5QCxAYwPYBMCmBRdBLAF1QCcAaPECAMzwBtMA7AQwFtMQByARg9KtQYEAysib0QXACx8BBAKoBnTAAUAHpwAMvAFYTStJg1AB9U8lJL6yAngGVG6AMKpaAVxYMQAJg2kHAGTwGTAA5dwAjTGIQADYAZlIAB1QFQlsGZzcPb19k1JsBQOCwlkjo%2BItMKwKGIQImYgJM908fSur0uoaCItCIqNiEhXrG5uy24e7ekrLBgEoLVFdiZHYOAFIvOKDkNywAanW4xzwWFiCCYiCAOgQj7HWNAEFN7YZd1wOjx2H0LCpbvdHi8tjs9phDsdfhdAXEHs9XmDPhDvtDULD4SC3h8vlCCOhaHhwhjgYj3uDIT9LkFgCSEc9TCwuF4ABx4faJYhUYwAa0wAE8NOsAKwAIS4XBFABEjqLgYzmWyOVzeQLJWKJdLZaSEVt/kEIUJsABFOTYEKObDGACyTwAGvsoFw5P5/JTvvsYho5vsALT7LhzUl6zA0YL7EIAeQtVttDqdLrd3w9MS8Pv9gZ1T2GxFc1mVpwa/NV/NJAHY5fSAJyuC4xSTGAj7JQAR1cjFW2urtcE9cb%2BwYAk7cUrLzLMpHWZzeabmFUBCizFoxjw6HLo8eNYuLP74X5C4UIvFMS1k4R467L2ekxsyH2PYIfabnKoEAfT/24UIClI%2BzQDGGfYFVZdk%2BVLMUbzwO9NWFKUg3PDcNCrYClQXFhEkhKUgLOYwlAIYxMESPB61UCBfE/b8gzPK8ryrNCMKOLDjBw1QSFwvBFQgejfzAo8hVgqjEKxTkmGAFgmH2ABxRxHHvBhiBcWgAyFat%2BGIR1UgAL0wft2UYgNZX2PTjmUwy8E2OUvFFeDaM3ejMOwlhjCYTAlHedjOO4/ZeLFcyBMvHU6MwdCHOYpyXLc3YmGGDzWS44LEh4gUjy4fi4ICpDiEwAhlgYfZ6L408Nwvels3qW9m0uGd9jnBdiCXFd0H2NTxPwjsMEwCBp3zTlC2IYswI5HkbPWCtAtcBh0kOMbq03brZ3nRcxEamrVwyqs5qqnqrnE/qSyG9bRqw%2Bz9NGxCNqQ65Eh5BzrtIQKjoy2j6Kunlrlbdt3hRYUHjiLCX3i9DXuuQcvt/F8S29Q4vBifYjVNc1LRte11pe66QaHb7fv%2BrlAcSYGPo7TBwZVMDA2h2GoxjZG7XWrKcvq/KEuuTA1uox7StMJgCGpcJXAXUwoEHIJCWCOYfXmgtdoGgUaveDrjFoVBUESLqtufHai32u7mzwbT%2ByyxJMG5kaZtoiaprOh6kMl3rpe1nlDpt9WasW%2BrltXVa13ZpCjsq7mUT%2B6bzs3V7bp5e7ZovH3aLUjS9Z0ptjKwoURyMyl9kN42CDMiyLJG33nYDlnPf0lruYI%2BWsDV4vrsErNN3p3L/YXV6ApKmjILvEWDXvOsGxz55gGyxsoJ5BRjEiYAgjffv%2BwAKjQdC6CiSeGiuKJTfOh84i8fsld/BBzJjzdopYfYADcXG5leIE2LxkESWs12FRwGBFRwc68LxAv2P//4Af/e%2BxB0AEAUMge%2Bv9AHQP2CAaGP8/pMHvhAJWCx4GMTXN/CAR80GQKssQZBi9UDL3oMQNexAN7EDmCAEAeDhTEHCKoe%2Bv46ErCYd/SOtEYHQPvmwFgJBSwcPgcgCB3967VibozKA74B5zCPu6Eyu8fSjVkkrduMpSpdyMgwUWEIZGNmBMPfCt5x6V3QBAK%2Bq4t7jQuLvfeqBD7HyEqfBQ58r4GBsPQO%2B38QFgMfh/d%2Br8v4/2rNwwBvDUAX2hsKTYwpjaqBYV4GJYo1GwQCR/YJUCwl/wiVE2JsTMDoASdEo8R8tSv0CZ/SBoTsk5O/o/Z%2B1SuG1NgYcFJqBpToL%2BgQrBqCWFijKbBLpUoeleGwXgahtDv6xOIEwdhXhEk/ySSA%2BZiyZmMOYeg5ZyBVlZOybw4KAjNn3xEffcRtFJF5WkXPAgcjjKOA9Eo6aqiOnsw7sCLRFw/zoWMK4esEB/yAUsU1eeF9imAqbMC/YoL%2BTWOrBCvuvYB7QtUA5MF60EX6KbPPfkaLSwnyQngKgjp56osYvpHFBcLpVkufsVO51aUQFJRnSlkIABifouCtMlG8jRNFXj6nDCEAAStgZQykNCSpUliQVEIQgAEkAAq2BhUSulcCL54kZ5wtolorFNUDCJCUC/UUIqxVFRsYIHcTYfKii0pgVAr4IZgShhZXW2lHUQGdWqIMaSCVVjjl1BOukHL0vTh6e1nqwKCSMnnKyVLNy2r8lhfSTiG5IT4Y/fk3iYjesFIkmIYFfyRqdaTAU3pzmbkzYkbNmxc1lv5FwAttqS1eobd6aUxaE6erzRWp6m5A32pDfpMN9yIyiuULnKy%2Bdg7WyrLbTWe1BrXQcmdIyv49LR2cYXLcSLdyYGnnlfSRjR7IFMVPGedb0aEy%2BpWndK79LtSwIrZWqsdYKuVcKu91Kz7eKWV4VpeDRQsGQXXb9m59UOAciekxE8HAQDvbZJCVQmBGsKUeZNDkoP%2Bgve/H2G13nVhbAoEgBAIAobQ%2BgLtHrXwUeNT6AA9O6h1tHDXGsKgJThm5oGtro%2BhsUaU0HIB%2BX8yQiHNy9UEK%2BOh1w4jEtibJ%2BTSTFPpI4Xs/%2BEAuDXA0NCg1qH2NijNeKpjqU0r7CYx%2BlVCx1N/009p3TfGTVGfM/sH%2BQyLNKqs1xpCMC7M6fnnpyjR5nMZj9OOsVLnTOdI85%2B8WdNsrN3peWPlHAFi0E4MKXgngOBaFIKgTgskSPLFWNDOIPBSAEE0KlhYPIQDCl8OljgkgstVby5wXgCgQC%2BEqzl1LpA4CwCQEvIipDyCUGGyvaIyBgDMibTQWgdVOsQHCK1r8zB%2BqcHK0vNgghIw6P5K1rA4kjDiF66QfAWVrB4Avq5Vrc5MDIH5msXLFwqitcJOEWZ/VnBYFa9SFgm2%2BtUAMMABQAA1PAmAADukYjbZfK/wQQIgxDsCkDIQQigVDqDO7oJtBgjAgFMMYcwH3OuQAWCrGonWOC%2BkjAB304lXIdaqA9mo9gGBOBcC0PQAQDQzAGE2vIaQBBjE8ILlIwuGDTH6NEJtlhWedBGE0Ln2Q5cs6uwILojRpelAFxYJXou9CTG13zmXEgFhFZWKjirWU1g8DSxllrZ38scFUCyGIvp6z7GACIgMXhrhcogI4X8uBCAkFK02/YzgSFRAj3MXgPWtDi1ILV%2Br%2BhODNdIAD0zpBsu5Zdx1rrFWqvJ8a14J3%2Bf2vF968nm7xBUh2EkEAA%3D%3D)
<small>([source: aes128-feistel-latency.c](/images/2022-06-30-plan-b-for-uuids-double-aes-128/aes128-feistel-latency.c))</small>.

The format-preserving Feistel network essentially does double the work of a plain AES-128 encryption, with a serial dependency between the two AES-128 evaluations.
We expect roughly twice the latency, and [uiCA agrees](https://uica.uops.info/?code=.L8%3A%0D%0A%20%20%20%20%20%20%20%20movq%20%20%20%20xmm0%2C%20rdx%0D%0A%20%20%20%20%20%20%20%20add%20%20%20%20%20rcx%2C%201%0D%0A%20%20%20%20%20%20%20%20pxor%20%20%20%20xmm0%2C%20xmm15%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm14%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm13%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm12%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm11%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm10%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20XMMWORD%20PTR%20%5Brsp-72%5D%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20XMMWORD%20PTR%20%5Brsp-56%5D%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20XMMWORD%20PTR%20%5Brsp-40%5D%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20XMMWORD%20PTR%20%5Brsp-24%5D%0D%0A%20%20%20%20%20%20%20%20aesenclast%20%20%20%20%20%20xmm0%2C%20XMMWORD%20PTR%20%5Brsp-120%5D%0D%0A%20%20%20%20%20%20%20%20movq%20%20%20%20rax%2C%20xmm0%0D%0A%20%20%20%20%20%20%20%20and%20%20%20%20%20rax%2C%20r9%0D%0A%20%20%20%20%20%20%20%20xor%20%20%20%20%20rdi%2C%20rax%0D%0A%20%20%20%20%20%20%20%20movq%20%20%20%20xmm0%2C%20rdi%0D%0A%20%20%20%20%20%20%20%20pxor%20%20%20%20xmm0%2C%20xmm9%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20XMMWORD%20PTR%20%5Brsp-104%5D%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20XMMWORD%20PTR%20%5Brsp-88%5D%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm8%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm7%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm6%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm5%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm4%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm3%0D%0A%20%20%20%20%20%20%20%20aesenc%20%20xmm0%2C%20xmm2%0D%0A%20%20%20%20%20%20%20%20aesenclast%20%20%20%20%20%20xmm0%2C%20xmm1%0D%0A%20%20%20%20%20%20%20%20movq%20%20%20%20rax%2C%20xmm0%0D%0A%20%20%20%20%20%20%20%20and%20%20%20%20%20rax%2C%20rsi%0D%0A%20%20%20%20%20%20%20%20xor%20%20%20%20%20rdx%2C%20rax%0D%0A%20%20%20%20%20%20%20%20cmp%20%20%20%20%20r8%2C%20rcx%0D%0A%20%20%20%20%20%20%20%20jne%20%20%20%20%20.L8&syntax=asIntel&uArchs=ICL&tools=uiCA&alignment=0&uiCAHtmlOptions=traceTable&uiCAHtmlOptions=graph):
78 cycles/format-preserving encoding on Ice Lake (compared to 36 cycles for AES-128 of 16 bytes).

On my unloaded 2 GHz EPIC 7713, I observe 98 cycles/format-preserving encoding (compared to 50 cycles for AES-128 of 16 bytes), and 26.5 ns/format-presering encoding when boosting a single active core (13.5 ns for AES-128).

Still much faster than a syscall, and, although twice as slow as AES-128 of one 16 byte block, not that slow: somewhere between a L3 hit and a load from RAM.

Sortable internal ids, pseudo-random external ids: not not fast
---------------------------------------------------------------

With hardware-accelerated [AES-128]((https://en.wikipedia.org/wiki/Advanced_Encryption_Standard)) ([SipHash](https://en.wikipedia.org/wiki/SipHash) or [Blake3](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE3) specialised for 8-byte inputs should be slower, but not unreasonably so), converting between structured 128-bit ids and opaque UUIDs takes less than 100 cycles on contemporary x86-64 servers... faster than a load from main memory!

This post only addressed the question of runtime performance.
I think the real challenges with encrypting external ids aren't strictly technical in nature, and have more to do with making it hard for programmers to accidentally leak internal ids.
I don't know how that would go because I've never had to use this trick in a production system, but it seems like it can't be harder than doing the same in a schemas that have explicit internal primary keys and external ids on each table.
I'm also hopeful that one could do something smart with views and user-defined types.

Either way, I believe the runtime overhead of encrypting and decrypting 128-bit identifiers is a non-issue for the vast majority of database workloads.
Arguments against encrypting structured identifiers should probably focus on system complexity, key management (e.g., between production and testing environments), and graceful failure in the face of [faulty hardware](https://sigops.org/s/conferences/hotos/2021/papers/hotos21-s01-hochschild.pdf#page=3) or code accidentally leaking internal identifiers.

<small>Thank you Andrew and Ruchir for helping me clarify this post.</small>

<p><hr style="width: 50%"></p>
