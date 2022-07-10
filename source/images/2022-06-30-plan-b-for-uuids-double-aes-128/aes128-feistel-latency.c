#include <immintrin.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

__m128i prf_key0[11];
__m128i prf_key1[11];

#define SEQUENCE_MAX ((1ULL << 60) - 1)
#define NONCE_MAX ((1ULL << 62) - 1)

struct primary_key
{
	uint64_t sequence;
	uint64_t nonce;
};

struct external_id
{
	uint8_t bytes[16];
};

static uint64_t prf(uint64_t bits, const __m128i key[static 11])
{
	__m128i temp = _mm_set_epi64x(0, bits);

	temp = _mm_xor_si128(temp, key[0]);

#pragma GCC unroll 10
	for (size_t i = 1; i < 10; i++)
		temp = _mm_aesenc_si128(temp, key[i]);

	temp = _mm_aesenclast_si128(temp, key[10]);
	return temp[0];
}

static struct external_id format_encode(struct primary_key pk)
{
	union {
		struct external_id eid;
		struct primary_key pk;
	} temp = {
		.pk = pk,
	};

	temp.pk.sequence ^= prf(temp.pk.nonce, prf_key0) & SEQUENCE_MAX;
	temp.pk.nonce ^= prf(temp.pk.sequence, prf_key1) & NONCE_MAX;
	return temp.eid;
}

__attribute__((noinline)) struct primary_key
encode_loop(struct primary_key pk, size_t repeat)
{
	union {
		struct primary_key pk;
		struct external_id eid;
	} state = {
		.pk = pk,
	};

	for (size_t i = 0; i < repeat; i++)
		state.eid = format_encode(state.pk);

	return state.pk;
}

static inline uint64_t
get_ticks_begin(uint64_t *compiler_barrier)
{
	uint32_t lo, hi;

	asm volatile("cpuid\n\t"
	             "rdtsc"
	             : "=a"(lo), "=d"(hi), "+r"(*compiler_barrier)::"%rbx", "%rcx",
	             "memory", "cc");
	return ((uint64_t)hi << 32) | lo;
}

static inline uint64_t
get_ticks_end(void)
{
	uint32_t lo, hi;

	asm volatile("rdtscp\n\t"
	             "mov %%eax, %[lo]\n\t"
	             "mov %%edx, %[hi]\n\t"
	             "cpuid"
	             : [lo] "=r"(lo), [hi] "=r"(hi)::"%rax", "%rdx", "%rbx", "%rcx",
	             "memory", "cc");
	return ((uint64_t)hi << 32) | lo;
}

static int cmp_u64(const void *vx, const void *vy)
{
	const uint64_t *x = vx;
	const uint64_t *y = vy;

	if (*x == *y)
		return 0;
	return (*x < *y) ? -1 : 1;
}

#define NREP 100000
#define NITER 1000

int main()
{
	static uint64_t elapsed[NREP];
	uint8_t key[sizeof(prf_key0) + sizeof(prf_key1)];

	for (size_t i = 0; i < sizeof(key); i++)
		key[i] = i;

	memcpy(&prf_key0, &key, sizeof(prf_key0));
	memcpy(&prf_key1, &key[sizeof(prf_key0)], sizeof(prf_key0));

	for (size_t i = 0; i < NREP; i++) {
		struct primary_key pk = { i, i };

		uint64_t begin = get_ticks_begin(&pk.sequence);
		pk = encode_loop(pk, NITER);
		asm("" : "+m"(pk));
		uint64_t end = get_ticks_end();

		elapsed[i] = end - begin;
	}

	qsort(elapsed, sizeof(elapsed) / sizeof(elapsed[0]),
	      sizeof(elapsed[0]), cmp_u64);

	printf("%.3f %.3f %.3f\n",
	       (1.0 * elapsed[NREP / 100] / NITER),
	       (1.0 * elapsed[NREP / 2] / NITER),
	       (1.0 * elapsed[NREP - 1 - NREP / 100] / NITER));
	return 0;
}
