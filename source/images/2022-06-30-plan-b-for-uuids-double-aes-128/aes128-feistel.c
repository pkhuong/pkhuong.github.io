#define _GNU_SOURCE
#include <assert.h>
#include <limits.h>
#include <immintrin.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

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

/* Truncated AES-128 as a PRF from u64 to u64. */
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

/*
 * Encrypts `pk` while ensuring the first (little-endian) u64 field in
 * the encrypted result is at most SEQUENCE_MAX, and the second* at
 * most NONCE_MAX.
 *
 * The input `pk` must satisfy the same range constraints.
 */
struct external_id format_encode(struct primary_key pk)
{
	union {
		struct external_id eid;
		struct primary_key pk;
	} temp = {
		.pk = pk,
	};

	assert(temp.pk.sequence <= SEQUENCE_MAX);
	assert(temp.pk.nonce <= NONCE_MAX);

	temp.pk.sequence ^= prf(temp.pk.nonce, prf_key0) & SEQUENCE_MAX;
	temp.pk.nonce ^= prf(temp.pk.sequence, prf_key1) & NONCE_MAX;

	assert(temp.pk.sequence <= SEQUENCE_MAX);
	assert(temp.pk.nonce <= NONCE_MAX);

	return temp.eid;
}

/*
 * Inverts `format_encode`.
 */
struct primary_key format_decode(struct external_id eid)
{
	union {
		struct external_id eid;
		struct primary_key pk;
	} temp = {
		.eid = eid,
	};

	temp.pk.nonce ^= prf(temp.pk.sequence, prf_key1) & NONCE_MAX;
	temp.pk.sequence ^= prf(temp.pk.nonce, prf_key0) & SEQUENCE_MAX;

	return temp.pk;
}

static ssize_t getrandom(void *buf, size_t buflen, int flags)
{
	return syscall(SYS_getrandom, buf, buflen, flags);
}

static void
hexdump(const char *prefix, const void *data, size_t n, bool newline)
{
	const unsigned char *bytes = data;

	if (prefix != NULL)
		printf("%s", prefix);

	for (size_t i = 0; i < n; i++)
		printf("%02x", bytes[i]);

	if (newline)
		printf("\n");
	return;
}

int
main()
{
	{
		ssize_t got;

		got = getrandom(&prf_key0, sizeof(prf_key0), /*flags=*/0);
		if (got != sizeof(prf_key0))
			abort();

		got = getrandom(&prf_key1, sizeof(prf_key1), /*flags=*/0);
		if (got != sizeof(prf_key1))
			abort();
	}

	hexdump("prf_key0: ", &prf_key0, sizeof(prf_key0), /*newline=*/true);
	hexdump("prf_key1: ", &prf_key1, sizeof(prf_key1), /*newline=*/true);
	struct primary_key pk = { 1, 4 };
	struct external_id encoded = format_encode(pk);
	struct primary_key decoded = format_decode(encoded);

	printf("pk: %"PRIu64", %"PRIu64"\n", pk.sequence, pk.nonce);

	hexdump("encoded: ", &encoded, sizeof(encoded), /*newline=*/true);

	printf("decoded: %"PRIu64", %"PRIu64"\n", decoded.sequence, decoded.nonce);
	return 0;
}
