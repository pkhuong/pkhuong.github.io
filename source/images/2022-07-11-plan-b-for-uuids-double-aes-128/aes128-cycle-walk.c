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

static struct external_id encode(struct primary_key pk)
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

static struct primary_key decode(struct external_id eid)
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

static bool is_valid_pkey(struct primary_key pk)
{
	return pk.sequence <= INT64_MAX;
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

	return temp.eid;
}

struct primary_key decode_with_zero_bit(struct external_id eid)
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

static ssize_t getrandom(void *buf, size_t buflen, int flags)
{
	return syscall(SYS_getrandom, buf, buflen, flags);
}

void derive_decryption_key(void)
{
	decryption_key[10] = encryption_key[0];
	for (size_t i = 1; i < 10; i++)
		decryption_key[10 - i] = _mm_aesimc_si128(encryption_key[i]);
	decryption_key[0] = encryption_key[10];
	return;
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

		got = getrandom(&encryption_key, sizeof(encryption_key), /*flags=*/0);
		if (got != sizeof(encryption_key))
			abort();
	}

	derive_decryption_key();

	hexdump("encryption key: ",
	        &encryption_key, sizeof(encryption_key), /*newline=*/true);
	hexdump("decryption key: ",
	        &decryption_key, sizeof(decryption_key), /*newline=*/true);

	struct primary_key pk = { 1, 4 };
	struct external_id encoded = encode(pk);
	struct external_id encoded_with_zero = encode_with_zero_bit(pk);
	struct primary_key decoded = decode(encoded);
	struct primary_key decoded_with_zero = decode_with_zero_bit(encoded_with_zero);

	printf("pk: %"PRIu64", %"PRIu64"\n", pk.sequence, pk.nonce);

	hexdump("encoded: ", &encoded, sizeof(encoded), /*newline=*/true);
	hexdump("encoded with zero: ",
	        &encoded_with_zero, sizeof(encoded_with_zero), /*newline=*/true);

	printf("decoded: %"PRIu64", %"PRIu64"\n", decoded.sequence, decoded.nonce);
	printf("decoded with zero: %"PRIu64", %"PRIu64"\n",
	       decoded_with_zero.sequence, decoded_with_zero.nonce);
	return 0;
}
