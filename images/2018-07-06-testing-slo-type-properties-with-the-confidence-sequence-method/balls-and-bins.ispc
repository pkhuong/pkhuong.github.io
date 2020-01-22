#ifndef MAX_BUCKET_COUNT
#define MAX_BUCKET_COUNT ((1UL << 16) - 16)
#endif

typedef unsigned int32 uint32;
typedef unsigned int64 uint64;

static int32 simulate(uniform int32 bucket_count,
                      uniform int32 bucket_size,
                      uniform int32 base_seed,
                      varying int32 task_index);

static task void balls_and_bins_worker(uniform int32 bucket_count,
                                       uniform int32 bucket_size,
                                       uniform int32 base_seed,
                                       uniform int32 count,
                                       uniform int32 n_balls_placed[]) {
        foreach (index = 0 ... count) {
                n_balls_placed[index] = simulate(bucket_count, bucket_size,
                                                 base_seed, index);
        }
}

export uniform int32
balls_and_bins(uniform int32 warp_size,
               uniform int32 bucket_count,
               uniform int32 bucket_size,
               uniform int32 base_seed,
               uniform int32 count,
               uniform int32 n_balls_placed[]) {
        if (bucket_count > MAX_BUCKET_COUNT) {
                return -1;
        }

#if 0
        foreach (index = 0 ... count) {
                n_balls_placed[index] = simulate(bucket_count, bucket_size,
                                                 base_seed, index);
        }
#else
        if (warp_size <= 0) {
                warp_size = (count + 255) / 256;
                if (warp_size < 16) {
                        warp_size = 8;
                } else if (warp_size < 32) {
                        warp_size = 16;
                } else {
                        warp_size = 32;
                }
        }

        warp_size = (warp_size + 7) & ~7;
        for (uniform int32 i = 0; i < count; i += warp_size) {
                uniform int32 task_size = warp_size;
                if (i + task_size > count) {
                        task_size = count - i;
                }

                launch balls_and_bins_worker(bucket_count,
                                             bucket_size,
                                             base_seed * count + i,
                                             task_size,
                                             &n_balls_placed[i]);
        }
#endif

        return bucket_count;
}

static inline uint64 splitmix(uint64 x) {
	uint64 z = (x += 0x9e3779b97f4a7c15ULL);
	z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
	z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
	return z ^ (z >> 31);
}

// xoshiro128+ http://xoshiro.di.unimi.it/xoshiro128plus.c
struct xoshiro_state {
        uint32 s[4];
};

static void xoshiro_init(varying int32 i, varying int32 base,
                         varying struct xoshiro_state *state) {
        uint64 x1 = splitmix(i ^ base);
        uint64 x2 = splitmix(x1) ^ base;

        state->s[0] = (uint32)x1;
        state->s[1] = x1 >> 32;
        state->s[2] = (uint32)x2;
        state->s[3] = x2 >> 32;
}

static inline uint32 rotl(const uint32 x, int k) {
	return (x << k) | (x >> (32 - k));
}

static inline uint32 xoshiro(varying struct xoshiro_state *state) {
        uint32 result = state->s[0] + state->s[3];

        uint32 t = state->s[1] << 9;
        state->s[2] ^= state->s[0];
        state->s[3] ^= state->s[1];
	state->s[1] ^= state->s[2];
	state->s[0] ^= state->s[3];

	state->s[2] ^= t;

	state->s[3] = rotl(state->s[3], 11);
        return result;
};

int32 simulate(uniform int32 bucket_count,
               uniform int32 bucket_size,
               uniform int32 base_seed,
               varying int32 task_index) {
        int32 buckets[MAX_BUCKET_COUNT];
        struct xoshiro_state state;

        for (int i = 0; i < MAX_BUCKET_COUNT; i++) {
                buckets[i] = 0;
        }

        xoshiro_init(task_index, base_seed, &state);
        uniform int32 max_i = bucket_size * bucket_count;
        uniform int32 next_check = 1000;

        uniform int32 multiplier = bucket_count * 2;
        varying int32 last_ok = max_i;
        for (uniform int i = 0; i < max_i;) {
#define BODY do {                                                       \
                        int64 rnd = xoshiro(&state) & ((1ULL << 31) - 1); \
                        int32 bucket = (rnd * multiplier) >> 32;        \
                        int32 current = buckets[bucket]++;              \
                                                                        \
                        i++;                                            \
                        int32 update = (current >= bucket_size) ? i : last_ok; \
                        last_ok = min(update, last_ok);                 \
                } while (0)

                BODY;
                BODY;
                BODY;
                BODY;
#undef BODY
                           
                if (i >= next_check) {
                        next_check = 3 * (next_check / 2) + 1;
                        if (all(last_ok != max_i)) {
                                break;
                        }
                }
        }

        return last_ok;
}
