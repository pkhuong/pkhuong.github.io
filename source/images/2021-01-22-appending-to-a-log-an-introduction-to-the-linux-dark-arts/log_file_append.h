#pragma once
#define _GNU_SOURCE

/*
 * Copyright 2021 Backtrace I/O, Paul Khuong
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

/**
 ** ## Log file append: a single file library for multi-process logging
 **
 ** Log file append implements a multi-process / multi-threaded log
 ** writing protocol on top of regular Linux files.  Writes are
 ** wait-free, reads lock-free (and only fail when the reader fell
 ** behind), and periodic maintenance mostly acquires locks
 ** opportunistically, without blocking.
 **
 ** This is a single-file library; `#define LOG_FILE_APPEND_IMPL` in
 ** *one file* to compile the definitions for `log_file_open`,
 ** `log_file_maintain` and `log_file_append`.  If you only use
 ** `log_file_append` in that one file, you may also `#define
 ** LOG_FILE_APPEND_LINKAGE static` to declare these three functions
 ** with `static` file scope.
 **/
#ifndef LOG_FILE_APPEND_LINKAGE
#define LOG_FILE_APPEND_LINKAGE
#endif

/**
 ** ### Parameters for periodic maintenance
 **
 ** Periodic maintenance always flushes data explicitly to bound the
 ** amount of written bytes buffered by the kernel.  Maintenance
 ** usually includes erasing old data, and may also compact empty
 ** ranges in the log file.
 **/

/**
 * Write calls aggressively initiate flushing for newly written data
 * in `alignment` blocks: we don't expect to overwrite log data, and
 * we expect logs to stick around so there's no point in buffering it.
 * When we do so, we also check that the tail `queue_size` bytes behind
 * our write head has reached persistent storage before letting writes
 * resume: this backpressure mechanism lets us bound the amount of
 * write buffering for each log file.
 */
struct log_file_flush_params {
        /* Number of bytes the kernel is allowed to buffer (+ alignment). */
        uint64_t queue_size;
        /*
         * Flush blocks at `alignment` boundaries.  Must be a power of
         * 2, and should be at most `queue_size / 4`: when the
         * alignment is too close to the target queue size, we are
         * unable to control the queue smoothly.
         *
         * 0 defaults to 1 MB.
         */
        uint64_t alignment;
};

/**
 * From time to time, write calls can also erase old data.  We define
 * "old" data in terms of byte counts, and aim to only keep the last
 * `retention_bytes` of data in a log file.
 */
struct log_file_erase_params {
        /* Number of bytes to keep.  0 to disable periodic erasure of old data. */
        uint64_t retention_bytes;
        /*
         * Erase blocks at `alignment` boundaries.  Must be a power of 2,
         * and should be at most equal to `retention_bytes`.
         */
        uint64_t alignment;
};

/**
 * When periodic maintenance can erase data, we can also collapse away
 * the growing hole at the head of the file, in `granularity`
 * increments.  Using a fixed collapsing increment
 * `shrink.granularity` makes it possible for readers to reposition
 * their read cursor with a bit of modular arithmetic after finding
 * that a log file has shrunk.
 */
struct log_file_shrink_params {
        /* Shrink log files in `alignment` increments.  0 to disable. */
        uint64_t granularity;
};

struct log_file_append_params {
        /*
         * The maintenance frequency is the inverse of the number of
         * bytes we want to write on average before engaging periodic
         * maintenance, including flushing.
         *
         * For example, setting the maintenance frequency to 1.0 /
         * (128 * 1024) will trigger a flush roughly every 128 KB.
         *
         * Zero defaults to `10.0 / flush.alignment`; this default value
         * guarantees that that we will miss a full `alignment` block
         * of flush calls with probability less than `exp(-10.0) < 1e-4`.
         */
        double maintenance_frequency;
        struct log_file_flush_params flush;
        struct log_file_erase_params erase;
        struct log_file_shrink_params shrink;
};

/**
 * The info struct determines what maintenance operations the log
 * writer *can* perform, and is updated as we learn that the
 * filesystem does not support some operations.
 *
 * Should be zero-initialised (all false), unless you want a big
 * hammer to disable the log rotation code.
 */
struct log_file_append_info {
        atomic_bool erase_broken;
        atomic_bool shrink_broken;
};

/**
 ** ### What to do with a log file: open a log and append to it
 **/

/**
 * Opens a write-only log file at `dirfd/path`.  See `openat` for more
 * information on `dirfd`, `path`, and `mode`.
 *
 * @param info if provided, updated with any missing feature for the newly
 *   opened file.
 * @param params if provided, force immediate maintenance work on the
 *   newly opened file.
 *
 * @return the new file descriptor on success, -1 on error.
 */
LOG_FILE_APPEND_LINKAGE
int log_file_open(int dirfd, const char *path, mode_t mode,
    struct log_file_append_info *info,
    const struct log_file_append_params *params);

/**
 * Applies the maintenance rule to `log_fd`.
 *
 * @param force whether to block when the log file is already locked
 *   by a concurrent maintainer.
 */
LOG_FILE_APPEND_LINKAGE
void log_file_maintain(int log_fd, struct log_file_append_info *info,
    bool force, const struct log_file_append_params *params);

/**
 * Appends `num` bytes from `data` to the end of `log_fd`, while
 * performing periodic maintenance tasks on `log_fd` as defined
 * by the `log_file_append_params`.
 *
 * @param log_fd a log file descriptor returned by `log_file_open`;
 *    the caller must have exclusive ownership over the file descriptor
 *    as well as the described file object.
 * @param info if provided, an INOUT parameter to track filesystem
 *   features that are not supported by the system.  Can be associated
 *   with a file descriptor, or have one per thread / process.
 * @param params if provided, enables and defines periodic maintenance
 *   work on `log_fd`.
 *
 * @return the offset one past the end of the newly-written `data`,
 *   or -1 on error.
 */
LOG_FILE_APPEND_LINKAGE
off_t log_file_append(int log_fd, struct log_file_append_info *info,
    const struct log_file_append_params *params,
    const void *data, size_t num);

/**
 ** ## Actual implementation
 **
 **{ Implementation headers
 **/
#ifdef LOG_FILE_APPEND_IMPL
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <math.h>
#include <pthread.h>
#include <sys/file.h>
#include <sys/random.h>
#include <sys/stat.h>
#include <unistd.h>

/**
 ** ### Thread-safe file locking
 **
 ** BSD `flock`s work at process granularity, and do weird things when
 ** a process acquires a lock on the same file, even through different
 ** descriptors.  We protect access to the `flock` API with an array
 ** of local pthread mutexes keyed on inodes to avoid this misfeature.
 **/

/**
 * Maps the inode described by `st` to a statically allocated mutex.
 *
 * This mutex must be acquired *around* file locks: otherwise, we
 * could end up acquiring a lock for the same file twice, and that
 * will succeed silently, but without the semantics anyone wants.
 */
static pthread_mutex_t *
file_flock_mutex(const struct stat *st)
{
        enum { NUM_MUTEX = 128 };
        static pthread_mutex_t flock_mutex[NUM_MUTEX] = {
                [0 ... NUM_MUTEX - 1] = PTHREAD_MUTEX_INITIALIZER,
        };
        __uint128_t slot;
        uint64_t h = st->st_dev;
        uint64_t k = st->st_ino;
        /*
         * We mix the inode and device together with the core of
         * MurmurHash64A by Austin Appleby.
         *
         * The device isn't mixed as heavily as the inode because we
         * expect many more inodes than devices.
         */
        const uint64_t m = 0xc6a4a7935bd1e995ULL;
        const int r = 47;

        k *= m;
        k ^= k >> r;
        k *= m;

        h ^= k;
        h *= m;

        /*
         * Map the high bits to a mutex index: there's more entropy
         * there.
         */
        slot = NUM_MUTEX;
        slot *= h;
        return &flock_mutex[slot >> 64];
}

/**
 * Acquires an exclusive lock on `fd`, with inode described by `st`.
 */
static void
file_flock_lock(int fd, const struct stat *st)
{
        pthread_mutex_t *mutex;

        mutex = file_flock_mutex(st);
        pthread_mutex_lock(mutex);

        /* This will loop forever if something's wrong with `fd`. */
        while (flock(fd, LOCK_EX) != 0)
                ;
        return;
}

/**
 * Attempts to acquire an exclusive lock on `fd`, with inode described by `st`.
 *
 * @return true on success, false on failure.
 */
static bool
file_flock_trylock(int fd, const struct stat *st)
{
        pthread_mutex_t *mutex;

        mutex = file_flock_mutex(st);
        if (pthread_mutex_trylock(mutex) != 0)
                return false;

        if (flock(fd, LOCK_EX | LOCK_NB) == 0)
                return true;

        pthread_mutex_unlock(mutex);
        return false;
}

/**
 * Releases an exclusive lock on `fd`, with inode described by `st`.
 */
static void
file_flock_unlock(int fd, const struct stat *st)
{
        pthread_mutex_t *mutex;

        while (true) {
                int r;

                r = flock(fd, LOCK_UN);
                if (r == -1 && errno == EINTR)
                        continue;

                /*
                 * Either we succeeded or something went horribly
                 * wrong.  Just let it go.
                 */
                break;
        }

        mutex = file_flock_mutex(st);
        pthread_mutex_unlock(mutex);
        return;
}

/**
 ** ### Probabilistic counting with `xoshiro256+`
 **
 ** Each thread has a private writer state, which only consists of two
 ** things: a PRNG state, and a countdown value sampled from the
 ** standard Exponential distribution with mean (lambda) = 1.
 **
 ** Every time we write to a log file, the thread will decrement a
 ** suitably scaled value from its countdown until the result crosses
 ** zero.  When the countdown is zero or negative, it's time to
 ** trigger maintenance for the file descriptor.  This process
 ** simulates a memoryless Negative Binomial for each byte written to
 ** a log, so doesn't need to attach any state to log files... a nifty
 ** property when independent processes write to the same file.
 **/

/**
 * This state is only needed for probabilistic counting, and is
 * independent of the number of files being written to.  It's also
 * statistically correct for both short- and long-lived threads.
 */
struct writer_state {
        /* xoshiro256+ random state.  Must be initialised when all zero. */
        uint64_t random_state[4];
        /*
         * Sampled from exp(1). Must be initialised when zero.
         *
         * You might be worried about round-off, but this counter
         * never exceeds -ln(1.0 / UINT64_MAX) < 45.
         */
        double countdown;
};

/**
 * Xoshiro256+
 *
 * Written in 2018 by David Blackman and Sebastiano Vigna (vigna@acm.org)
 *
 * To the extent possible under law, the author has dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide. This software is distributed without any warranty.
 *
 * See <http://creativecommons.org/publicdomain/zero/1.0/>.
 */
static uint64_t
xoshiro256_plus(struct writer_state *state)
{
        const uint64_t result = state->random_state[0] + state->random_state[3];

        const uint64_t t = state->random_state[1] << 17;

        state->random_state[2] ^= state->random_state[0];
        state->random_state[3] ^= state->random_state[1];
        state->random_state[1] ^= state->random_state[2];
        state->random_state[0] ^= state->random_state[3];

        state->random_state[2] ^= t;

#define rotl(x, k) ((x << k) | (x >> (64 - k)))
        state->random_state[3] = rotl(state->random_state[3], 45);
#undef rotl
        return result;
}

static void
seed_state(struct writer_state *state)
{
        ssize_t r;

        do {
                /* This will loop forever on an old kernel. */
                r = getrandom(state->random_state, sizeof(state->random_state),
                    /*flags=*/0);
        } while (r != sizeof(state->random_state));

        return;
}

/**
 * Computes a new value for `state->countdown` by sampling from the
 * standard Exponential distribution.
 */
static void
update_countdown(struct writer_state *state)
{
        uint64_t u64;

        u64 = xoshiro256_plus(state);
        /* We always want to retry on zero, because infinity is bad. */
        while (u64 == 0) {
                /*
                 * xoshiro256+ returns 0 for the invalid zero-filled
                 * state.  We can check if that's what happened (very
                 * likely), and initialise the state if so.
                 */
                if (state->random_state[0] == 0 &&
                    state->random_state[1] == 0 &&
                    state->random_state[2] == 0 &&
                    state->random_state[3] == 0)
                        seed_state(state);

                u64 = xoshiro256_plus(state);
        }

        /*
         * Convert our pseudorandom uniformly distributed u64 into a
         * double in (0, 1], and fudge the inverse CDF for exp(1) up
         * by a tiny bit to make sure it's non-zero.  In practice, the
         * DBL_EPSILON is so small (2**-53) that it doesn't affect our
         * maintenance rate.
         */
        state->countdown = -log(u64 * (1.0 / UINT64_MAX)) + DBL_EPSILON;
        return;
}

/**
 ** ### Base IO: writing and flushing
 **
 ** We assume individual log records are encoded in a
 ** self-synchronising format, with corruption checks and handling of
 ** duplicate data in the higher level log consumption logic.
 **
 ** This means writes can `O_APPEND` and let the kernel figure out
 ** where the bytes should go.  On failure, including short writes, we
 ** can just try again: readers will gracefully handle these rare
 ** events as invalid records.
 **/

static off_t
lseek_retry(int fd, off_t offset, int whence)
{
        off_t r;

        do {
                r = lseek(fd, offset, whence);
        } while (r == -1 && errno == EINTR);

        return r;
}

/**
 * Appends `num` bytes from `data` to `log_fd`, with `log_fd` owned
 * exclusively by the caller.
 *
 * Returns the file offset immediately after the last byte of `data`
 * on success, negative on failure.
 */
static off_t
write_and_retry(int log_fd, const void *data, size_t num)
{
        static const size_t num_attempts = 3;

        for (size_t i = 0; i < num_attempts; i++) {
                ssize_t r;

                r = write(log_fd, data, num);
                if ((size_t)r == num)
                        return lseek_retry(log_fd, 0, SEEK_CUR);

                /* Short write: try again. */
                if (r >= 0)
                        continue;

                /*
                 * EINTR: you get a free retry, otherwise people
                 * attaching a debugger to figure out what's up will
                 * cascade into more serious failures.
                 */
                if (errno == EINTR) {
                        i--;
                        continue;
                }
        }

        return -1;
}

/**
 ** Memory-less flushing is a more subtle affair.
 **
 ** We want to regularly initiate flushing of all data up to the write
 ** cursor (modulo alignment), and also verify that all "old" data has
 ** actually made it to persistent storage.  We can't rely on the
 ** filesystem having an efficient implementation of no-op
 ** `fdatasync`s, so we cap the lookback to a multiple of the flush
 ** alignment: this cap avoids quadratic scaling (albeit with a low
 ** constant factor) with respect to the physical size of the log
 ** file.
 **/
#define LOG_FILE_APPEND_DEFAULT_FLUSH_ALIGNMENT (1UL << 20)

static uint64_t
saturating_add(uint64_t x, uint64_t y)
{
        uint64_t sum = x + y;

        return (sum < x) ? UINT64_MAX : sum;
}

/**
 * Explicitly flushes the write queue of the file described by `fd`'s,
 * now that the file's size is approximately `file_size`, and after a
 * write of `written` bytes.
 */
static void
flush_pending_bytes(int fd, off_t file_size, size_t written,
    const struct log_file_flush_params *params)
{
        /*
         * Look back by one additional block, in addition to the queue size.
         *
         * With the default maintenance frequency of 10 / alignment,
         * and assuming `retention_bytes >= 4 * alignment`, we expect to
         * "miss" a byte with probability `exp(-50) < 2e-22`; that's
         * practically impossible.
         */
        static const uint64_t lookback_factor = 1;
        uint64_t alignment = params->alignment;
        uint64_t lookback_size;
        uint64_t queue_size = params->queue_size;
        off_t alignment_mask;
        off_t flush_begin;
        off_t flush_end;

        if (file_size <= 0)
                return;

        if ((off_t)alignment <= 0)
                alignment = LOG_FILE_APPEND_DEFAULT_FLUSH_ALIGNMENT;

        alignment_mask = -(off_t)alignment;
        flush_end = file_size & alignment_mask;

        /*
         * We conservatively overshoot backward by `lookback_size`
         * alignment blocks when flushing, just in case we got really
         * unlucky with the probabilistic counting.
         *
         * We could also always start from 0, but that feels like a
         * good way to break things on large files.
         */
        if (alignment <= UINT64_MAX / lookback_factor) {
                lookback_size = lookback_factor * alignment;
        } else {
                lookback_size = UINT64_MAX;
        }

        /*
         * Increase the "lookback" by the number of bytes just written
         * because we don't want to miss flushing a range of data
         * whenever we write more than the default lookback size.
         */
        lookback_size = saturating_add(lookback_size, written);

        /*
         * And that lookback is in addition to the queue size itself.
         */
        lookback_size = saturating_add(lookback_size, queue_size);

        if ((uint64_t)flush_end > lookback_size) {
                flush_begin = (flush_end - lookback_size) & alignment_mask;
        } else {
                flush_begin = 0;
        }

        if (flush_begin >= flush_end)
                return;

        /*
         * Initiate write-out of `lookback_size` bytes up to the
         * aligned end of the file, modulo alignment.
         */
        (void)sync_file_range(fd, flush_begin, flush_end - flush_begin,
            SYNC_FILE_RANGE_WRITE);

        /*
         * And now, we must block to make sure old data that was flushed
         * before this call to `flush_pending_bytes` has made it to
         * persistent storage, if there even is any such old data.
         */
        if (written >= (uint64_t)file_size)
                return;

        /*
         * We only need to block on the flush once more than
         * `queue_size` bytes have been marked for flushing.
         */
        if ((uint64_t)flush_end <= queue_size)
                return;

        flush_end -= queue_size;

        /* Avoid blocking on data we *just* marked for flushing. */
        {
                off_t previous_size = (uint64_t)file_size - written;

                if (flush_end > previous_size)
                        flush_end = previous_size;
        }

        flush_end &= alignment_mask;
        if (flush_begin >= flush_end)
                return;

        /*
         * And now wait until all but the last `queue_size` (plus
         * `alignment` slop) bytes of data have made it to storage.
         */
        (void)sync_file_range(fd, flush_begin, flush_end - flush_begin,
            SYNC_FILE_RANGE_WAIT_BEFORE | SYNC_FILE_RANGE_WRITE | SYNC_FILE_RANGE_WAIT_AFTER);
        return;
}

/**
 ** ### Log rotation mechanism
 **
 ** We rotate logs in two ways:
 **
 ** 1. We regularly punch holes in the trail of log files to
 **    remove old data from the log file's physical footprint
 **
 ** 2. Optionally, we also shrink a log file when there is a large hole
 **    at the head of the file.
 **
 ** Punching holes is nice because the operation is idempotent.
 ** Unfortunately, not all filesystems and Unix utilities deal
 ** well with large sparse file, so we sometimes have to be nice
 ** and shrink logs.  Shrinking is *not* idempotent, and also
 ** loses idempotence for hole punching, so we must then rely
 ** on file locking.
 **/

/**
 * Computes the byte offset where we want to punch a hole: every byte
 * at a file offset less than that limit can be erased.
 *
 * Returns 0 when there is nothing to punch.
 */
static off_t
erase_byte_limit(off_t file_size, size_t written,
    struct log_file_append_info *info,
    const struct log_file_erase_params *params)
{
        uint64_t retention = params->retention_bytes;
        off_t alignment_mask;

        if (file_size <= 0 ||
            retention == 0 ||
            atomic_load(&info->erase_broken) == true)
                return 0;

        /*
         * We don't want to erase what we *just* wrote, so make sure
         * to retain at least that many bytes.
         */
        if (written >= retention)
                retention = written;

        if ((uint64_t)file_size <= retention)
                return 0;

        if ((off_t)params->alignment <= 0) {
                alignment_mask = -1;
        } else {
                alignment_mask = -(off_t)params->alignment;
        }

        /*
         * Any data at offset less than the return value is safe to delete.
         */
        return (file_size - retention) & alignment_mask;
}

/**
 * Punches a hole in `fd` over all bytes before `retention_limit`.
 */
static void
erase_old_bytes(int fd, off_t retention_limit,
    struct log_file_append_info *info)
{

        if (retention_limit <= 0)
                return;

        while (true) {
                int r;

                r = fallocate(fd, FALLOC_FL_PUNCH_HOLE | FALLOC_FL_KEEP_SIZE,
                    0, retention_limit);
                if (r == 0)
                        break;

                if (errno == EINTR)
                        continue;

                if (errno == ENOSYS || errno == EOPNOTSUPP)
                        atomic_store(&info->erase_broken, true);
                break;
        }

        return;
}

/**
 * Collapses the head of the file in large increments, when the file
 * is too large.
 *
 * This operation is not idempotent, and breaks idempotence of hole
 * punching, so must only be called with a file lock on `fd`.
 */
static void
shrink_file(int fd, off_t file_size, struct log_file_append_info *info,
    uint64_t retention_bytes, const struct log_file_shrink_params *params)
{
        static const size_t n_attempts = 3;
        uint64_t shrink_granularity = params->granularity;
        uint64_t file_size_limit;
        off_t r;
        off_t to_shrink;

        if (file_size <= 0 ||
            shrink_granularity == 0 ||
            atomic_load(&info->shrink_broken) == true)
                return;

        /*
         * For any file shorter than `file_size_limit`, there's
         * definitely nothing to do.
         */
        file_size_limit = saturating_add(shrink_granularity, retention_bytes);
        if ((uint64_t)file_size < file_size_limit)
                return;

        to_shrink = file_size - retention_bytes;
        /*
         * Round down to `shrink_granularity`.
         */
        to_shrink = (to_shrink / shrink_granularity) * shrink_granularity;

        if (to_shrink == 0)
                return;

        /* Confirm there is data to remove. */
        r = lseek_retry(fd, to_shrink, SEEK_HOLE);
        /*
         * There's still data where we would like to shrink (or
         * `lseek` failed, and there's nothing we can do about that).
         * We'll wait for the retention code to delete that data, and
         * instead shrink less.
         */
        if (r != to_shrink) {
                if ((uint64_t)to_shrink <= shrink_granularity)
                        return;

                /*
                 * If the log file has grown to twice the shrink
                 * granularity, force a (less aggressive) collapse.
                 */
                to_shrink -= shrink_granularity;
        }

        for (size_t i = 0; i < n_attempts; i++) {
                r = fallocate(fd, FALLOC_FL_COLLAPSE_RANGE, 0, to_shrink);
                if (r == 0)
                        break;

                if (errno == EINTR) {
                        i--;
                        continue;
                }

                if (errno == ENOSYS || errno == EOPNOTSUPP) {
                        atomic_store(&info->shrink_broken, true);
                        break;
                }
        }

        return;
}

/**
 ** ### Assembling all the pieces together
 **/

/**
 * Performs the maintenance tasks on `fd`.
 *
 * @param fd a descriptor for the file to maintain (flush new data,
 *   erase old data, and shrink large files)
 * @param size our current guess for the size of the file
 * @param written the number of bytes we just wrote to `fd`
 * @param force whether to force an eager cleanup
 * @param[INOUT] info tracks the filesystem features we know can't be used.
 * @param params the maintenance task parameters.
 */
static void
maintain_file(int fd, off_t size, size_t written, bool force,
    struct log_file_append_info *info,
    const struct log_file_append_params *params)
{
        struct stat sb;
        off_t erase_limit;

        /* 0 probably isn't a real guess... */
        if (size <= 0)
                size = lseek_retry(fd, 0, SEEK_END);

        if (size <= 0)
                return;

        /*
         * Only flush if we don't erase data, or if we flush more
         * aggressively than we erase: when that's not the case, we'll
         * erase data before we start blocking on it being flushed,
         * and that's useless traffic.
         */
        if (params->erase.retention_bytes == 0 ||
            params->flush.queue_size < params->erase.retention_bytes)
                flush_pending_bytes(fd, size, written, &params->flush);

        erase_limit = erase_byte_limit(size, written, info, &params->erase);
        if (erase_limit <= 0)
                return;

        /* If we don't shrink, we can just punch holes without locking.  */
        if (params->shrink.granularity == 0) {
                erase_old_bytes(fd, erase_limit, info);
                return;
        }

        /*
         * Otherwise, we need to lock, so let's reduce contention with
         * double-checked locking: there's nothing to do if the data
         * just before `erase_limit` has already been deleted.
         */
        if (lseek_retry(fd, erase_limit - 1, SEEK_HOLE) == erase_limit - 1)
                return;

        /* If stat fails, we're in a bad shape. */
        if (fstat(fd, &sb) < 0)
                return;

        if (force) {
                file_flock_lock(fd, &sb);
        } else if (!file_flock_trylock(fd, &sb)) {
                return;
        }

        /*
         * We must recompute the size and erase limit with the lock
         * held: another writer may have shrunk the file.  In that
         * case it's safe to pass the same value for `written`: we only
         * use that parameter to avoid erasing data we just wrote, so
         * the worst that can happen is that this specific maintenance
         * call will delete less data than it could.
         */
        size = lseek_retry(fd, 0, SEEK_END);
        erase_limit = erase_byte_limit(size, written, info, &params->erase);
        if (erase_limit > 0) {
                erase_old_bytes(fd, erase_limit, info);
                shrink_file(fd, size, info, params->erase.retention_bytes,
                    &params->shrink);
        }

        file_flock_unlock(fd, &sb);
        return;
}

/**
 * Updates `info` with disabled `fallocate` features for `fd`.
 */
static void
check_features(int fd, off_t size, struct log_file_append_info *info)
{
        /* Get close to OFF_MAX. */
        off_t large_offset = (off_t)7 << (sizeof(off_t) < 8 ? 28UL : 60UL);

        /*
         * We check features by trying to erase data way past the end
         * of the file.  If we see a file that big, assume the file
         * system is pretty good.  At the very least, we won't have to
         * worry about hitting EFBIG because we can't shrink log files.
         */
        if (size > large_offset / 2)
                return;

        /* Can we punch holes? */
        while (atomic_load(&info->erase_broken) == false) {
                int r;

                r = fallocate(fd, FALLOC_FL_PUNCH_HOLE | FALLOC_FL_KEEP_SIZE,
                    large_offset, 1);
                if (r == 0)
                        break;

                if (errno == EINTR)
                        continue;

                if (errno == ENOSYS || errno == EOPNOTSUPP)
                        atomic_store(&info->erase_broken, true);
                break;
        }

        /* Can we collapse files? */
        while (atomic_load(&info->shrink_broken) == false) {
                int r;

                r = fallocate(fd, FALLOC_FL_COLLAPSE_RANGE, large_offset, 1);
                if (r == 0)
                        break;

                if (errno == EINTR)
                        continue;

                if (errno == ENOSYS || errno == EOPNOTSUPP)
                        atomic_store(&info->shrink_broken, true);
                break;
        }

        return;
}

LOG_FILE_APPEND_LINKAGE int
log_file_open(int dirfd, const char *path, mode_t mode,
    struct log_file_append_info *info,
    const struct log_file_append_params *params)
{
        struct log_file_append_info default_info = { false };
        off_t size;
        int ret;

        ret = openat(dirfd, path,
            O_WRONLY | O_APPEND | O_CLOEXEC | O_CREAT, mode);
        if (ret < 0 || (info == NULL && params == NULL))
                return ret;

        size = lseek_retry(ret, 0, SEEK_END);
        if (size < 0)
                return ret;

        if (info != NULL) {
                check_features(ret, size, info);
        } else {
                info = &default_info;
        }

        if (params != NULL)
                maintain_file(ret, size, 0, /*force=*/true, info, params);
        return ret;
}

LOG_FILE_APPEND_LINKAGE void
log_file_maintain(int log_fd, struct log_file_append_info *info,
    bool force, const struct log_file_append_params *params)
{
        struct log_file_append_info default_info = { false };

        if (info == NULL)
                info = &default_info;

        maintain_file(log_fd, /*size=*/0, /*written=*/0, force, info, params);
        return;
}

/**
 * Computes the default maintenance frequency, `10.0 / params->flush.alignment`.
 */
static double
derive_frequency(const struct log_file_append_params *params)
{
        uint64_t alignment;

        alignment = params->flush.alignment;
        if ((off_t)alignment <= 0)
                alignment = LOG_FILE_APPEND_DEFAULT_FLUSH_ALIGNMENT;

        /*
         * We assume `alignment` is a power of two, so
         * `alignment == 2 ** __builtin_ctzll(alignment)`.
         */
        return ldexp(10.0, -__builtin_ctzll(alignment));
}

LOG_FILE_APPEND_LINKAGE off_t
log_file_append(int log_fd, struct log_file_append_info *info,
    const struct log_file_append_params *params,
    const void *data, size_t num)
{
        static __thread struct writer_state writer_state;
        struct log_file_append_info default_info = { false };
        double maintenance_frequency = params->maintenance_frequency;
        double scaled;
        off_t ret;

        if (num == 0)
                return lseek_retry(log_fd, 0, SEEK_END);

        if (info == NULL)
                info = &default_info;

        for (size_t i = 0; i < 2; i++) {
                ret = write_and_retry(log_fd, data, num);

                /* Force emergency maintenance on the first failure. */
                if (ret < 0 && i == 0 && params != NULL) {
                        maintain_file(log_fd, /*file_size*/0, /*written=*/0,
                            /*force=*/true, info, params);
                        continue;
                }

                if (ret <= 0 || params == NULL)
                        return ret;
        }

        /* Scale the bytes written back to the exp(1) domain. */
        if (maintenance_frequency == 0)
                maintenance_frequency = derive_frequency(params);

        scaled = num * maintenance_frequency;
        if (scaled < writer_state.countdown) {
                writer_state.countdown -= scaled;
                return ret;
        }

        /* 0 means we have to initialise our state. */
        if (writer_state.countdown == 0) {
                update_countdown(&writer_state);

                /* Check if we still want to sample. */
                if (scaled < writer_state.countdown) {
                        writer_state.countdown -= scaled;
                        return ret;
                }
        }

        update_countdown(&writer_state);
        maintain_file(log_fd, ret, num, /*force=*/false, info, params);
        return ret;
}
#endif /* ! LOG_FILE_APPEND_IMPL */
