from functools import reduce

class MonoidFifo:
    def __init__(self, combiner, identity, trace=False):
        self.combiner = combiner
        self.identity = identity
        self.trace = trace
        self.store = dict()  # int -> value or suffix product
        self._input_values = dict() # int -> value, used only for check_rep and its callees

        # values in [pop_index:push_index)
        self.pop_idx = 0
        self.push_idx = 0
        # write cursor goes down toward pop_idx (write_cursor >= pop_idx),
        # and the suffix product is up to date *at* write_cursor inclusively.
        self.write_cursor = 0

        # staging list in [first_staging_idx:first_ingestion_idx)
        self.first_staging_idx = 0
        self.staging_product = identity # product for the staging list

        # ingestion list in [first_ingestion_idx:push_index)
        self.first_ingestion_idx = 0
        self.ingestion_product = identity # running product for the ingestion list
        self.check_rep()

    def check_rep(self):
        """Check internal invariants."""
        self._check_structure()
        self._check_products()
        self._check_progress()

    def _check_structure(self):
        """Look for grossly invalid state."""
        assert self.pop_idx <= self.first_ingestion_idx <= self.push_idx
        assert self.write_cursor <= self.first_ingestion_idx
        assert self.first_staging_idx <= self.first_ingestion_idx
        assert list(self.store) == list(range(self.pop_idx, self.push_idx))
        for idx in range(self.first_ingestion_idx, self.push_idx):  # The ingestion list should have the raw values
            assert self.store[idx] == self._input_values[idx]
        for idx in range(self.first_staging_idx, self.write_cursor):  # Same for unprocessed staging values
            assert self.store[idx] == self._input_values[idx]

    def _check_products(self):
        """Make sure our suffix products have the expected values."""
        def reference(indices):
            return reduce(self.combiner, (self._input_values[idx] for idx in indices), self.identity)
        assert reference(range(self.first_ingestion_idx, self.push_idx)) == self.ingestion_product
        assert reference(range(self.first_staging_idx, self.first_ingestion_idx)) == self.staging_product
        for idx in range(self.write_cursor, self.first_ingestion_idx):
            assert reference(range(idx, self.first_ingestion_idx)) == self.store[idx], \
                "at or greater than write cursor: must have updated product"
        for idx in range(self.pop_idx, min(self.write_cursor, self.first_staging_idx)):
            assert reference(range(idx, self.first_staging_idx)) == self.store[idx], \
                "old excretion, left of write cursor: must have old product"

    def _check_progress(self):
        """Make sure the suffix product doesn't fall behind."""
        assert self.push_idx - self.first_ingestion_idx <= self.first_ingestion_idx - self.pop_idx, \
            "ingestion list <= excretion list"
        assert self.first_staging_idx - self.pop_idx >= self.first_staging_idx - self.write_cursor, \
            "old ingestion list >= unupdated staging list"

    def push(self, value):
        self.check_rep()
        assert self.push_idx not in self.store
        self.store[self.push_idx] = value
        self._input_values[self.push_idx] = value # Only for check_rep
        self.push_idx += 1
        self.ingestion_product = self.combiner(self.ingestion_product, value)
        self.maintain()

    def peek(self):
        self.check_rep()
        if self.pop_idx == self.push_idx:
            return self.identity
        ret = self.store[self.pop_idx]
        if self.pop_idx < self.write_cursor:
            ret = self.combiner(ret, self.staging_product)
        ret = self.combiner(ret, self.ingestion_product)
        return ret

    def pop(self):
        ret = self.peek()
        del self.store[self.pop_idx]
        self.pop_idx += 1
        self.maintain()
        return ret

    def maintain(self):
        self._check_structure()
        if self.write_cursor > self.pop_idx:
            self._advance()
        if self.write_cursor <= self.pop_idx:
            self._promote()
        self.check_rep()

    def _advance(self):
        assert self.write_cursor > self.pop_idx
        self.write_cursor -= 1
        curr = self.store[self.write_cursor]
        if self.write_cursor < self.first_staging_idx:
            # outside the staging list, we update the precomputed suffix product
            update = self.combiner(curr, self.staging_product)
        else:
            # in the staging list, we compute a regular suffix product
            update = self.combiner(curr, self.store[self.write_cursor + 1])
        if self.trace:
            print(f"advance {curr} => {update}")
        self.store[self.write_cursor] = update

    def _promote(self):
        self.staging_product = self.ingestion_product
        self.ingestion_product = self.identity
        self.first_staging_idx = self.first_ingestion_idx

        if self.trace:
            print(f"promote {[self.store[idx] for idx in range(self.pop_idx, self.first_staging_idx)]} "
                  f" {[self.store[idx] for idx in range(self.first_staging_idx, self.push_idx)]} "
                  f"{self.staging_product}")

        if self.pop_idx == self.push_idx: # empty FIFO -> empty excretion list
            # If it weren't for `check_rep`, we could execute the `else`
            # block unconditionally: the only thing we can do with an empty
            # FIFO is `peek` (which already guards for empty FIFO), or
            # `push` (will will immediate promote and overwrite
            # `write_cursor`/`ingestion_product`).
            self.write_cursor = self.push_idx
            self.ingestion_product = self.identity
        else:
            self.write_cursor = self.push_idx - 1 # one free combine with identity
            self.first_ingestion_idx = self.push_idx


from collections import deque
SUT = None
TEST_CASE = None


def test_one(ops, trace=False):
    global SUT, TEST_CASE
    counter = 1
    # free monoid is useless as an agg, but obviously catches issues.
    sut = MonoidFifo(lambda x, y: x + y, [], trace)
    SUT = sut
    TEST_CASE = list(ops)
    ref = deque()

    assert sut.peek() == list(ref)

    for idx, op in enumerate(ops):
        assert sut.peek() == list(ref), (ops[:idx + 1], sut.peek(), list(ref))
        if op:
            push = [counter]
            sut.push(push)
            ref.append(counter)
            counter += 1
        else:
            ret = sut.pop()
            assert ret == list(ref), ops[:idx + 1]
            ref.popleft()
        assert sut.peek() == list(ref), (ops[:idx + 1], sut.peek(), list(ref))


def _enumerate_sawtooth_cases(max_pushes):
    if not max_pushes:
        yield []
        return
    for count in range(1, max_pushes[0] + 1):
        # Last one always pops as much as possible
        for pops in (range(1, count + 1) if len(max_pushes) > 1 else range(count, count + 1)):
            for remainder in _enumerate_sawtooth_cases(max_pushes[1:]):
                yield [count, -pops] + remainder

def enumerate_sawtooth_cases(*max_pushes):
    for counts in sorted(set(tuple(counts) for counts in _enumerate_sawtooth_cases(max_pushes)),
                         key=lambda counts: (sum(abs(count) for count in counts), counts)):
        acc = []
        remainder = 0
        for count in counts:
            remainder += count
            if count > 0:
                acc += [True] * count
            else:
                acc += [False] * -count
        acc += [False] * remainder
        yield acc


def enumerate_test_cases(push_budget=14, pop_budget=0):
    if push_budget == 0:
        yield [False] * pop_budget
        return
    if pop_budget > 0:
        for case in enumerate_test_cases(push_budget, pop_budget - 1):
            yield [False] + case
    for case in enumerate_test_cases(push_budget - 1, pop_budget + 1):
        yield [True] + case


if __name__ == "__main__":
    count = 0
    for test_case in enumerate_sawtooth_cases(100):
        test_one(test_case)
        count += 1
    print(f"Completed {count} in [1..100]")

    count = 0
    for test_case in enumerate_sawtooth_cases(25, 25):
        test_one(test_case)
        count += 1
    print(f"Completed {count} in [1..25], [1..25]")

    count = 0
    for test_case in enumerate_sawtooth_cases(10, 10, 10):
        test_one(test_case)
        count += 1
    print(f"Completed {count} in [1..10], [1..10], [1..10]")

    for pushes in range(17):  # up to 16 pushes (and 16 pops)
        count = 0
        for test_case in enumerate_test_cases(pushes):
            test_one(test_case)
            count += 1
        print(f"Completed {count} at pushes={pushes}")
