class MonoidFifo:
    def __init__(self, combiner, zero, trace=False):
        self.combiner = combiner
        self.zero = zero
        self.trace = trace
        self.store = dict()  # int -> value or suffix product

        # values in [pop_index:push_index)
        self.pop_idx = 0
        self.push_idx = 0
        # write cursor goes down toward pop_idx (write_cursor >= pop_idx),
        # and the suffix product is up to date *at* write_cursor inclusively.
        self.write_cursor = 0

        # staging list in [first_staging_idx:first_ingestion_idx)
        self.first_staging_idx = 0
        self.staging_product = zero # product for the staging list

        # ingestion list in [first_ingestion_idx:push_index)
        self.first_ingestion_idx = 0
        self.ingestion_product = zero # running product for the ingestion list

    def push(self, value):
        assert self.push_idx not in self.store
        self.store[self.push_idx] = value
        self.push_idx += 1
        self.ingestion_product = self.combiner(self.ingestion_product, value)
        self.maintain()

    def peek(self):
        if self.pop_idx == self.push_idx:
            return self.zero
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
        if self.write_cursor > self.pop_idx:
            self.advance()
        if self.write_cursor <= self.pop_idx:
            self.promote()

    def advance(self):
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

    def promote(self):
        # pop_idx ... first_staging_idx ... first_ingestion_idx ... push_idx
        #                                  write_cursor
        if self.first_ingestion_idx == self.push_idx:  # optional, but might as well
            assert self.ingestion_product == self.zero
            return
        self.staging_product = self.ingestion_product
        self.ingestion_product = self.zero
        self.first_staging_idx = self.first_ingestion_idx

        if self.trace:
            print(f"promote {[self.store[idx] for idx in range(self.pop_idx, self.first_staging_idx)]} "
                  f" {[self.store[idx] for idx in range(self.first_staging_idx, self.push_idx)]} "
                  f"{self.staging_product}")

        self.write_cursor = self.push_idx - 1 # one free combine with zero
        self.first_ingestion_idx = self.push_idx


from collections import deque
SUT = None


def test_one(ops):
    global SUT
    counter = 1
    # free monoid is useless as an agg, but obviously catches issues.
    sut = MonoidFifo(lambda x, y: x + y, [])
    SUT = sut
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


def enumerate_test_cases(depth = 20, pop_budget = 0):
    if depth <= 0:
        yield []
        return
    if pop_budget > 0:
        for test_case in enumerate_test_cases(depth - 1, pop_budget - 1):
            yield [False] + test_case
    for test_case in enumerate_test_cases(depth - 1, pop_budget + 1):
        yield [True] + test_case


if __name__ == "__main__":
    for depth in range(25, 26):  # start at 0 to minimise failures
        for test_case in enumerate_test_cases(depth):
            test_one(test_case)
