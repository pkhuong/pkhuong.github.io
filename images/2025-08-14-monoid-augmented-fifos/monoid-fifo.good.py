from collections import deque

class MonoidFifo:
    def __init__(self, combiner, zero, trace=False):
        self.combiner = combiner
        self.zero = zero
        self.trace = trace
        self.store = dict()  # int -> value or prefix sum

        # values in [pop_index:push_index)
        self.pop_idx = 0
        self.push_idx = 0
        self.write_cursor = 0

        # staging list in [staging_idx:ingestion_idx)
        self.staging_idx = 0
        self.staging_product = zero

        # ingestion list in [ingestion_idx:push_index)
        self.ingestion_idx = 0
        self.ingestion_product = zero

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

    def promote(self):
        # pop_idx ... staging_idx ... ingestion_idx ... push_idx
        #                              write_cursor
        if self.ingestion_idx == self.push_idx:
            assert self.ingestion_product == self.zero
            return
        self.staging_product = self.ingestion_product
        self.ingestion_product = self.zero
        self.staging_idx = self.ingestion_idx

        if self.trace:
            print(f"promote {[self.store[idx] for idx in range(self.pop_idx, self.staging_idx)]} "
                  f" {[self.store[idx] for idx in range(self.staging_idx, self.push_idx)]} "
                  f"{self.staging_product}")

        self.write_cursor = self.push_idx - 1 # one free combine with zero
        self.ingestion_idx = self.push_idx

    def advance(self):
        self.write_cursor -= 1
        curr = self.store[self.write_cursor]
        if self.write_cursor < self.staging_idx:
            update = self.combiner(curr, self.staging_product)
        else:
            update = self.combiner(curr, self.store[self.write_cursor + 1])
        if self.trace:
            print(f"advance {curr} => {update}")
        self.store[self.write_cursor] = update

SUT = None

def test_one(ops):
    global SUT
    counter = 1
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
