import itertools as it
from collections.abc import Callable
from pathlib import Path


def parse_range(s: str) -> range:
    first, last = s.split("-")
    return range(int(first), int(last) + 1)


def is_doubled(n: int) -> bool:
    s = str(n)
    mid = len(s) // 2
    return s[:mid] == s[mid:]


def is_repeated(n: int) -> bool:
    s = str(n)
    num_digits = len(s)
    for repeat_len in range(1, num_digits):
        if num_digits % repeat_len != 0:
            continue
        if all(a == b for a, b in it.pairwise(it.batched(s, repeat_len))):
            return True
    return False


def sum_matching(ranges: list[range], pred: Callable[[int], bool]) -> int:
    return sum(filter(pred, it.chain.from_iterable(ranges)))


def main():
    text = (Path(__file__).parent / "../../input/2025/input_02.txt").read_text()
    ranges = [parse_range(s) for s in text.split(",")]
    print("Part 1:", sum_matching(ranges, is_doubled))
    print("Part 2:", sum_matching(ranges, is_repeated))


if __name__ == "__main__":
    main()
