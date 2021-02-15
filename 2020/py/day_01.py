import itertools as it
from math import prod
from pathlib import Path


def find(iterable, r):
    for items in it.combinations(iterable, r):
        if sum(items) == 2020:
            return prod(items)


def main():
    text = (Path(__file__).parent / "../input/input_01.txt").read_text()
    entries = [int(line) for line in text.splitlines()]
    print("Part 1:", find(entries, 2))
    print("Part 2:", find(entries, 3))


if __name__ == "__main__":
    main()
