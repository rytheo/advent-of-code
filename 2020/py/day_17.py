import itertools as it
from collections import Counter
from pathlib import Path


deltas = {n: {v for v in it.product((-1, 0, 1), repeat=n)} - {(0,) * n} for n in (3, 4)}


def find_adj(cell: tuple[int, ...]) -> set[tuple[int, ...]]:
    return {tuple(x + d for x, d in zip(cell, delta)) for delta in deltas[len(cell)]}


def simulate(text: str, n: int) -> int:
    active = set()
    for y, row in enumerate(text.splitlines()):
        for x, c in enumerate(row):
            if c == '#':
                active.add((x, y, *(0,)*(n-2)))
    for _ in range(6):
        # Track all cells next to active cells
        activity = Counter()
        for cell in active:
            for adj in find_adj(cell):
                activity[adj] += 1
        # Keep cells based on adjacency counts
        active = {c for c, a in activity.items() if a == 3 or a == 2 and c in active}
    return len(active)


def main():
    text = (Path(__file__).parent / "../input/input_17.txt").read_text().strip()
    print("Part 1:", simulate(text, 3))
    print("Part 2:", simulate(text, 4))


if __name__ == "__main__":
    main()
