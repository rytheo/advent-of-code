import itertools as it
from pathlib import Path


deltas = {n: {v for v in it.product((-1, 0, 1), repeat=n)} - {(0,) * n} for n in (3, 4)}


def find_adj(cell: tuple) -> set[tuple]:
    return {tuple(x + d for x, d in zip(cell, delta)) for delta in deltas[len(cell)]}


def simulate(text, n) -> int:
    cells = set()
    for y, row in enumerate(text.splitlines()):
        for x, c in enumerate(row):
            if c == '#':
                cells.add((x, y, *(0,)*(n-2)))
    for _ in range(6):
        future = cells.copy()
        for cell in cells.union(*(find_adj(c) for c in cells)):
            neighbors = sum(tuple(x + d for x, d in zip(cell, delta)) in cells for delta in deltas[n])
            if cell in cells and not (neighbors == 2 or neighbors == 3):
                future.remove(cell)
            elif cell not in cells and neighbors == 3:
                future.add(cell)
        cells = future
    return len(cells)


def main():
    text = (Path(__file__).parent / "../input/input_17.txt").read_text().strip()
    print("Part 1:", simulate(text, 3))
    print("Part 2:", simulate(text, 4))


if __name__ == "__main__":
    main()
