import itertools as it
from math import atan2, gcd
from pathlib import Path


def main():
    text = (Path(__file__).parent / "../input/input_10.txt").read_text()
    grid = [list(s) for s in text.strip().splitlines()]
    w, h = len(grid[0]), len(grid)
    vectors = {v for v in it.product(range(-h, h), range(-w, w)) if gcd(*v) == 1}
    vectors = sorted(vectors, key=lambda v: -atan2(v[1], v[0]))

    def scan(loc: tuple[int, int], vaporize: bool) -> int:
        total = 0
        for dy, dx in it.cycle(vectors) if vaporize else vectors:
            y, x = loc[0] + dy, loc[1] + dx
            while 0 <= y < len(grid) and 0 <= x < len(grid[0]):
                if grid[y][x] == '#':
                    total += 1
                    if vaporize:
                        grid[y][x] = '.'
                        if total == 200:
                            return 100 * x + y
                    break
                y, x = y + dy, x + dx
        return total

    station = (0, 0)
    best = 0
    for y, x in it.product(range(h), range(w)):
        if grid[y][x] == '#' and (n := scan((y, x), False)) > best:
            station = (y, x)
            best = n
    print("Part 1:", best)
    print("Part 2:", scan(station, True))


if __name__ == "__main__":
    main()
