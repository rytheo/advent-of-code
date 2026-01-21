import itertools as it
from collections.abc import Iterator
from pathlib import Path


type Grid = list[list[str]]
type Point = tuple[int, int]


def neighbors(grid: Grid, point: Point) -> Iterator[Point]:
    height, width = len(grid), len(grid[0])
    y, x = point
    for dy, dx in it.product((-1, 0, 1), repeat=2):
        y1, x1 = y + dy, x + dx
        if (y1, x1) != (y, x) and 0 <= y1 < height and 0 <= x1 < width:
            yield y1, x1


def accessible_rolls(grid: Grid) -> Iterator[Point]:
    for y, row in enumerate(grid):
        for x, tile in enumerate(row):
            if tile != "@":
                continue
            num_nearby = sum(1 for y1, x1 in neighbors(grid, (y, x)) if grid[y1][x1] == "@")
            if num_nearby < 4:
                yield y, x


def remove_rolls(grid: Grid) -> int:
    total = 0
    while True:
        prev_total = total
        for y, x in accessible_rolls(grid):
            grid[y][x] = "."
            total += 1
        if total == prev_total:
            return total


def main():
    text = (Path(__file__).parent / "../../input/2025/input_04.txt").read_text()
    grid = [list(line) for line in text.splitlines()]
    print("Part 1:", sum(1 for _ in accessible_rolls(grid)))
    print("Part 2:", remove_rolls(grid))


if __name__ == "__main__":
    main()
