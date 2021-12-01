from math import prod
from pathlib import Path


def tree_count(grid, dx, dy):
    x = y = count = 0
    while y < len(grid):
        if grid[y][x] == '#':
            count += 1
        y, x = y + dy, (x + dx) % len(grid[0])
    return count


def main():
    text = (Path(__file__).parent / "../../input/2020/input_03.txt").read_text()
    grid = text.splitlines()
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print("Part 1:", tree_count(grid, 3, 1))
    print("Part 2:", prod(tree_count(grid, *s) for s in slopes))


if __name__ == "__main__":
    main()
