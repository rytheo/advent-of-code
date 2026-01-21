import functools
from pathlib import Path


def count_splits(grid: list[list[str]]) -> int:
    count = 0
    for r, row in enumerate(grid[1:], start=1):
        for c, char in enumerate(row):
            if grid[r-1][c] in ("S", "|"):
                if char == ".":
                    grid[r][c] = "|"
                elif char == "^":
                    grid[r][c-1] = "|"
                    grid[r][c+1] = "|"
                    count += 1
    return count


def count_timelines(grid: list[list[str]]) -> int:
    @functools.cache
    def count_inner(y: int, x: int) -> int:
        for y in range(y, len(grid)):
            if grid[y][x] == "^":
                return count_inner(y, x - 1) + count_inner(y, x + 1)
        return 1
    return count_inner(0, grid[0].index("S"))


def main():
    text = (Path(__file__).parent / "../../input/2025/input_07.txt").read_text()
    grid = [list(row) for row in text.splitlines()]
    print("Part 1:", count_splits(grid))
    print("Part 2:", count_timelines(grid))


if __name__ == "__main__":
    main()
