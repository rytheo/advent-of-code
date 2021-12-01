import itertools as it
from copy import deepcopy
from pathlib import Path


def in_bounds(grid, y, x) -> bool:
    return 0 <= y < len(grid) and 0 <= x < len(grid[0])

def neighbors(grid, y, x) -> int:
    count = 0
    for y1, x1 in it.product((y-1, y, y+1), (x-1, x, x+1)):
        if in_bounds(grid, y1, x1) and (y1 != y or x1 != x) and grid[y1][x1] == '#':
            count += 1
    return count

def visible(grid, y, x) -> int:
    count = 0
    for dy, dx in it.product((-1, 0, 1), repeat=2):
        if dy == dx == 0:
            continue
        y1, x1 = y, x
        while True:
            y1, x1 = y1 + dy, x1 + dx
            if not in_bounds(grid, y1, x1) or grid[y1][x1] == 'L':
                break
            if grid[y1][x1] == '#':
                count += 1
                break
    return count


def simulate(grid, advanced) -> int:
    grid = deepcopy(grid)
    stable = False
    while not stable:
        stable = True
        g_prime = deepcopy(grid)
        for y, x in it.product(range(len(grid)), range(len(grid[0]))):
            n = visible(grid, y, x) if advanced else neighbors(grid, y, x)
            if grid[y][x] == 'L' and n == 0:
                g_prime[y][x] = '#'
                stable = False
            elif grid[y][x] == '#' and n >= 4 + advanced:
                g_prime[y][x] = 'L'
                stable = False
        grid = g_prime
    return sum(seat == '#' for row in grid for seat in row)

def main():
    text = (Path(__file__).parent / "../../input/2020/input_11.txt").read_text()
    grid = [list(line) for line in text.splitlines()]
    print("Part 1: ", simulate(grid, False))
    print("Part 2: ", simulate(grid, True))


if __name__ == "__main__":
    main()
