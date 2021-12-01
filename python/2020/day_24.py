import re
from pathlib import Path


deltas = {
    'e': (1, -1, 0),
    'se': (0, -1, 1),
    'sw': (-1, 0, 1),
    'w': (-1, 1, 0),
    'nw': (0, 1, -1),
    'ne': (1, 0, -1),
}


def tile_from_str(s: str) -> tuple[int, int, int]:
    x = y = z = 0
    for dir in re.findall(r"[ns]?[ew]", s):
        dx, dy, dz = deltas[dir]
        x, y, z = x + dx, y + dy, z + dz
    return x, y, z


def find_adj(tile: tuple[int, int, int]) -> set[tuple[int, int, int]]:
    x, y, z = tile
    return {(x + dx, y + dy, z + dz) for dx, dy, dz in deltas.values()}


def main():
    text = (Path(__file__).parent / "../../input/2020/input_24.txt").read_text()
    flipped = set()
    for line in text.splitlines():
        tile = tile_from_str(line)
        if tile in flipped:
            flipped.remove(tile)
        else:
            flipped.add(tile)
    print("Part 1:", len(flipped))
    for _ in range(100):
        prev = flipped.copy()
        to_check = prev.union(*(find_adj(t) for t in prev))
        for tile in to_check:
            adj = sum(t in prev for t in find_adj(tile))
            if (adj == 0 or adj > 2) and tile in prev:
                flipped.remove(tile)
            elif adj == 2 and tile not in prev:
                flipped.add(tile)
    print("Part 2:", len(flipped))


if __name__ == "__main__":
    main()
