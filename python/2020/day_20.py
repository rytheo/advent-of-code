import itertools as it
import re
from math import prod
from pathlib import Path


class Tile:
    def __init__(self, raw: str):
        header, *data = raw.splitlines()
        self.id = int(header[5:-1])
        self.image = [list(row[1:-1]) for row in data[1:-1]]
        self.edges = [
            list(data[0]),
            [row[-1] for row in data],
            list(data[-1]),
            [row[0] for row in data],
        ]
        self.adj = [None] * 4

    def attach(self, tile: "Tile") -> bool:
        for (d1, e1), (d2, e2) in it.product(enumerate(self.edges), enumerate(tile.edges)):
            if e1 == e2 or e1 == list(reversed(e2)):
                for _ in range((d1 - d2 + 2) % 4):
                    tile.rotate()
                d2 = (d1 + 2) % 4
                e2 = tile.edges[d2]
                if e1 != e2:
                    tile.flip(d1 % 2 == 0)
                self.adj[d1] = tile
                tile.adj[d2] = self
                return True
        return False

    def rotate(self):
        self.image = [[self.image[-1-x][y] for x in range(len(self.image))] for y in range(len(self.image))]
        self.edges = [
            list(reversed(self.edges[3])),
            self.edges[0],
            list(reversed(self.edges[1])),
            self.edges[2],
        ]
        self.adj.insert(0, self.adj.pop())

    def flip(self, x: bool):
        if x:
            for row in self.image:
                row.reverse()
        else:
            self.image.reverse()
        self.edges[1+x].reverse()
        self.edges[(3+x) % 4].reverse()
        self.edges[0+x], self.edges[2+x] = self.edges[2+x], self.edges[0+x]
        self.adj[0+x], self.adj[2+x] = self.adj[2+x], self.adj[0+x]


def scan(image) -> int:
    image = [''.join(row) for row in image]
    monsters = 0
    for y in range(1, len(image)-1):
        for m in re.finditer(r"(?:#....#){3}##", image[y]):
            i = m.start()
            if re.match(r".{18}#.", image[y-1][i:]) and re.match(r"(?:.#.){6}..", image[y+1][i:]):
                monsters += 1
    return sum(row.count('#') for row in image) - 15 * monsters if monsters else None


def orient(image) -> int:
    for _ in range(4):
        for _ in range(2):
            image.reverse()
            if v := scan(image):
                return v
        # Rotate image a quarter clockwise
        image = [[image[-1-x][y] for x in range(len(image))] for y in range(len(image))]


def main():
    text = (Path(__file__).parent / "../../input/2020/input_20.txt").read_text()
    box = [Tile(raw) for raw in text.strip().split('\n\n')]
    # Start from any tile
    puzzle = [box.pop()]
    # Loop while there are unattached tiles
    while box:
        t2 = box.pop(0)
        for t1 in puzzle:
            if t1.attach(t2):
                puzzle.append(t2)
                break
        else:
            box.append(t2)
    # Finish connecting the tiles
    for (t1, t2) in it.combinations(puzzle, 2):
        for d1 in range(4):
            d2 = (d1 + 2) % 4
            if t1.edges[d1] == t2.edges[d2]:
                t1.adj[d1] = t2
                t2.adj[d2] = t1
    corners = [t for t in puzzle if sum(map(bool, t.adj)) == 2]
    print("Part 1:", prod(t.id for t in corners))
    # Assemble the image
    left = next(t for t in corners if not (t.adj[0] or t.adj[3]))
    image = []
    while left:
        rows = left.image
        right = left.adj[1]
        while right:
            for i, row in enumerate(right.image):
                rows[i].extend(row)
            right = right.adj[1]
        image.extend(rows)
        left = left.adj[2]
    print("Part 2:", orient(image))


if __name__ == "__main__":
    main()
