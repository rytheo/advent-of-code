import itertools as it
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


@dataclass
class Line:
    x0: int
    y0: int
    x1: int
    y1: int
    length: int
    prev: 'Line' = None

    def __post_init__(self):
        if self.prev:
            self.length += self.prev.length


@dataclass
class Intersect:
    x: int
    y: int
    v: Line
    h: Line
    dist: int = field(init=False)
    cost: int = field(init=False)

    def __post_init__(self):
        self.dist = abs(self.x) + abs(self.y)
        self.cost = (abs(self.y - self.v.y0) + self.v.prev.length
                   + abs(self.x - self.h.x0) + self.h.prev.length)

    @staticmethod
    def try_from_lines(v: Line, h: Line) -> Optional['Intersect']:
        x0, x1 = sorted([h.x0, h.x1])
        y0, y1 = sorted([v.y0, v.y1])
        if x0 < v.x0 < x1 and y0 < h.y0 < y1:
            return Intersect(v.x0, h.y0, v, h)


@dataclass
class Wire:
    verticals: list[Line] = field(default_factory=list)
    horizontals: list[Line] = field(default_factory=list)


def main():
    text = (Path(__file__).parent / "../../input/2019/input_03.txt").read_text()
    wires = []
    # Loop over each wire path
    for path in text.splitlines():
        wires.append(wire := Wire())
        prev = None
        x1, y1 = 0, 0
        # Evaluate each move
        for move in path.strip().split(','):
            x0, y0 = x1, y1
            way, dist = move[0], int(move[1:])
            if way == 'U':
                y1 -= dist
            elif way == 'R':
                x1 += dist
            elif way == 'D':
                y1 += dist
            elif way == 'L':
                x1 -= dist
            line = Line(x0, y0, x1, y1, dist, prev)
            prev = line
            if way in {'U', 'D'}:
                wire.verticals.append(line)
            else:
                wire.horizontals.append(line)
    # Compute all intersections
    intersections = []
    for a, b in [wires, reversed(wires)]:
        for v, h in it.product(a.verticals, b.horizontals):
            if intersection := Intersect.try_from_lines(v, h):
                intersections.append(intersection)
    # Part 1
    nearest = min(intersections, key=lambda i: i.dist)
    print("Part 1:", nearest.dist)
    # Part 2
    best = min(intersections, key=lambda i: i.cost)
    print("Part 2:", best.cost)


if __name__ == "__main__":
    main()
