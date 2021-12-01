import itertools as it
import math
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable


@dataclass
class Moon:
    pos: list[int]
    vel: list[int] = field(default_factory=lambda: [0, 0, 0])

    @property
    def energy(self):
        return sum(abs(x) for x in self.pos) * sum(abs(x) for x in self.vel)


def step(moons: list[Moon], dims: Iterable[int]) -> None:
    # Apply gravity
    for a, b in it.combinations(moons, 2):
        for i in dims:
            if a.pos[i] < b.pos[i]:
                a.vel[i] += 1
                b.vel[i] -= 1
            elif a.pos[i] > b.pos[i]:
                a.vel[i] -= 1
                b.vel[i] += 1
    # Apply velocity
    for m in moons:
        for i in dims:
            m.pos[i] += m.vel[i]


def parse_moons(text: str) -> list[Moon]:
    return [Moon([int(s) for s in re.findall(r"-?\d+", l)]) for l in text.splitlines()]


def find_period(moons: list[Moon], dim: int) -> int:
    initial = [(m.pos[dim], m.vel[dim]) for m in moons]
    t = 0
    while True:
        t += 1
        step(moons, [dim])
        if [(m.pos[dim], m.vel[dim]) for m in moons] == initial:
            return t


def main():
    text = (Path(__file__).parent / "../../input/2019/input_12.txt").read_text()
    moons = parse_moons(text)
    # Part 1
    for _ in range(1000):
        step(moons, range(3))
    print("Part 1:", sum(m.energy for m in moons))
    tx, ty, tz = [find_period(parse_moons(text), dim) for dim in range(3)]
    print("Part 2:", math.lcm(tx, ty, tz))


if __name__ == "__main__":
    main()
