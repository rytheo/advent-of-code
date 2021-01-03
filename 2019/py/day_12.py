import itertools as it
import re
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class Moon:
    pos: list[int]
    vel: list[int] = field(default_factory=lambda: [0, 0, 0])

    @property
    def energy(self):
        return sum(abs(x) for x in self.pos) * sum(abs(x) for x in self.vel)


def main():
    text = (Path(__file__).parent / "../input/input_12.txt").read_text()
    moons = [Moon([int(s) for s in re.findall(r"-?\d+", l)]) for l in text.strip().splitlines()]
    for _ in range(1000):
        # Apply gravity
        for a, b in it.combinations(moons, 2):
            for i in range(3):
                if a.pos[i] < b.pos[i]:
                    a.vel[i] += 1
                    b.vel[i] -= 1
                elif a.pos[i] > b.pos[i]:
                    a.vel[i] -= 1
                    b.vel[i] += 1
        # Apply velocity
        for m in moons:
            for i in range(3):
                m.pos[i] += m.vel[i]
    print("Part 1:", sum(m.energy for m in moons))



if __name__ == "__main__":
    main()
