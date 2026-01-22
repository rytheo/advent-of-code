import itertools
import math
from pathlib import Path


type Point = tuple[int, int, int]


def parse_point(s: str) -> Point:
    x, y, z = s.split(",")
    return int(x), int(y), int(z)


def sq_dist(a: Point, b: Point) -> int:
    return sum((ai - bi) ** 2 for ai, bi in zip(a, b))


def merge(a: Point, b: Point, circuit_map: dict[Point, list[Point]]):
    big = circuit_map[a]
    small = circuit_map[b]
    if big is small:
        return
    if len(small) > len(big):
        big, small = small, big
    big.extend(small)
    for point in small:
        circuit_map[point] = big
    small.clear()


def main():
    text = (Path(__file__).parent / "../../input/2025/input_08.txt").read_text()
    boxes = [parse_point(line) for line in text.splitlines()]
    circuits = [[box] for box in boxes]
    circuit_map = {box: circuits[i] for i, box in enumerate(boxes)}
    box_pairs = sorted(
        ((a, b) for a, b in itertools.combinations(boxes, 2)),
        key=lambda t: sq_dist(*t)
    )
    for a, b in box_pairs[:1000]:
        merge(a, b, circuit_map)
    circuits.sort(key=len, reverse=True)
    print("Part 1:", math.prod(len(c) for c in circuits[:3]))
    for a, b in box_pairs[1000:]:
        merge(a, b, circuit_map)
        if sum(1 for c in circuits if len(c) > 0) < 2:
            print("Part 2:", a[0] * b[0])
            break


if __name__ == "__main__":
    main()
