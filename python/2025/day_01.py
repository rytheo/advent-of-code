from pathlib import Path

def parse_rot(s: str) -> int:
    amount = int(s[1:])
    return -1 * amount if s[0] == "L" else amount


def part_1(rotations: list[int]) -> int:
    dial = 50
    count = 0
    for rot in rotations:
        dial = (dial + rot) % 100
        if dial == 0:
            count += 1
    return count


def part_2(rotations: list[int]) -> int:
    dial = 50
    count = 0
    for rot in rotations:
        while rot < 0:
            rot += 1
            dial = (dial + 1) % 100
            if dial == 0:
                count += 1
        while rot > 0:
            rot -= 1
            dial = (dial - 1) % 100
            if dial == 0:
                count += 1
    return count


def main():
    text = (Path(__file__).parent / "../../input/2025/input_01.txt").read_text()
    rotations = [parse_rot(line) for line in text.splitlines()]
    print("Part 1:", part_1(rotations))
    print("Part 2:", part_2(rotations))


if __name__ == "__main__":
    main()
