import re
from pathlib import Path


def bag_contains(rules, colour, target) -> bool:
    return target in rules[colour] or any(bag_contains(rules, c, target) for c in rules[colour])


def bag_count(rules, colour) -> int:
    return 1 + sum(q * bag_count(rules, c) for c, q in rules[colour].items())


def main():
    text = (Path(__file__).parent / "../input/input_07.txt").read_text()
    rules = {}
    for line in text.splitlines():
        outer, inner = line.split(" bags contain ")
        rules[outer] = {m[2]: int(m[1]) for m in re.finditer(r"(\d+) (.+?) bag", inner)}
    print("Part 1:", sum(bag_contains(rules, c, "shiny gold") for c in rules))
    print("Part 2:", bag_count(rules, "shiny gold") - 1)


if __name__ == "__main__":
    main()
