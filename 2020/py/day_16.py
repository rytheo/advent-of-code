import re
from math import prod
from pathlib import Path


def error_rate(ranges: list, ticket: list) -> int:
    return sum(v for v in ticket if not any(v in r for r in ranges))


def find_pair(tracker: dict) -> tuple[str, int]:
    for field, positions in tracker.items():
        if len(positions) == 1:
            return field, positions.pop()


def main():
    text = (Path(__file__).parent / "../input/input_16.txt").read_text()
    rules_text, your_text, nearby_text = re.split("\n\n.+:\n", text)
    # Parse rules
    rules = {}
    for line in rules_text.splitlines():
        m = re.match(r"([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)", line)
        rules[m[1]] = [
            range(int(m[2]), int(m[3]) + 1),
            range(int(m[4]), int(m[5]) + 1),
        ]
    # Determine which tickets are definitely invalid
    all_ranges = [r for l in rules.values() for r in l]
    your_ticket = [int(v) for v in your_text.split(',')]
    nearby_tickets = [[int(v) for v in line.split(',')] for line in nearby_text.splitlines()]
    print("Part 1:", sum(error_rate(all_ranges, t) for t in nearby_tickets))
    # Find possible positions for each field
    nearby_tickets = [t for t in nearby_tickets if error_rate(all_ranges, t) == 0]
    tracker = {field: set() for field in rules}
    for field, ranges in rules.items():
        for pos in range(len(your_ticket)):
            if all(any(t[pos] in r for r in ranges) for t in nearby_tickets):
                tracker[field].add(pos)
    # Deduce the position of each field
    pos_names = [''] * len(your_ticket)
    while pair := find_pair(tracker):
        field, pos = pair
        pos_names[pos] = field
        del tracker[field]
        for s in tracker.values():
            s.discard(pos)
    print("Part 2:", prod(val for val, field in zip(your_ticket, pos_names) if field.startswith('departure')))


if __name__ == "__main__":
    main()
