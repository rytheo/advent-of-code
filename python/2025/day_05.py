from pathlib import Path


def parse_range(s: str) -> range:
    first, last = s.split("-")
    return range(int(first), int(last) + 1)


def dedupe_ranges(ranges: list[range]) -> list[range]:
    ranges.sort(key=lambda r: r.start)
    output = []
    cur = ranges[0]
    for r in ranges[1:]:
        if r.start < cur.stop:
            cur = range(cur.start, max(cur.stop, r.stop))
        else:
            output.append(cur)
            cur = r
    output.append(cur)
    return output


def is_fresh(ranges: list[range], ingredient: int) -> bool:
    return any(ingredient in r for r in ranges)


def main():
    text = (Path(__file__).parent / "../../input/2025/input_05.txt").read_text()
    fresh_block, available_block = text.split("\n\n")
    ranges = dedupe_ranges([parse_range(s) for s in fresh_block.splitlines()])
    available = [int(s) for s in available_block.splitlines()]
    print("Part 1:", sum(1 for x in available if is_fresh(ranges, x)))
    print("Part 2:", sum(len(r) for r in ranges))


if __name__ == "__main__":
    main()
