import math
from pathlib import Path


def parse_nums_1(rows: list[str]) -> list[list[int]]:
    bins = [[] for _ in range(len(rows[0].split()))]
    for row in rows:
        for col, token in enumerate(row.split()):
            bins[col].append(int(token))
    return bins


def parse_nums_2(rows: list[str]) -> list[list[int]]:
    bins = [[] for _ in range(len(rows[0].split()))]
    bin_idx = 0
    for col_idx in range(len(rows[0])):
        cur_num = 0
        for row in rows:
            try:
                cur_num = cur_num * 10 + int(row[col_idx])
            except ValueError:
                continue
        if cur_num > 0:
            bins[bin_idx].append(cur_num)
        else:
            bin_idx += 1
    return bins


def accumulate(vals: list[int], op: str) -> int:
    return sum(vals) if op == "+" else math.prod(vals)


def main():
    text = (Path(__file__).parent / "../../input/2025/input_06.txt").read_text()
    *rows, ops_line = text.splitlines()
    ops = ops_line.split()
    for part, parser in enumerate((parse_nums_1, parse_nums_2), start=1):
        bins = parser(rows)
        total = sum(accumulate(vals, op) for vals, op in zip(bins, ops))
        print(f"Part {part}: {total}")


if __name__ == "__main__":
    main()
