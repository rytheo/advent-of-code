from pathlib import Path


def max_joltage(bank: str, batteries: int) -> int:
    digits = ""
    while batteries > 0:
        batteries -= 1
        idx = max(range(0, len(bank) - batteries), key=bank.__getitem__)
        digits += bank[idx]
        bank = bank[idx+1:]
    return int(digits)


def main():
    text = (Path(__file__).parent / "../../input/2025/input_03.txt").read_text()
    banks = text.splitlines()
    print("Part 1:", sum(max_joltage(bank, 2) for bank in banks))
    print("Part 2:", sum(max_joltage(bank, 12) for bank in banks))


if __name__ == "__main__":
    main()
