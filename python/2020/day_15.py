from pathlib import Path


def play(text: str, target: int) -> int:
    mem = [None] * target
    for turn, n in enumerate(map(int, text.strip().split(',')), start=1):
        mem[n] = turn
    turn, n = len(text.split(',')) + 1, 0
    for turn in range(turn, target):
        diff = turn - mem[n] if mem[n] else 0
        mem[n] = turn
        n = diff
    return n


def main():
    text = (Path(__file__).parent / "../../input/2020/input_15.txt").read_text()
    print("Part 1:", play(text, 2020))
    print("Part 2:", play(text, 30_000_000))


if __name__ == "__main__":
    main()
