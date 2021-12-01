from pathlib import Path


def main():
    text = (Path(__file__).parent / "../../input/2020/input_10.txt").read_text()
    jolts = [0] + sorted(int(x) for x in text.splitlines())
    device = jolts[-1] + 3
    # Part 1
    diff_1 = sum(a - jolts[i-1] == 1 for i, a in enumerate(jolts[1:], start=1))
    diff_3 = sum(a - jolts[i-1] == 3 for i, a in enumerate(jolts[1:], start=1)) + 1
    print("Part 1:", diff_1 * diff_3)
    # Part 2
    ways = [0 for _ in jolts]
    for i, a in reversed(list(enumerate(jolts))):
        ways[i] = device - a <= 3
        ways[i] += sum(ways[j] for j, b in enumerate(jolts[i+1:i+4], start=i+1) if b - a <= 3)
    print("Part 2:", ways[0])


if __name__ == "__main__":
    main()
