from pathlib import Path


def repeats(password: str) -> set:
    lengths = set()
    length = 1
    for i, c in enumerate(password):
        if i == 0:
            continue
        if password[i-1] > c:
            return {1}
        if password[i-1] == c:
            length += 1
        else:
            lengths.add(length)
            length = 1
    lengths.add(length)
    return lengths


def main():
    text = (Path(__file__).parent / "../input/input_04.txt").read_text()
    start, end = map(int, text.split('-'))
    sets = [repeats(str(p)) for p in range(start, end + 1)]
    print(f"Part 1: {sum(s != {1} for s in sets)}")
    print(f"Part 2: {sum(2 in s for s in sets)}")


if __name__ == "__main__":
    main()
