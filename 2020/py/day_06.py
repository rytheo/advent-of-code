from pathlib import Path


def main():
    text = (Path(__file__).parent / "../input/input_06.txt").read_text()
    groups = text.split('\n\n')
    print("Part 1:", sum(len(set(g) - {'\n'}) for g in groups))
    print("Part 2:", sum(len(set.intersection(*(set(p) for p in g.splitlines()))) for g in groups))


if __name__ == "__main__":
    main()
