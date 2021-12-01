from pathlib import Path


class Cup:
    def __init__(self, label: int):
        self.label = label
        self.cw = None
        self.dest = None


def do_moves(seq, n, big) -> Cup:
    # Construct cups based on starting sequence
    cmap = {x: Cup(x) for x in seq}
    ordered = list(cmap.values())
    if big:
        ordered.extend(Cup(x) for x in range(10, 1_000_001))
    for i, c in enumerate(ordered):
        c.cw = ordered[i+1 if i+1 < len(ordered) else 0]
        c.dest = cmap.get(c.label - 1, ordered[-1] if big else cmap[9]) if c.label < 11 else ordered[i-1]
    # Perform the moves
    curr = ordered[0]
    for _ in range(n):
        pickup = curr.cw
        curr.cw = pickup.cw.cw.cw
        dest = curr.dest
        while dest is pickup or dest is pickup.cw or dest is pickup.cw.cw:
            dest = dest.dest
        pickup.cw.cw.cw = dest.cw
        dest.cw = pickup
        curr = curr.cw
    return cmap[1]


def main():
    text = (Path(__file__).parent / "../../input/2020/input_23.txt").read_text()
    seq = [int(x) for x in text.strip()]
    cup = do_moves(seq, 100, False)
    print("Part 1: ", end='')
    for _ in range(8):
        cup = cup.cw
        print(cup.label, end='')
    cup = do_moves(seq, 10_000_000, True)
    print("\nPart 2:", cup.cw.label * cup.cw.cw.label)


if __name__ == "__main__":
    main()
