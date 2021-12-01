from pathlib import Path


def calc_score(deck) -> int:
    return sum(a * b for a, b in enumerate(reversed(deck), start=1))


def combat(d1: list, d2: list, recursive: bool) -> tuple[int, int]:
    d1, d2 = d1.copy(), d2.copy()
    history = set()
    while d1 and d2:
        pair = (tuple(d1), tuple(d2))
        if recursive and pair in history:
            return (1, calc_score(d1))
        history.add(pair)
        a, b = d1.pop(0), d2.pop(0)
        if recursive and a <= len(d1) and b <= len(d2):
            winner, _ = combat(d1[:a], d2[:b], True)
        else:
            winner = 1 if a > b else 2
        if winner == 1:
            d1 += [a, b]
        else:
            d2 += [b, a]
    return (1 if d1 else 2, calc_score(d1 or d2))


def main():
    text = (Path(__file__).parent / "../input/input_22.txt").read_text()
    decks = {}
    for block in text.split('\n\n'):
        header, *nums = block.splitlines()
        p = int(header[-2])
        decks[p] = [int(n) for n in nums]
    print("Part 1:", combat(decks[1], decks[2], False)[1])
    print("Part 2:", combat(decks[1], decks[2], True)[1])


if __name__ == "__main__":
    main()
