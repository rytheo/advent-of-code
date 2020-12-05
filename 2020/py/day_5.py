from pathlib import Path


def seat_id(seat):
    return sum(2**i for i, c in enumerate(reversed(seat)) if c in 'BR')


def main():
    text = (Path(__file__).parent / "../input/input_5.txt").read_text()
    seat_ids = sorted(seat_id(line) for line in text.splitlines())
    print("Part 1:", seat_ids[-1])
    for i, sid in enumerate(seat_ids[1:]):
        if sid - seat_ids[i] > 1:
            print("Part 2:", sid - 1)
            break


if __name__ == "__main__":
    main()
