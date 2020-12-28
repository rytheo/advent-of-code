from pathlib import Path


def main():
    text = (Path(__file__).parent / "../input/input_25.txt").read_text()
    public_keys = [int(x) for x in text.splitlines()]
    base, loop_size = 1, 0
    while base != public_keys[0]:
        base = base * 7 % 20201227
        loop_size += 1
    enc_key = pow(public_keys[1], loop_size, 20201227)
    print("Part 1:", enc_key)


if __name__ == "__main__":
    main()
