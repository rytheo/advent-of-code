from pathlib import Path


def main():
    text = (Path(__file__).parent / "../input/input_08.txt").read_text().strip()
    width, height = 25, 6
    lsize = width * height
    layers = [[int(x) for x in text[i:i+lsize]] for i in range(0, len(text), lsize)]
    target = min(layers, key=lambda l: sum(c == 0 for c in l))
    print("Part 1:", sum(x == 1 for x in target) * sum(x == 2 for x in target))
    print("Part 2:")
    for row in range(height):
        for col in range(width):
            for layer in layers:
                if (c := layer[row * width + col]) != 2:
                    print('#' if c else ' ', end='')
                    break
        print()


if __name__ == "__main__":
    main()
