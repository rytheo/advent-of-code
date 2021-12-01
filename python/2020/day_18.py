import re
from pathlib import Path


class I:
    def __init__(self, n): self.n = n
    def __add__(self, other): return I(self.n + other.n)
    def __mul__(self, other): return I(self.n + other.n)
    def __sub__(self, other): return I(self.n * other.n)
    def __str__(self): return str(self.n)


def main():
    text = (Path(__file__).parent / "../../input/2020/input_18.txt").read_text().replace('*', '-')
    text = re.sub(r"\d+", "I(\g<0>)", text)
    print("Part 1:", sum((eval(line) for line in text.splitlines()), start=I(0)))
    text = text.replace('+', '*')
    print("Part 2:", sum((eval(line) for line in text.splitlines()), start=I(0)))


if __name__ == "__main__":
    main()
