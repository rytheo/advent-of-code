import re
from pathlib import Path


def valid_1(a, b, char, password) -> bool:
    return a <= password.count(char) <= b


def valid_2(a, b, char, password) -> bool:
    return (password[a-1] == char) != (password[b-1] == char)


def main():
    text = (Path(__file__).parent / "../input/input_02.txt").read_text()
    total_1 = total_2 = 0
    for line in text.splitlines():
        a, b, char, password = re.split(r"[ :-]+", line)
        a, b = int(a), int(b)
        total_1 += valid_1(a, b, char, password)
        total_2 += valid_2(a, b, char, password)
    print("Part 1:", total_1)
    print("Part 2:", total_2)


if __name__ == "__main__":
    main()
