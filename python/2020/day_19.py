import re
from functools import cache
from pathlib import Path


def main():
    text = (Path(__file__).parent / "../../input/2020/input_19.txt").read_text()
    rules, messages = text.strip().split('\n\n')
    patterns = dict(line.split(': ') for line in rules.splitlines())

    @cache
    def regex(key) -> str:
        tokens = patterns[key].split()
        for i in range(len(tokens)):
            if tokens[i].startswith('"'):
                tokens[i] = tokens[i][1:-1]
            elif tokens[i].isdigit():
                tokens[i] = f"({regex(tokens[i])})"
        return ''.join(tokens)

    print("Part 1:", sum(bool(re.fullmatch(regex('0'), line)) for line in messages.splitlines()))
    regex.cache_clear()
    patterns['8'] = "42 +"
    patterns['11'] = " | ".join(f"42 {{{n}}} 31 {{{n}}}" for n in range(1, 5))
    print("Part 2:", sum(bool(re.fullmatch(regex('0'), line)) for line in messages.splitlines()))


if __name__ == "__main__":
    main()
