from pathlib import Path


def fix(program: list) -> int:
    swap = {'nop': 'jmp', 'jmp': 'nop'}
    for i, (op, _) in enumerate(program):
        if op in swap:
            op = program[i][0] = swap[op]
            if (v := run(program)) is not None:
                return v
            program[i][0] = swap[op]


def run(program: list, acc_on_loop=False) -> int:
    visited = [False] * len(program)
    i = acc = 0
    while i < len(program):
        if visited[i]:
            return acc if acc_on_loop else None
        visited[i] = True
        op, n = program[i]
        if op == 'acc':
            acc += n
        elif op == 'jmp':
            i += n - 1
        i += 1
    return acc


def main():
    text = (Path(__file__).parent / "../../input/2020/input_08.txt").read_text()
    program = []
    for line in text.strip().splitlines():
        op, n = line.split(' ')
        program.append([op, int(n)])
    print("Part 1:", run(program, acc_on_loop=True))
    print("Part 2:", fix(program))


if __name__ == "__main__":
    main()
