import itertools as it
from pathlib import Path
from typing import Callable


def write_1(mem, addr, val, mask):
    for i, b in enumerate(reversed(mask)):
        if b == '0':
            val &= ~(1 << i)
        elif b == '1':
            val |= 1 << i
    mem[addr] = val


def write_2(mem, addr, val, mask):
    x_indices = []
    for i, b in enumerate(reversed(mask)):
        if b == 'X':
            x_indices.append(i)
        elif b == '1':
            addr |= 1 << i
    for combo in it.product((0, 1), repeat=len(x_indices)):
        for i, b in zip(x_indices, combo):
            if b == 1:
                addr |= 1 << i
            else:
                addr &= ~(1 << i)
        mem[addr] = val


def run(program: list, write_fn: Callable) -> int:
    mem = {}
    for line in program:
        op, val = line.split(' = ')
        if op == 'mask':
            mask = val
        else:
            addr = int(op[4:-1])
            write_fn(mem, addr, int(val), mask)
    return sum(mem.values())


def main():
    text = (Path(__file__).parent / "../../input/2020/input_14.txt").read_text()
    program = text.splitlines()
    print("Part 1:", run(program, write_1))
    print("Part 2:", run(program, write_2))


if __name__ == "__main__":
    main()
