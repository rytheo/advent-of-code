import itertools as it
from pathlib import Path

from intcode import CPU


def main():
    # Read in the program
    text = (Path(__file__).parent / "../input/input_02.txt").read_text()
    program = [int(x) for x in text.split(',')]
    # Part 1
    cpu = CPU(program.copy())
    cpu.mem[1:3] = 12, 2
    cpu.run()
    print("Part 1:", cpu.mem[0])
    # Part 2: Find the inputs that will produce 19690720
    for noun, verb in it.product(range(100), repeat=2):
        cpu = CPU(program.copy())
        cpu.mem[1:3] = noun, verb
        cpu.run()
        if cpu.mem[0] == 19690720:
            print("Part 2:", 100 * noun + verb)
            return


if __name__ == "__main__":
    main()
