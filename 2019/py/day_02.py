from pathlib import Path


def run(program: list) -> int:
    tape = program.copy()
    i = 0
    while True:
        opcode, a, b, c = tape[i:i+4]
        if opcode == 99:
            break
        elif opcode == 1:
            tape[c] = tape[a] + tape[b]
        elif opcode == 2:
            tape[c] = tape[a] * tape[b]
        else:
            raise SyntaxError(f'Unknown opcode: {opcode}')
        i += 4  # Step to the next opcode
    return tape[0]


def main():
    # Read in the program
    text = (Path(__file__).parent / "../input/input_02.txt").read_text()
    program = [int(x) for x in text.split(',')]
    # Part 1
    program[1] = 12
    program[2] = 2
    print("Part 1:", run(program))
    # Part 2: Find the inputs that will produce 19690720
    for noun in range(100):
        for verb in range(100):
            program[1] = noun
            program[2] = verb
            if run(program) == 19690720:
                print("Part 2:", 100 * noun + verb)
                return


if __name__ == "__main__":
    main()
