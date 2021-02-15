from pathlib import Path


def get_op(value) -> tuple[int, list[int]]:
    opcode = value % 100
    modes = [(value // (10**i)) % 10 for i in range(2,5)]
    return opcode, modes


def run(program: list, inputs: list) -> list:
    tape = program.copy()
    inputs = iter(inputs)
    outputs = []
    i = 0
    while True:
        opcode, modes = get_op(tape[i])
        if opcode == 99:  # Halt execution
            return outputs
        elif opcode == 1:  # Add
            a, b, c = tape[i+1:i+4]
            a, b = [x if mode else tape[x] for x, mode in zip((a, b), modes)]
            tape[c] = a + b
            i += 4
        elif opcode == 2:  # Multiply
            a, b, c = tape[i+1:i+4]
            a, b = [x if mode else tape[x] for x, mode in zip((a, b), modes)]
            tape[c] = a * b
            i += 4
        elif opcode == 3:  # Write
            a = tape[i+1]
            tape[a] = next(inputs)
            i += 2
        elif opcode == 4:  # Output
            a = tape[i+1]
            outputs.append(a if modes[0] else tape[a])
            i += 2
        elif opcode == 5:  # Jump if true
            a, b = tape[i+1:i+3]
            a, b = [x if mode else tape[x] for x, mode in zip((a, b), modes)]
            i = b if a else i+3
        elif opcode == 6:  # Jump if false
            a, b = tape[i+1:i+3]
            a, b = [x if mode else tape[x] for x, mode in zip((a, b), modes)]
            i = b if not a else i+3
        elif opcode == 7:  # Less than
            a, b, c = tape[i+1:i+4]
            a, b = [x if mode else tape[x] for x, mode in zip((a, b), modes)]
            tape[c] = int(a < b)
            i += 4
        elif opcode == 8:  # Equals
            a, b, c = tape[i+1:i+4]
            a, b = [x if mode else tape[x] for x, mode in zip((a, b), modes)]
            tape[c] = int(a == b)
            i += 4
        else:
            raise SyntaxError(f'Unknown opcode: {opcode}')


def main():
    # Read in the program
    text = (Path(__file__).parent / "../input/input_05.txt").read_text()
    program = [int(x) for x in text.split(',')]
    print("Part 1:", run(program, [1])[-1])
    print("Part 2:", run(program, [5])[-1])


if __name__ == "__main__":
    main()
