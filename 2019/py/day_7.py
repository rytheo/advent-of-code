import itertools as it
from pathlib import Path
from typing import Generator


def get_op(value) -> tuple[int, list[int]]:
    opcode = value % 100
    modes = [(value // (10**i)) % 10 for i in range(2,5)]
    return opcode, modes


def run(program: list) -> Generator[int, int, None]:
    tape = program.copy()
    i = 0
    while True:
        opcode, modes = get_op(tape[i])
        if opcode == 99:  # Halt
            return
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
        elif opcode == 3:  # Input
            a = tape[i+1]
            tape[a] = yield
            i += 2
        elif opcode == 4:  # Output
            a = tape[i+1]
            yield a if modes[0] else tape[a]
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


def try_combo_linear(program: list, phases: tuple) -> int:
    signal = 0
    for phase in phases:
        amp = run(program)  # Construct generator
        next(amp)  # Start generator
        amp.send(phase)  # Provide phase
        signal = amp.send(signal)
    return signal


def try_combo_loop(program: list, phases: tuple) -> int:
    signals = [0] * len(phases)
    amps = []
    for phase in phases:
        amps.append(amp := run(program))
        next(amp)  # Start generator
        amp.send(phase)  # Provide phase
    i = 0
    loop = True
    while loop or i > 0:
        try:
            sig = amps[i].send(signals[i-1])
            signals[i] = sig
            next(amps[i])
        except StopIteration:
            loop = False
        finally:
            i = (i + 1) % len(signals)
    return signals[-1]


def main():
    # Read in the amp controller software
    text = (Path(__file__).parent / "../input/input_7.txt").read_text()
    program = [int(x) for x in text.split(',')]
    # Part 1
    max_signal = max(try_combo_linear(program, combo) for combo in it.permutations(range(5)))
    print("Part 1:", max_signal)
    max_signal = max(try_combo_loop(program, combo) for combo in it.permutations(range(5, 10)))
    print("Part 2:", max_signal)


if __name__ == "__main__":
    main()
