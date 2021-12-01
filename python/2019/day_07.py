import itertools as it
from pathlib import Path

from intcode import CPU


def try_combo(program: list[int], phases: tuple[int, ...], loop: bool) -> int:
    # Setup amps
    amps = [CPU(program)]
    # Each amp's output is the next amp's input
    for i in range(1, 5):
        amps.append(CPU(program, input=amps[i-1].output))
        amps[i].input.append(phases[i])
    if loop:
        amps[0].input = amps[-1].output
    amps[0].input.extend([phases[0], 0])
    # Run amps in sequence
    for i, amp in it.cycle(enumerate(amps)):
        op = amp.run()
        if (not loop or op == 99) and i == 4:
            break
    return amps[-1].output[-1]


def main():
    # Read in the amp controller software
    text = (Path(__file__).parent / "../../input/2019/input_07.txt").read_text()
    program = [int(x) for x in text.split(',')]
    # Part 1
    max_signal = max(try_combo(program, combo, False) for combo in it.permutations(range(5)))
    print("Part 1:", max_signal)
    # Part 2
    max_signal = max(try_combo(program, combo, True) for combo in it.permutations(range(5, 10)))
    print("Part 2:", max_signal)


if __name__ == "__main__":
    main()
