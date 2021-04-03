from pathlib import Path

from intcode import CPU


def main():
    # Read in the program
    text = (Path(__file__).parent / "../input/input_05.txt").read_text()
    program = [int(x) for x in text.split(',')]
    # Run with each system ID
    for i, sys_id in enumerate([1, 5]):
        cpu = CPU(program.copy())
        cpu.input.append(sys_id)
        cpu.run()
        print(f"Part {i + 1}: {cpu.output[-1]}")


if __name__ == "__main__":
    main()
