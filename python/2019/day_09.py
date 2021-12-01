from pathlib import Path

from intcode import CPU


def main():
    # Read in the amp controller software
    text = (Path(__file__).parent / "../../input/2019/input_09.txt").read_text()
    program = [int(x) for x in text.split(',')]
    for p in [1, 2]:
        cpu = CPU(program)
        cpu.input.append(p)
        cpu.run()
        print(f"Part {p}:", cpu.output.popleft())


if __name__ == "__main__":
    main()
