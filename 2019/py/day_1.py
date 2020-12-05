from pathlib import Path


def main():
    text = (Path(__file__).parent / "../input/input_1.txt").read_text()
    module_fuel = 0
    total_fuel = 0
    for line in text.splitlines():
        mass = int(line)
        fuel = mass // 3 - 2
        module_fuel += fuel
        while fuel > 0:
            total_fuel += fuel
            fuel = fuel // 3 - 2
    print("Part 1:", module_fuel)
    print("Part 2:", total_fuel)


if __name__ == "__main__":
    main()
