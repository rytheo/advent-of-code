from math import lcm
from pathlib import Path


def main():
    text = (Path(__file__).parent / "../../input/2020/input_13.txt").read_text()
    # Part 1
    start, timeline = text.splitlines()
    start = int(start)
    bus_data = {int(b): i for i, b in enumerate(timeline.split(',')) if b != 'x'}
    time_map = {b: start - start % b + b for b in bus_data}
    first = min(time_map, key=time_map.get)
    print("Part 1:", first * (time_map[first] - start))
    # Part 2
    time = 0
    period = 1
    for bus, offset in bus_data.items():
        while (time + offset) % bus != 0:
            time += period
        period = lcm(period, bus)
    print("Part 2:", time)


if __name__ == "__main__":
    main()
