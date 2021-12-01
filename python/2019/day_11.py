from pathlib import Path

from intcode import CPU


deltas = [
    (0, -1),  # Up
    (1, 0),  # Right
    (0, 1),  # Down
    (-1, 0),  # Left
]


def paint(program: list[int], start_white: bool = False) -> dict[tuple[int, int], int]:
    robot = CPU(program)
    x = y = 0
    direction = 0  # Robot starts facing up
    panels = {}
    if start_white:
        panels[(0, 0)] = 1
    # Run until the robot halts
    while True:
        # Give the robot its next colour
        robot.input.append(panels.get((x, y), 0))
        if robot.run() == 99:
            break
        # Paint the current tile
        panels[(x, y)] = robot.output.popleft()
        # Turn left or right
        turn =  -1 if robot.output.popleft() == 0 else 1
        direction = (direction + turn) % 4
        # Move forward
        dx, dy = deltas[direction]
        x, y = x + dx, y + dy
    return panels


def main():
    text = (Path(__file__).parent / "../../input/2019/input_11.txt").read_text()
    program = [int(x) for x in text.strip().split(",")]
    # Print the number of painted panels
    print("Part 1:", len(paint(program)))
    print("Part 2:")
    panels = paint(program, start_white=True)
    x0, y0 = min(p[0] for p in panels), min(p[1] for p in panels)
    x1, y1 = max(p[0] for p in panels), max(p[1] for p in panels)
    for y in range(y0, y1 + 1):
        for x in range(x0, x1 + 1):
            print("#" if panels.get((x, y), 0) else " ", end="" if x < x1 else "\n")


if __name__ == "__main__":
    main()
