from pathlib import Path


dmap = {
    'E': lambda x, y, v: (x + v, y),
    'S': lambda x, y, v: (x, y + v),
    'W': lambda x, y, v: (x - v, y),
    'N': lambda x, y, v: (x, y - v),
}


def run_1(steps) -> int:
    face = 'E'
    x = y = 0
    for action, val in steps:
        if action in dmap:
            x, y = dmap[action](x, y, val)
        elif action == 'F':
            x, y = dmap[face](x, y, val)
        elif action in 'LR':
            turns = (action == 'R' or -1) * val // 90
            face = 'ESWN'[('ESWN'.index(face) + turns) % 4]
    return abs(x) + abs(y)


def run_2(steps) -> int:
    ship_x = ship_y = 0
    wp_x, wp_y = 10, -1
    for action, val in steps:
        if action in dmap:
            wp_x, wp_y = dmap[action](wp_x, wp_y, val)
        elif action == 'F':
            ship_x += val * wp_x
            ship_y += val * wp_y
        elif action in 'LR':
            s = action == 'R' or -1
            for _ in range(val // 90):
                wp_x, wp_y = s * -wp_y, s * wp_x
    return abs(ship_x) + abs(ship_y)


def main():
    text = (Path(__file__).parent / "../../input/2020/input_12.txt").read_text()
    steps = [(t[0], int(t[1:])) for t in text.splitlines()]
    print("Part 1:", run_1(steps))
    print("Part 2:", run_2(steps))


if __name__ == "__main__":
    main()
