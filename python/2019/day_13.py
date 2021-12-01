import curses
import re
from pathlib import Path
from typing import TYPE_CHECKING, Any, Optional, cast

from intcode import CPU


if TYPE_CHECKING:
    from _curses import _CursesWindow as Window
else:
    Window = Any


Point = tuple[int, int]
tile_map = [" ", "█", "#", "=", "o"]


def draw(win: Window, tiles: dict[Point, int]):
    for (x, y), tid in tiles.items():
        win.addch(y, x, tile_map[tid])
    win.refresh()


def update_tiles(cpu: CPU, tiles: dict[Point, int]) -> tuple[Optional[int], Optional[Point], Optional[Point]]:
    score = paddle = ball = None
    while cpu.output:
        x, y, tid = [cpu.output.popleft() for _ in range(3)]
        if (x, y) == (-1, 0):
            score = tid
        else:
            tiles[(x, y)] = tid
            if tid == 3:
                paddle = (x, y)
            elif tid == 4:
                ball = (x, y)
    return score, paddle, ball


def run(stdscr: Window) -> tuple[int, int]:
    stdscr.clear()
    text = (Path(__file__).parent / "../input/input_13.txt").read_text()
    # Replace the paddle row with walls
    pattern = r"1,(0,)+3,(0,)+1,"
    if m := re.search(pattern, text):
        text = re.sub(pattern, "1," * (len(m[0]) // 2), text)
    program = [int(x) for x in text.split(",")]
    # Initialize game
    cpu = CPU(program)
    tiles = {}
    cpu.run()
    score, paddle, ball = cast(tuple[int, int, int], update_tiles(cpu, tiles))
    # Get the screen dimensions
    width = max(t[0] for t in tiles) + 1
    height = max(t[1] for t in tiles) + 1
    win = curses.newwin(height + 1, width, 0, 0)
    block_count = sum(v == 2 for v in tiles.values())
    # Run game to completion
    cpu = CPU(program)
    cpu.mem[0] = 2
    tiles = {}
    while True:
        retcode = cpu.run()
        s, p, b = update_tiles(cpu, tiles)
        score = s or score
        paddle = p or paddle
        ball = b or ball
        draw(win, tiles)
        if retcode == 99:
            break
        cpu.input.append(0)
    return block_count, score


def main():
    p1, p2 = curses.wrapper(run)
    print(f"Part 1: {p1}\nPart 2: {p2}")


if __name__ == "__main__":
    main()
