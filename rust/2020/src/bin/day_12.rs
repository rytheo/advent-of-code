use std::fs;

use Cardinal::*;
use Relative::*;
use Direction::*;

enum Cardinal { East, South, West, North }

impl Cardinal {
    fn apply(&self, loc: (i32, i32), val: i32) -> (i32, i32) {
        let (x, y) = loc;
        match self {
            East => (x + val, y),
            South => (x, y + val),
            West => (x - val, y),
            North => (x, y - val),
        }
    }
}

enum Relative { Left, Right }

impl Relative {
    fn sign(&self) -> i32 {
        match self {
            Left => -1,
            Right => 1,
        }
    }
}

enum Direction {
    Card(Cardinal),
    Rel(Relative),
    Fwd,
}

fn run_1(steps: &[(Direction, i32)]) -> i32 {
    let mut face = East;
    let mut loc = (0, 0);
    for (dir, val) in steps {
        match dir {
            Card(c) => loc = c.apply(loc, *val),
            Fwd => loc = face.apply(loc, *val),
            Rel(d) => {
                let turns = d.sign() * val / 90;
                face = match (face as i32 + turns).rem_euclid(4) {
                    0 => East,
                    1 => South,
                    2 => West,
                    3 => North,
                    _ => unreachable!(),
                }
            }
        }
    }
    loc.0.abs() + loc.1.abs()
}

fn run_2(steps: &[(Direction, i32)]) -> i32 {
    let mut ship_loc = (0, 0);
    let mut wp_loc = (10, -1);
    for (dir, val) in steps {
        match dir {
            Card(c) => wp_loc = c.apply(wp_loc, *val),
            Fwd => ship_loc = (
                ship_loc.0 + val * wp_loc.0,
                ship_loc.1 + val * wp_loc.1,
            ),
            Rel(d) => {
                for _ in 0..(val / 90) {
                    wp_loc = (d.sign() * -wp_loc.1, d.sign() * wp_loc.0);
                }
            }
        }
    }
    ship_loc.0.abs() + ship_loc.1.abs()
}

fn main() {
    let input = fs::read_to_string("../input/2020/input_12.txt").unwrap();
    let steps: Vec<_> = input.lines().map(|s| {
        let dir = match &s[..1] {
            "E" => Card(East),
            "S" => Card(South),
            "W" => Card(West),
            "N" => Card(North),
            "L" => Rel(Left),
            "R" => Rel(Right),
            "F" => Fwd,
            s => panic!("Unknown action: {}", s),
        };
        (dir, s[1..].parse().unwrap())
    }).collect();
    println!("Part 1: {}", run_1(&steps));
    println!("Part 2: {}", run_2(&steps));
}
