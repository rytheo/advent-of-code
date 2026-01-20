use std::fs;

fn run(cmds: &[(&str, u64)], advanced: bool) -> u64 {
    let (mut aim, mut x, mut y) = (0, 0, 0);
    for (dir, val) in cmds {
        match *dir {
            "forward" => {
                x += val;
                if advanced {
                    y += aim * val
                }
            }
            "down" => {
                if advanced {
                    aim += val
                } else {
                    y += val
                }
            }
            "up" => {
                if advanced {
                    aim -= val
                } else {
                    y -= val
                }
            }
            _ => unreachable!(),
        }
    }
    x * y
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_02.txt").unwrap();
    let cmds: Vec<_> = input
        .lines()
        .map(|line| {
            let (a, b) = line.split_once(' ').unwrap();
            (a, b.parse::<u64>().unwrap())
        })
        .collect();
    println!("Part 1: {}", run(&cmds, false));
    println!("Part 2: {}", run(&cmds, true));
}
