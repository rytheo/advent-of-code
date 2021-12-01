use std::collections::HashMap;
use rug::Integer;
use std::fs;

use aoc_2019::CPU;

fn paint(program: &[Integer], start_white: bool) -> HashMap<(isize, isize), Integer> {
    let mut robot = CPU::new(&program);
    let (mut x, mut y, mut direction) = (0isize, 0isize, 0i32);
    let mut panels = HashMap::new();
    if start_white {
        panels.insert((0, 0), 1.into());
    }
    // Run until robot halts
    loop {
        // Give the robot its next colour
        robot.input.push_back(panels.get(&(x, y)).cloned().unwrap_or_default());
        if robot.run() == 99 {
            break panels;
        }
        // Paint the current tile
        panels.insert((x, y), robot.output.pop_front().unwrap());
        // Turn left or right
        let turn = if robot.output.pop_front().unwrap() == 0 { -1 } else { 1 };
        direction = (direction + turn).rem_euclid(4);
        // Move forward
        match direction {
            0 => y -= 1,
            1 => x += 1,
            2 => y += 1,
            3 => x -= 1,
            _ => unreachable!(),
        }
    }
}

fn main() {
    let text = fs::read_to_string("../input/input_11.txt").unwrap();
    let program: Vec<_> = text.trim().split(',').map(|s| s.parse().unwrap()).collect();
    println!("Part 1: {}", paint(&program, false).len());
    println!("Part 2:");
    let panels = paint(&program, true);
    let x0 = panels.keys().map(|&(x, _)| x).min().unwrap();
    let y0 = panels.keys().map(|&(_, y)| y).min().unwrap();
    let x1 = panels.keys().map(|&(x, _)| x).max().unwrap();
    let y1 = panels.keys().map(|&(_, y)| y).max().unwrap();
    for y in y0..=y1 {
        for x in x0..=x1 {
            match panels.get(&(x, y)) {
                Some(v) if *v == 1 => print!("#"),
                _ => print!(" "),
            }
        }
        println!();
    }
}
