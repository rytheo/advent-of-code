use std::fs;
use std::collections::{HashMap, HashSet};

fn step(octopi: &mut HashMap<(isize, isize), u8>) -> usize {
    // Increase all energy by 1
    // Any octopus with energy > 9 flashes
    let mut to_flash = vec![];
    for (&point, energy) in octopi.iter_mut() {
        *energy += 1;
        if *energy > 9 {
            to_flash.push(point);
        }
    }
    // Increase adjacent energy by 1
    // If this causes an octopus to have energy > 9, it also flashes
    let mut flashed = HashSet::new();
    while let Some(point @ (y, x)) = to_flash.pop() {
        flashed.insert(point);
        for dy in -1..=1 {
            for dx in -1..=1 {
                if dy == 0 && dx == 0 {
                    continue;
                }
                let adj = (y + dy, x + dx);
                if let Some(energy) = octopi.get_mut(&adj) {
                    *energy += 1;
                    if *energy == 10 && !flashed.contains(&adj) {
                        to_flash.push(adj);
                    }
                }
            }
        }
    }
    // Any octopus that flashed has its energy set to 0
    for &p in &flashed {
        octopi.insert(p, 0);
    }
    flashed.len()
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_11.txt").unwrap();
    let mut octopi = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, digit) in line.bytes().enumerate() {
            octopi.insert((y as isize, x as isize), digit - b'0');
        }
    }
    println!("Part 1: {}", (0..100).map(|_| step(&mut octopi)).sum::<usize>());
    for n in 101.. {
        if step(&mut octopi) == octopi.len() {
            println!("Part 2: {}", n);
            break;
        }
    }
}
