use aoc::geometry::Vector;
use aoc::vecN;
use std::collections::HashMap;
use std::fs;

fn main() {
    let text = fs::read_to_string("../input/2017/input_03.txt").unwrap();
    let input: u32 = text.trim().parse().unwrap();
    let mut here = Vector::zero();
    let mut grid = HashMap::from([(here, 1)]);
    let mut square = 1;
    let mut side_length = 0;
    let deltas = [vecN!(1, 0), vecN!(0, -1), vecN!(-1, 0), vecN!(0, 1)];
    let (mut p1, mut p2) = (None, None);
    'outer: for delta in deltas.into_iter().cycle() {
        // Increase side length at each horizontal edge
        if delta[0] != 0 {
            side_length += 1;
        }
        for _ in 0..side_length {
            here += delta;
            square += 1;
            if p2.is_none() {
                let value = here
                    .neighbors()
                    .map(|p| grid.get(&p).copied().unwrap_or(0))
                    .sum();
                grid.insert(here, value);
                if value > input {
                    p2 = Some(value);
                }
            }
            if square == input {
                p1 = Some(here.dist_manhattan(Vector::zero()));
                break 'outer;
            }
        }
    }
    println!("Part 1: {}", p1.unwrap());
    println!("Part 2: {}", p2.unwrap());
}
