use std::fs;
use std::cmp::Ordering;
use regex::Regex;

fn fire(vel: (i32, i32), target: (i32, i32, i32, i32)) -> Option<i32> {
    let (x0, x1, y0, y1) = target;
    let (mut dx, mut dy) = vel;
    let mut max_y = 0;
    let (mut x, mut y) = (0, 0);
    while y >= y0 {
        x += dx;
        y += dy;
        max_y = max_y.max(y);
        if x0 <= x && x <= x1 && y0 <= y && y <= y1 {
            return Some(max_y);
        }
        match dx.cmp(&0) {
            Ordering::Less => dx += 1,
            Ordering::Greater => dx -= 1,
            _ => (),
        }
        dy -= 1;
    }
    None
}

fn main() {
    let re = Regex::new(r"-?\d+").unwrap();
    let input = fs::read_to_string("../input/2021/input_17.txt").unwrap();
    let coords: Vec<i32> = re.find_iter(&input).map(|m| m.as_str().parse().unwrap()).collect();
    let (x0, x1, y0, y1) = (coords[0], coords[1], coords[2], coords[3]);
    let mut heights = vec![];
    // Brute force initial velocity
    for dy in -y0.abs()..=y0.abs() {
        for dx in x0.min(0)..=x1.max(0) {
            if let Some(y) = fire((dx, dy), (x0, x1, y0, y1)) {
                heights.push(y);
            }
        }
    }
    println!("Part 1: {}", heights.iter().max().unwrap());
    println!("Part 2: {}", heights.len());
}
