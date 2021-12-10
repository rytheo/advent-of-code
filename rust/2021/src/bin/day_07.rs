use std::fs;

fn min_fuel(crabs: &[i32], constant: bool) -> i32 {
    let (lower, upper) = (crabs[0], crabs[crabs.len() - 1]);
    (lower..=upper).map(|pos| {
        crabs.iter().map(|&x| {
            let dist = (x - pos).abs();
            if constant { dist } else { dist * (dist + 1) / 2}
        }).sum()
    }).min().unwrap()
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_07.txt").unwrap();
    let mut crabs: Vec<i32> = input.trim().split(',').map(|s| s.parse().unwrap()).collect();
    crabs.sort();
    println!("Part 1: {}", min_fuel(&crabs, true));
    println!("Part 2: {}", min_fuel(&crabs, false));
}
