use std::fs;
use std::collections::HashMap;

fn simulate(nums: &[u8], days: usize) -> u64 {
    let mut fish_counter = HashMap::new();
    for &timer in nums {
        *fish_counter.entry(timer).or_default() += 1;
    }
    for _ in 0..days {
        let previous = fish_counter;
        fish_counter = HashMap::new();
        for (timer, count) in previous {
            if timer > 0 {
                *fish_counter.entry(timer - 1).or_default() += count;
            } else {
                *fish_counter.entry(6).or_default() += count;
                fish_counter.insert(8, count);
            }
        }
    }
    fish_counter.values().sum()
}
fn main() {
    let input = fs::read_to_string("../input/2021/input_06.txt").unwrap();
    let nums: Vec<_> = input.trim().split(',').map(|s| s.parse().unwrap()).collect();
    println!("Part 1: {}", simulate(&nums, 80));
    println!("Part 2: {}", simulate(&nums, 256));
}
