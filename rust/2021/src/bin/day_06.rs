use std::fs;
use aoc::counter::Counter;

fn simulate(nums: &[u8], days: usize) -> usize {
    let mut fish_counter: Counter<_> = nums.iter().cloned().collect();
    for _ in 0..days {
        let previous = fish_counter;
        fish_counter = Counter::new();
        for (timer, count) in previous {
            if timer > 0 {
                fish_counter[&(timer - 1)] += count;
            } else {
                fish_counter[&6] += count;
                fish_counter[&8] = count;
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
