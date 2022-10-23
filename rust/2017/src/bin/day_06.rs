use std::cmp::Reverse;
use std::collections::HashMap;
use std::fs;
use std::mem;

fn redist(banks: &mut [u32]) {
    // Use min_by_key with Reverse to get the first largest bank's index
    let mut i = (0..banks.len()).min_by_key(|&i| Reverse(banks[i])).unwrap();
    let blocks = mem::take(&mut banks[i]);
    // Redistribute evenly
    for _ in 0..blocks {
        i = (i + 1) % banks.len();
        banks[i] += 1;
    }
}

fn main() {
    let input = fs::read_to_string("../input/2017/input_06.txt").unwrap();
    let mut banks: Vec<u32> = input.split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();
    let mut seen = HashMap::from([(banks.clone(), 0)]);
    for cycle in 1.. {
        redist(&mut banks);
        if let Some(previous) = seen.insert(banks.clone(), cycle) {
            println!("Part 1: {cycle}");
            println!("Part 2: {}", cycle - previous);
            return;
        }
    }
}
