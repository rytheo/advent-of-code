use std::collections::HashSet;
use std::fs;

fn main() {
    let text = fs::read_to_string("../input/input_01.txt").unwrap();
    let mut freqs = HashSet::new();
    let changes: Vec<i32> = text.lines().map(|s| s.parse().unwrap()).collect();
    println!("Part 1: {}", changes.iter().sum::<i32>());
    let mut freq = 0;
    for c in changes.iter().cycle() {
        if freqs.contains(&freq) {
            println!("Part 2: {}", freq);
            break;
        }
        freqs.insert(freq);
        freq += c;
    }
}
