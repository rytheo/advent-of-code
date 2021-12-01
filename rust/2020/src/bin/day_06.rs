use std::collections::HashSet;
use std::fs;

fn main() {
    let alpha: HashSet<_> = "abcdefghijklmnopqrstuvwxyz".chars().collect();
    let input = fs::read_to_string("../input/input_06.txt").unwrap();
    println!("Part 1: {}", input.split("\n\n").map(|g| {
        g.chars().filter(|&c| c != '\n').collect::<HashSet<_>>().len()
    }).sum::<usize>());
    println!("Part 2: {}", input.split("\n\n").map(|g| {
        g.lines().map(|p| p.chars().collect())
            .fold(alpha.clone(), |a, b| a.intersection(&b).cloned().collect())
            .len()
    }).sum::<usize>());
}
