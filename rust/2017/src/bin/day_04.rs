use aoc::counter::Counter;
use itertools::Itertools;
use std::fs;

fn validate(s: &str, anagrams_ok: bool) -> bool {
    let counter: Counter<_> = s
        .split_whitespace()
        .map(|w| match anagrams_ok {
            true => w.to_owned(),
            false => w.chars().sorted().collect::<String>(),
        })
        .collect();
    counter.values().all(|&v| v < 2)
}

fn main() {
    let input = fs::read_to_string("../input/2017/input_04.txt").unwrap();
    for (i, anagrams_ok) in [(1, true), (2, false)] {
        let num_valid = input.lines().filter(|s| validate(s, anagrams_ok)).count();
        println!("Part {i}: {num_valid}");
    }
}
