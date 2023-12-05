use std::collections::HashSet;
use std::sync::OnceLock;

use regex::Regex;

fn parse_digits(s: &str) -> HashSet<u32> {
    static RE: OnceLock<Regex> = OnceLock::new();
    let re = RE.get_or_init(|| Regex::new(r"\d+").unwrap());
    re.find_iter(s)
        .map(|m| m.as_str().parse().unwrap())
        .collect()
}

fn count_matches(line: &str) -> usize {
    let (_, data) = line.split_once(':').unwrap();
    let (left, right) = data.split_once('|').unwrap();
    let scratched = parse_digits(left);
    let winning = parse_digits(right);
    scratched.intersection(&winning).count()
}

fn main() {
    let input = aoc::read_input("2023/input_04.txt");
    let match_counts: Vec<_> = input.lines().map(count_matches).collect();
    let score_total: usize = match_counts
        .iter()
        .map(|count| match count {
            0 => 0,
            n => 2usize.pow((n - 1) as u32),
        })
        .sum();
    println!("Part 1: {score_total}");
    let mut copy_counts: Vec<usize> = vec![1; match_counts.len()];
    for i in 0..copy_counts.len() {
        for j in (i + 1)..=(i + match_counts[i]) {
            copy_counts[j] += copy_counts[i];
        }
    }
    println!("Part 2: {}", copy_counts.into_iter().sum::<usize>());
}
