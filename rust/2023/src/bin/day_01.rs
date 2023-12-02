use regex::Regex;
use std::fs;

const WORDS: [&'static str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

enum Direction {
    Forward,
    Reverse,
}

fn init_regex(dir: Direction) -> Regex {
    let mut pat = String::new();
    for word in WORDS {
        pat += "(";
        match dir {
            Direction::Forward => pat += word,
            Direction::Reverse => pat.extend(word.chars().rev()),
        }
        pat += ")|";
    }
    pat += r"(\d)";
    Regex::new(&pat).unwrap()
}

fn find_first_digit(s: &str, re: &Regex) -> u32 {
    let caps = re.captures(s).unwrap();
    for i in 1..=9 {
        if caps.get(i).is_some() {
            return i as u32;
        }
    }
    caps[10].parse().unwrap()
}

fn find_easy(s: &str) -> u32 {
    let first = s
        .matches(|c: char| c.is_ascii_digit())
        .next()
        .unwrap()
        .parse::<u32>()
        .unwrap();
    let last = s
        .rmatches(|c: char| c.is_ascii_digit())
        .next()
        .unwrap()
        .parse::<u32>()
        .unwrap();
    first * 10 + last
}

fn find_hard(s: &str, re_forward: &Regex, re_reverse: &Regex) -> u32 {
    let first = find_first_digit(s, re_forward);
    let last = find_first_digit(&s.chars().rev().collect::<String>(), re_reverse);
    first * 10 + last
}

fn main() {
    let input = fs::read_to_string("../input/2023/input_01.txt").unwrap();
    let re_forward = init_regex(Direction::Forward);
    let re_reverse = init_regex(Direction::Reverse);
    let p1 = input.trim().lines().map(find_easy).sum::<u32>();
    println!("Part 1: {p1}",);
    let p2 = input
        .trim()
        .lines()
        .map(|s| find_hard(s, &re_forward, &re_reverse))
        .sum::<u32>();
    println!("Part 2: {p2}",);
}
