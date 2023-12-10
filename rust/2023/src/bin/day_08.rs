use std::{collections::HashMap, sync::OnceLock};

use regex::Regex;

enum Direction {
    Left,
    Right,
}

fn parse_dir(c: char) -> Direction {
    match c {
        'L' => Direction::Left,
        'R' => Direction::Right,
        _ => panic!("Invalid char"),
    }
}

fn parse_network(s: &str) -> HashMap<&str, (&str, &str)> {
    static RE: OnceLock<Regex> = OnceLock::new();
    let re = RE.get_or_init(|| Regex::new(r"(\w+) = \((\w+), (\w+)\)").unwrap());
    s.lines()
        .map(|line| {
            let caps = re.captures(line).unwrap();
            let key = caps.get(1).unwrap().as_str();
            let left = caps.get(2).unwrap().as_str();
            let right = caps.get(3).unwrap().as_str();
            (key, (left, right))
        })
        .collect()
}

fn count_steps(
    network: &HashMap<&str, (&str, &str)>,
    directions: &[Direction],
    start: &str,
    predicate: fn(&str) -> bool,
) -> usize {
    let mut current = start;
    let mut steps = 0;
    let mut first_find = None;
    for direction in directions.iter().cycle() {
        let (left, right) = network[current];
        current = match direction {
            Direction::Left => left,
            Direction::Right => right,
        };
        steps += 1;
        if predicate(current) {
            // The network has important properties for solving part 2 easily
            if let Some((count, node_name)) = first_find {
                // The distance from the start equals the loop length
                assert_eq!(steps, count);
                // Each A node only reaches one Z node in its loop
                assert_eq!(current, node_name);
                break;
            } else {
                first_find = Some((steps, current));
                steps = 0;
            }
        }
    }
    steps
}

fn main() {
    let input = aoc::read_input("2023/input_08.txt");
    let (head, body) = input.split_once("\n\n").unwrap();
    let directions: Vec<_> = head.chars().map(parse_dir).collect();
    let network = parse_network(body.trim());
    let part_1 = count_steps(&network, &directions, "AAA", |s| s == "ZZZ");
    println!("Part 1: {part_1}");
    let part_2 = network
        .keys()
        .filter(|s| s.ends_with('A'))
        .map(|start| count_steps(&network, &directions, start, |s| s.ends_with('Z')))
        .reduce(num::integer::lcm)
        .unwrap();
    println!("Part 2: {part_2}");
}
