use aoc::counter::Counter;
use std::collections::HashMap;
use std::fs;

fn simulate(rule_map: &HashMap<(u8, u8), u8>, bytes: &[u8], steps: usize) -> usize {
    // Track the number of elements and pairs
    let mut elements: Counter<_> = bytes.iter().copied().collect();
    let mut pairs: Counter<_> = bytes
        .iter()
        .copied()
        .zip(bytes.iter().copied().skip(1))
        .collect();
    // Run simulation
    for _ in 0..steps {
        let prev = pairs;
        pairs = Counter::new();
        for (p @ (a, b), n) in prev {
            let c = rule_map[&p];
            elements[&c] += n;
            for pair in [(a, c), (c, b)] {
                pairs[&pair] += n;
            }
        }
    }
    let least = elements.values().min().unwrap();
    let most = elements.values().max().unwrap();
    most - least
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_14.txt").unwrap();
    let (template, rules) = input.split_once("\n\n").unwrap();
    let template_bytes = template.as_bytes();
    // Map element pairs to elements
    let rule_map: HashMap<_, _> = rules
        .lines()
        .map(|line| {
            let (pair, elem) = line.split_once(" -> ").unwrap();
            let bytes = pair.as_bytes();
            ((bytes[0], bytes[1]), elem.as_bytes()[0])
        })
        .collect();
    println!("Part 1: {}", simulate(&rule_map, template_bytes, 10));
    println!("Part 2: {}", simulate(&rule_map, template_bytes, 40));
}
