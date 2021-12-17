use std::fs;
use std::collections::HashMap;

fn simulate(rule_map: &HashMap<(u8, u8), u8>, bytes: &[u8], steps: usize) -> usize {
    // Track the number of elements and pairs
    let mut elements: HashMap<_, usize> = HashMap::new();
    for &byte in bytes {
        *elements.entry(byte).or_default() += 1;
    }
    let mut pairs: HashMap<_, usize> = HashMap::new();
    for i in 1..bytes.len() {
        *pairs.entry((bytes[i-1], bytes[i])).or_default() += 1;
    }
    // Run simulation
    for _ in 0..steps {
        let prev = pairs;
        pairs = HashMap::new();
        for (p @ (a, b), n) in prev {
            let c = rule_map[&p];
            *elements.entry(c).or_default() += n;
            for pair in [(a, c), (c, b)] {
                *pairs.entry(pair).or_default() += n;
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
    let rule_map: HashMap<_, _> = rules.lines().map(|line| {
        let (pair, elem) = line.split_once(" -> ").unwrap();
        let bytes = pair.as_bytes();
        ((bytes[0], bytes[1]), elem.as_bytes()[0])
    }).collect();
    println!("Part 1: {}", simulate(&rule_map, &template_bytes, 10));
    println!("Part 2: {}", simulate(&rule_map, &template_bytes, 40));
}
