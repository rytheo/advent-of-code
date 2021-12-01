use std::collections::HashMap;
use std::fs;

fn bag_contains(rules: &HashMap<&str, HashMap<&str, u32>>, colour: &str, target: &str) -> bool {
    rules[colour].contains_key(target)
    || rules[colour].keys().any(|c| bag_contains(rules, c, target))
}

fn bag_count(rules: &HashMap<&str, HashMap<&str, u32>>, colour: &str) -> u32 {
    1 + rules[colour].iter().map(|(c, q)| q * bag_count(rules, c)).sum::<u32>()
}

fn main() {
    let input = fs::read_to_string("../input/input_07.txt").unwrap();
    let rules: HashMap<_, _> = input.lines().map(|line| {
        let key = line.split(" bags contain ").next().unwrap();
        let counts: HashMap<_, _> = line.match_indices(char::is_numeric).map(|(i, s)| {
            let end = i + line[i..].find(" bag").unwrap();
            (&line[i+2..end], s.parse::<u32>().unwrap())
        }).collect();
        (key, counts)
    }).collect();
    println!("Part 1: {}", rules.keys().filter(|c| bag_contains(&rules, c, "shiny gold")).count());
    println!("Part 2: {}", bag_count(&rules, "shiny gold") - 1);
}
