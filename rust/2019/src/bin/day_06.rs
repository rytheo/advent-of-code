use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let text = fs::read_to_string("../input/2019/input_06.txt").unwrap();
    let orbits: HashMap<_, _> = text.lines().map(|line| {
        let (parent, child) = line.split_once(')').unwrap();
        (child, parent)
    }).collect();
    let mut total = 0;
    for mut body in orbits.keys() {
        while *body != "COM" {
            total += 1;
            body = &orbits[body];
        }
    }
    println!("Part 1: {}", total);
    let paths: Vec<_> = ["YOU", "SAN"].iter().map(|mut body| {
        let mut path: HashSet<_> = HashSet::new();
        while *body != "COM" {
            body = &orbits[body];
            path.insert(body);
        }
        path
    }).collect();
    println!("Part 2: {}", paths[0].symmetric_difference(&paths[1]).count());
}
