use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let text = fs::read_to_string("../input/2018/input_03.txt").unwrap();
    let mut intact = HashSet::new();
    let mut claims: HashMap<(usize, usize), Vec<usize>> = HashMap::new();
    for line in text.lines() {
        let n: Vec<usize> = line
            .split(|c| "# @,:x".contains(c))
            .filter_map(|s| s.parse().ok())
            .collect();
        let (id, x, y, w, h) = (n[0], n[1], n[2], n[3], n[4]);
        intact.insert(id);
        for x1 in x..x + w {
            for y1 in y..y + h {
                claims.entry((x1, y1)).or_default().push(id);
            }
        }
    }
    println!(
        "Part 1: {}",
        claims.values().filter(|v| v.len() > 1).count()
    );
    for v in claims.values().filter(|v| v.len() > 1) {
        for id in v {
            intact.remove(id);
        }
    }
    println!("Part 2: {}", intact.iter().next().unwrap());
}
