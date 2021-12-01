use std::cmp::Ordering;
use std::fs;

fn valid(p: u32, strict: bool) -> bool {
    let s = p.to_string();
    let b = s.as_bytes();
    let mut min = false;
    let mut exact = false;
    let mut adj = 1;
    for i in 1..s.len() {
        match b[i-1].cmp(&b[i]) {
            Ordering::Less => {
                if adj == 2 { exact = true; }
                adj = 1;
            }
            Ordering::Equal => adj += 1,
            Ordering::Greater => return false,
        }
        if adj == 2 { min = true; }
    }
    if strict { exact || adj == 2 } else { min }
}

fn main() {
    let text = fs::read_to_string("../input/2019/input_04.txt").unwrap();
    let (left, right) = text.trim().split_once('-').unwrap();
    let low: u32 = left.parse().unwrap();
    let high: u32 = right.parse().unwrap();
    println!("Part 1: {}", (low..=high).filter(|&p| valid(p, false)).count());
    println!("Part 2: {}", (low..=high).filter(|&p| valid(p, true)).count());
}
