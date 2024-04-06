use std::collections::HashSet;

fn priority(c: u8) -> u8 {
    match c {
        b'a'..=b'z' => c - b'a' + 1,
        b'A'..=b'Z' => c - b'A' + 27,
        _ => panic!("input should be an ASCII letter"),
    }
}

fn find_duplicate(sack: &str) -> u8 {
    let (front, back) = sack.split_at(sack.len() / 2);
    let front_set: HashSet<_> = front.bytes().collect();
    let back_set: HashSet<_> = back.bytes().collect();
    *front_set.intersection(&back_set).next().unwrap()
}

fn find_badge(sacks: &[&str]) -> u8 {
    sacks
        .iter()
        .map(|sack| sack.bytes().collect::<HashSet<_>>())
        .reduce(|a, b| &a & &b)
        .unwrap()
        .into_iter()
        .next()
        .unwrap()
}

fn main() {
    let input = aoc::read_input("2022/input_03.txt");
    let sacks: Vec<_> = input.lines().collect();
    let p1 = sacks
        .iter()
        .map(|sack| priority(find_duplicate(sack)) as u32)
        .sum::<u32>();
    println!("Part 1: {p1}");
    let p2 = sacks
        .chunks(3)
        .map(|group| priority(find_badge(group)) as u32)
        .sum::<u32>();
    println!("Part 2: {p2}");
}
