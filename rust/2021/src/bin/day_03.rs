use std::fs;

fn get_deltas(lines: &[&str]) -> Vec<i32> {
    let mut deltas: Vec<i32> = vec![0; lines[0].len()];
    for line in lines {
        for (i, c) in line.bytes().enumerate() {
            deltas[i] += if c == b'1' { 1 } else { -1 }
        }
    }
    deltas
}

fn find_rating(lines: &[&str], o2: bool) -> u32 {
    let mut lines = Vec::from(lines);
    let mut i = 0;
    while lines.len() > 1 {
        let deltas = get_deltas(&lines);
        // If 1 is most frequent or equal, keep 1 for o2 and 0 for co2
        let keep = if (deltas[i] >= 0) == o2 { b'1' } else { b'0' };
        lines = lines.into_iter().filter(|s| s.as_bytes()[i] == keep).collect();
        i += 1;
    }
    u32::from_str_radix(lines[0], 2).unwrap()
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_03.txt").unwrap();
    let lines: Vec<_> = input.lines().collect();
    let tracker = get_deltas(&lines);
    let gamma: u32 = tracker.iter().rev().enumerate()
        .map(|(i, &b)| if b > 0 { 1 << i } else { 0 })
        .sum();
    let epsilon = !gamma & (1 << tracker.len()) - 1;
    println!("Part 1: {}", gamma * epsilon);
    let oxygen = find_rating(&lines, true);
    let co2 = find_rating(&lines, false);
    println!("Part 2: {}", oxygen * co2);
}
