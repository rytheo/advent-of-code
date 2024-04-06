use aoc::Part;
use std::ops::RangeInclusive;

fn parse_ranges(line: &str) -> [RangeInclusive<u8>; 2] {
    let vals: Vec<_> = line
        .split(&['-', ','])
        .map(|s| s.parse::<u8>().unwrap())
        .collect();
    [vals[0]..=vals[1], vals[2]..=vals[3]]
}

fn full_overlap(a: &RangeInclusive<u8>, b: &RangeInclusive<u8>) -> bool {
    a.start() <= b.start() && b.end() <= a.end() || b.start() <= a.start() && a.end() <= b.end()
}

fn partial_overlap(a: &RangeInclusive<u8>, b: &RangeInclusive<u8>) -> bool {
    a.start() <= b.end() && b.start() <= a.end()
}

fn main() {
    let input = aoc::read_input("2022/input_04.txt");
    let range_pairs: Vec<_> = input.lines().map(parse_ranges).collect();
    for (part, check_fn) in [
        (Part::One, full_overlap as fn(_, _) -> bool),
        (Part::Two, partial_overlap as fn(_, _) -> bool),
    ] {
        println!(
            "{part}: {}",
            range_pairs.iter().filter(|[a, b]| check_fn(a, b)).count()
        );
    }
}
