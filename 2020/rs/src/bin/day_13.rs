use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("../input/input_13.txt").unwrap();
    let lines: Vec<_> = input.lines().collect();
    // Part 1
    let start: u64 = lines[0].parse().unwrap();
    let bus_data: HashMap<u64, u64> = lines[1].split(',')
        .enumerate()
        .filter_map(|(i, b)| match b.parse() {
            Ok(x) => Some((x, i as u64)),
            Err(_) => None,
        })
        .collect();
    let first = bus_data.keys().min_by_key(|&&bus| start - start % bus + bus).unwrap();
    println!("Part 1: {}", first * (first - start % first));
    // Part 2
    let (mut time, mut period) = (0, 1);
    for (bus, offset) in bus_data {
        while (time + offset) % bus != 0 {
            time += period;
        }
        period *= bus;
    }
    println!("Part 2: {}", time);
}
