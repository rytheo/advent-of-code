use aoc::Part;
use std::collections::HashSet;

fn find_packet_offset(input: &str, marker_len: usize) -> Option<usize> {
    input
        .as_bytes()
        .windows(marker_len)
        .enumerate()
        .find_map(|(i, window)| {
            (window.iter().collect::<HashSet<_>>().len() == window.len()).then_some(i + marker_len)
        })
}

fn main() {
    let input = aoc::read_input("2022/input_06.txt");
    for (part, marker_len) in [(Part::One, 4), (Part::Two, 14)] {
        println!(
            "{part}: {}",
            find_packet_offset(&input, marker_len).unwrap()
        );
    }
}
