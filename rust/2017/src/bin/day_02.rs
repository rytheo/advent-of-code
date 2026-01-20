use itertools::Itertools;
use std::fs;

fn main() {
    let text = fs::read_to_string("../input/2017/input_02.txt").unwrap();
    let mut checksums = [0, 0];
    for line in text.lines() {
        let values: Vec<u32> = line.split('\t').filter_map(|s| s.parse().ok()).collect();
        checksums[0] += values.iter().max().unwrap() - values.iter().min().unwrap();
        checksums[1] += values
            .iter()
            .permutations(2)
            .find_map(|p| {
                let (a, b) = (p[0], p[1]);
                (a % b == 0).then_some(a / b)
            })
            .unwrap();
    }
    for (n, checksum) in (1..).zip(checksums) {
        println!("Part {n}: {checksum}");
    }
}
