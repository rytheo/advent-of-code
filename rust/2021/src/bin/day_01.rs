use std::fs;

fn count_increases(values: &[u64]) -> usize {
    (1..values.len()).filter(|&i| values[i] > values[i-1]).count()
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_01.txt").unwrap();
    let values: Vec<u64> = input.lines().map(|s| s.parse().unwrap()).collect();
    println!("Part 1: {}", count_increases(&values));
    let window_sums: Vec<u64> = (2..values.len()).map(|i| values[i-2..=i].iter().sum()).collect();
    println!("Part 2: {}", count_increases(&window_sums));
}
