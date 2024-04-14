use std::collections::HashMap;

const DIR_MAX_SIZE: u64 = 100_000;
const TOTAL_DISK_SPACE: u64 = 70_000_000;
const UPDATE_SIZE: u64 = 30_000_000;

fn main() {
    let input = aoc::read_input("2022/input_07.txt");
    let mut dir_sizes: HashMap<&str, u64> = HashMap::new();
    let mut path = vec!["/"];
    for line in input.lines() {
        if let Some(dir) = line.strip_prefix("$ cd ") {
            match dir {
                "/" => path.truncate(1),
                ".." => drop(path.pop()),
                _ => path.push(dir),
            }
        } else if line.starts_with(|c: char| c.is_ascii_digit()) {
            let (size_str, _) = line.split_once(' ').unwrap();
            let file_size: u64 = size_str.parse().unwrap();
            for dir in &path {
                *dir_sizes.entry(dir).or_default() += file_size;
            }
        }
    }
    let small_sum: u64 = dir_sizes.values().filter(|&&v| v <= DIR_MAX_SIZE).sum();
    println!("Part 1: {small_sum}");
    let free_space = TOTAL_DISK_SPACE - dir_sizes["/"];
    let min_to_free = UPDATE_SIZE - free_space;
    let dir_size = dir_sizes.values().filter(|&&v| v >= min_to_free).min().unwrap();
    println!("Part 2: {dir_size}");
}
