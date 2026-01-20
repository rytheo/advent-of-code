use std::fs;

fn play(nums: &[usize], target: usize) -> usize {
    let mut mem = vec![0; target];
    for (turn, &n) in (1..).zip(nums) {
        mem[n] = turn;
    }
    let mut n = 0;
    for turn in (nums.len() + 1)..target {
        let diff = match mem[n] {
            0 => 0,
            t => turn - t,
        };
        mem[n] = turn;
        n = diff;
    }
    n
}

fn main() {
    let input = fs::read_to_string("../input/2020/input_15.txt").unwrap();
    let nums: Vec<_> = input
        .trim()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();
    println!("Part 1: {}", play(&nums, 2020));
    println!("Part 2: {}", play(&nums, 30_000_000));
}
