use std::fs;

fn invalid_index(nums: &[i64]) -> Option<i64> {
    for (i, &n) in nums.iter().enumerate().skip(25) {
        if !nums[i-25..i].iter().any(|&a| nums[i-25..i].contains(&(n - a))) {
            return Some(n);
        }
    }
    None
}

fn main() {
    let input = fs::read_to_string("../input/input_09.txt").unwrap();
    let nums: Vec<i64> = input.split_terminator('\n').map(|s| s.parse().unwrap()).collect();
    let invalid = invalid_index(&nums).unwrap();
    println!("Part 1: {}", invalid);
    let (mut lower, mut upper) = (0, 0);
    let mut total = nums[0];
    while total != invalid {
        if total < invalid {
            upper += 1;
            total += nums[upper]
        } else {
            total -= nums[lower];
            lower += 1;
        }
    }
    let min = nums[lower..upper+1].iter().min().unwrap();
    let max = nums[lower..upper+1].iter().max().unwrap();
    println!("Part 2: {}", min + max);
}
