use std::fs;

fn main() {
    let input = fs::read_to_string("../input/input_10.txt").unwrap();
    let mut jolts = vec![0];
    jolts.extend(input.lines().map(|s| s.parse::<i32>().unwrap()));
    jolts.sort();
    let device = jolts[jolts.len()-1] + 3;
    // Part 1
    let diff_1 = jolts.iter().enumerate().skip(1)
        .filter(|(i, &a)| a - jolts[i-1] == 1)
        .count();
    let diff_3 = jolts.iter().enumerate().skip(1)
        .filter(|(i, &a)| a - jolts[i-1] == 3)
        .count() + 1;
    println!("Part 1: {}", diff_1 * diff_3);
    // Part 2
    let mut ways = vec![0; jolts.len()];
    for (i, a) in jolts.iter().enumerate().rev() {
        ways[i] = (device - a <= 3) as u64;
        ways[i] += jolts[i+1..(i+4).min(jolts.len())].iter().enumerate().filter_map(|(j, b)| {
            if b - a <= 3 { Some(ways[j+i+1]) } else { None }
        }).fuse().sum::<u64>();
    }
    println!("Part 2: {}", ways[0]);
}
