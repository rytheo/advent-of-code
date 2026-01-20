use std::fs;

fn run(offsets: &[isize], strange: bool) -> usize {
    let mut offsets = offsets.to_owned();
    let mut position = 0isize;
    let mut steps = 0;
    while 0 <= position && (position as usize) < offsets.len() {
        let offset = offsets[position as usize];
        let delta = if strange && offset >= 3 { -1 } else { 1 };
        offsets[position as usize] += delta;
        position += offset;
        steps += 1;
    }
    steps
}

fn main() {
    let input = fs::read_to_string("../input/2017/input_05.txt").unwrap();
    let offsets = input
        .lines()
        .map(|s| s.parse().unwrap())
        .collect::<Vec<isize>>();
    for (i, strange) in [(1, false), (2, true)] {
        println!("Part {i}: {}", run(&offsets, strange));
    }
}
