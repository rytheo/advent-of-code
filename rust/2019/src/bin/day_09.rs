use std::fs;

use aoc_2019::CPU;

fn main() {
    let text = fs::read_to_string("../input/2019/input_09.txt").unwrap();
    let program: Vec<_> = text.trim().split(',').map(|s| s.parse().unwrap()).collect();
    for i in 1..=2 {
        let mut cpu = CPU::new(&program);
        cpu.input.push_back(i.into());
        cpu.run();
        println!("Part {}: {}", i, cpu.output.pop_front().unwrap());
    }
}
