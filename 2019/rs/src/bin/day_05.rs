use std::fs;

use aoc_2019::CPU;

fn main() {
    let text = fs::read_to_string("../input/input_05.txt").unwrap();
    let program: Vec<_> = text.trim().split(',').map(|s| s.parse().unwrap()).collect();
    for (i, &p) in [1, 5].iter().enumerate() {
        let mut cpu = CPU::new(&program);
        cpu.input.push_back(p.into());
        cpu.run();
        println!("Part {}: {}", i + 1, cpu.output.pop_back().unwrap());
    }
}
