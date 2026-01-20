use rug::Integer;
use std::fs;

use aoc_2019::CPU;

fn run(program: &[Integer], noun: u32, verb: u32) -> Integer {
    let mut cpu = CPU::new(program);
    cpu.mem.insert(1.into(), noun.into());
    cpu.mem.insert(2.into(), verb.into());
    cpu.run();
    cpu.mem.remove(&Integer::new()).unwrap()
}

fn main() {
    let text = fs::read_to_string("../input/2019/input_02.txt").unwrap();
    let program: Vec<Integer> = text
        .trim_end()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();
    println!("Part 1: {}", run(&program, 12, 2));
    for noun in 0..=99 {
        for verb in 0..=99 {
            if run(&program, noun, verb) == 19690720 {
                println!("Part 2: {}", 100 * noun + verb);
                return;
            }
        }
    }
}
