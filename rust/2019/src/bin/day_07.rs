use itertools::Itertools;
use rug::Integer;
use std::fs;

use aoc_2019::CPU;

fn try_combo(program: &[Integer], phases: &[Integer], feedback: bool) -> Integer {
    let mut amps: Vec<_> = (0..5).map(|i| {
        let mut cpu = CPU::new(&program);
        cpu.input.push_back(phases[i].clone());
        cpu
    }).collect();
    let mut signal = Integer::new();
    for i in (0..5).cycle() {
        amps[i].input.push_back(signal.clone());
        let op = amps[i].run();
        signal = amps[i].output.pop_front().unwrap();
        if (!feedback || op == 99) && i == 4 {
            break;
        }
    }
    signal
}

fn main() {
    let text = fs::read_to_string("../input/2019/input_07.txt").unwrap();
    let program: Vec<_> = text.trim().split(',').map(|s| s.parse().unwrap()).collect();
    for (i, range, feedback) in vec![(1, 0..5, false), (2, 5..10, true)].into_iter() {
        let max_signal = range.map_into().permutations(5)
            .map(|p| try_combo(&program, &p, feedback))
            .max().unwrap();
        println!("Part {}: {}", i, max_signal);
    }
}
