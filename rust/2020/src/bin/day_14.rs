use std::collections::HashMap;
use std::fs;

fn write_1(mem: &mut HashMap<u64, u64>, addr: u64, mut val: u64, mask: &str) {
    for (i, b) in mask.chars().rev().enumerate() {
        match b {
            '0' => val &= !(1 << i),
            '1' => val |= 1 << i,
            _ => (),
        }
    }
    mem.insert(addr, val);
}

fn write_2(mem: &mut HashMap<u64, u64>, mut addr: u64, val: u64, mask: &str) {
    let mut x_indices = vec![];
    for (i, b) in mask.chars().rev().enumerate() {
        match b {
            'X' => x_indices.push(i),
            '1' => addr |= 1 << i,
            _ => (),
        }
    }
    for combo in 0u32..(1 << x_indices.len()) {
        let bits = format!("{:032b}", combo.reverse_bits());
        for (b, i) in bits.chars().zip(&x_indices) {
            match b == '1' {
                true => addr |= 1 << i,
                false => addr &= !(1 << i),
            }
        }
        mem.insert(addr, val);
    }
}

fn run(program: &[&str], advanced: bool) -> u64 {
    let mut mem = HashMap::new();
    let mut mask = "";
    for line in program {
        let tokens: Vec<_> = line.split(" = ").collect();
        let (op, val) = (tokens[0], tokens[1]);
        if op == "mask" {
            mask = val;
        } else {
            let addr = op[4..op.len() - 1].parse().unwrap();
            let val = val.parse().unwrap();
            match advanced {
                false => write_1(&mut mem, addr, val, mask),
                true => write_2(&mut mem, addr, val, mask),
            }
        }
    }
    mem.values().sum()
}

fn main() {
    let input = fs::read_to_string("../input/2020/input_14.txt").unwrap();
    let program: Vec<_> = input.lines().collect();
    println!("Part 1: {}", run(&program, false));
    println!("Part 2: {}", run(&program, true));
}
