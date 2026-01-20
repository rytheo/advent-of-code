use rug::{Assign, Integer};
use std::collections::{HashMap, VecDeque};

pub struct CPU {
    pub mem: HashMap<Integer, Integer>,
    pub ip: Integer,
    pub ro: Integer,
    pub input: VecDeque<Integer>,
    pub output: VecDeque<Integer>,
}

impl CPU {
    pub fn new(program: &[Integer]) -> CPU {
        CPU {
            mem: program
                .iter()
                .enumerate()
                .map(|(i, v)| (Integer::from(i), v.clone()))
                .collect(),
            ip: Integer::new(),
            ro: Integer::new(),
            input: VecDeque::new(),
            output: VecDeque::new(),
        }
    }

    pub fn run(&mut self) -> u32 {
        loop {
            let (op, modes) = self.get_op();
            match op {
                99 => return op, // Halt
                3 => match self.input.pop_front() {
                    // Input
                    None => return op,
                    Some(v) => {
                        let mut p = self.get_params(1, &modes, true);
                        self.mem.insert(p.pop().unwrap(), v);
                        self.ip += 2;
                    }
                },
                4 => {
                    // Output
                    self.output.extend(self.get_params(1, &modes, false));
                    self.ip += 2;
                }
                5 | 6 => {
                    // Jump if true/false
                    let p = self.get_params(2, &modes, false);
                    if op == 5 && p[0] != 0 || op == 6 && p[0] == 0 {
                        self.ip.assign(&p[1]);
                    } else {
                        self.ip += 3;
                    }
                }
                9 => {
                    // Change relative offset
                    let p = self.get_params(1, &modes, false);
                    self.ro += &p[0];
                    self.ip += 2;
                }
                1 | 2 | 7 | 8 => {
                    // Combine two values
                    let mut p = self.get_params(3, &modes, true);
                    let c = p.pop().unwrap();
                    let (a, b) = (&p[0], &p[1]);
                    self.mem.insert(
                        c,
                        match op {
                            1 => Integer::from(a + b),
                            2 => Integer::from(a * b),
                            7 => Integer::from(a < b),
                            8 => Integer::from(a == b),
                            _ => unreachable!(),
                        },
                    );
                    self.ip += 4
                }
                _ => panic!("Unknown opcode: {}", op),
            }
        }
    }

    fn get_op(&self) -> (u32, [u32; 3]) {
        let inst = self.mem[&self.ip].to_u32().unwrap();
        (
            inst % 100,
            [inst / 100 % 10, inst / 1000 % 10, inst / 10000],
        )
    }

    fn get_params(&self, n: usize, modes: &[u32], write: bool) -> Vec<Integer> {
        (0..n)
            .map(|i| {
                let mut p = self
                    .mem
                    .get(&(self.ip.clone() + 1 + i as u8))
                    .cloned()
                    .unwrap_or_default();
                // Check for position or relative mode
                if modes[i] != 1 {
                    // Apply relative offset
                    if modes[i] == 2 {
                        p += &self.ro;
                    }
                    // Skip read if last param is a write address
                    if !(i == n - 1 && write) {
                        p = self.mem.get(&p).cloned().unwrap_or_default();
                    }
                }
                p
            })
            .collect()
    }
}
