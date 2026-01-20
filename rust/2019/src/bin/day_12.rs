use itertools::Itertools;
use num_integer::Integer;
use std::{cmp::Ordering, fs};

#[derive(Clone)]
struct Moon {
    pos: Vec<i32>,
    vel: Vec<i32>,
}

impl Moon {
    fn new(pos: Vec<i32>) -> Moon {
        Moon {
            vel: vec![0; pos.len()],
            pos,
        }
    }

    fn energy(&self) -> u32 {
        self.pos.iter().map(|p| p.unsigned_abs()).sum::<u32>()
            * self.vel.iter().map(|v| v.unsigned_abs()).sum::<u32>()
    }
}

fn step(moons: &mut [Moon], dims: &[usize]) {
    // Apply gravity
    for (a, b) in (0..moons.len()).tuple_combinations() {
        for &i in dims {
            match moons[a].pos[i].cmp(&moons[b].pos[i]) {
                Ordering::Less => {
                    moons[a].vel[i] += 1;
                    moons[b].vel[i] -= 1;
                }
                Ordering::Greater => {
                    moons[a].vel[i] -= 1;
                    moons[b].vel[i] += 1;
                }
                Ordering::Equal => {}
            }
        }
    }
    // Apply velocity
    for m in moons {
        for &i in dims {
            m.pos[i] += m.vel[i];
        }
    }
}

fn find_period(moons: &mut [Moon], dim: usize) -> u64 {
    let initial: Vec<_> = moons.iter().map(|m| (m.pos[dim], m.vel[dim])).collect();
    let mut t = 0;
    loop {
        t += 1;
        step(moons, &[dim]);
        if moons
            .iter()
            .map(|m| (m.pos[dim], m.vel[dim]))
            .collect::<Vec<_>>()
            == initial
        {
            return t;
        }
    }
}

fn main() {
    let text = fs::read_to_string("../input/2019/input_12.txt").unwrap();
    let moons: Vec<_> = text
        .lines()
        .map(|line| {
            let pos = line
                .split(|c| "<xyz=, >".contains(c))
                .filter_map(|s| s.parse().ok())
                .collect();
            Moon::new(pos)
        })
        .collect();
    let mut moons_p1 = moons.clone();
    for _ in 0..1000 {
        step(&mut moons_p1, &[0, 1, 2]);
    }
    println!(
        "Part 1: {}",
        moons_p1.iter().map(|m| m.energy()).sum::<u32>()
    );
    let periods: Vec<_> = (0..3)
        .map(|dim| find_period(&mut moons.clone(), dim))
        .collect();
    println!("Part 2: {}", periods.iter().fold(1, |a, b| a.lcm(b)));
}
