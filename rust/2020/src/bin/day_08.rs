use std::fs;

fn swap(op: &mut &str) {
    match *op {
        "nop" => *op = "jmp",
        "jmp" => *op = "nop",
        _ => (),
    }
}

fn fix(program: &mut [(&str, i32)]) -> i32 {
    for i in 0..program.len() {
        swap(&mut program[i].0);
        if let Some(v) = run(program, false) {
            return v;
        }
        swap(&mut program[i].0);
    }
    unreachable!()
}

fn run(program: &[(&str, i32)], acc_on_loop: bool) -> Option<i32> {
    let mut visited = vec![false; program.len()];
    let (mut i, mut acc) = (0, 0);
    while i < program.len() {
        if visited[i] {
            return match acc_on_loop {
                true => Some(acc),
                false => None,
            };
        }
        visited[i] = true;
        let (op, n) = program[i];
        match op {
            "acc" => acc += n,
            "jmp" => i = (i as i32 + n - 1) as usize,
            _ => (),
        }
        i += 1;
    }
    Some(acc)
}

fn main() {
    let input = fs::read_to_string("../input/2020/input_08.txt").unwrap();
    let mut program: Vec<_> = input
        .lines()
        .map(|line| {
            let tokens: Vec<_> = line.split(' ').collect();
            (tokens[0], tokens[1].parse::<i32>().unwrap())
        })
        .collect();
    println!("Part 1: {}", run(&program, true).unwrap());
    println!("Part 2: {}", fix(&mut program));
}
