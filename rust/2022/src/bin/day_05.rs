use aoc::Part;

const NUM_STACKS: usize = 9;
const CRATE_WIDTH: usize = 3;

struct Step {
    amount: usize,
    source: usize,
    dest: usize,
}

fn parse_stacks(input: &str) -> [Vec<char>; NUM_STACKS] {
    let rows = input.lines().rev().skip(1);
    let mut stacks: [Vec<_>; NUM_STACKS] = Default::default();
    for row in rows {
        for (chunk, stack) in row.as_bytes().chunks(CRATE_WIDTH + 1).zip(&mut stacks) {
            let val = char::from(chunk[1]);
            if val.is_alphabetic() {
                stack.push(val);
            }
        }
    }
    stacks
}

fn parse_step(line: &str) -> Step {
    let mut nums = line.split_whitespace().filter_map(|s| s.parse().ok());
    Step {
        amount: nums.next().unwrap(),
        source: nums.next().unwrap() - 1,
        dest: nums.next().unwrap() - 1,
    }
}

fn do_steps(stacks: &mut [Vec<char>], steps: &[Step], part: Part) {
    for step in steps {
        let source = &mut stacks[step.source];
        let selected = source.split_off(source.len() - step.amount);
        let dest = &mut stacks[step.dest];
        let original_len = dest.len();
        dest.extend_from_slice(&selected);
        if part == Part::One {
            dest[original_len..].reverse();
        }
    }
}

fn main() {
    let input = aoc::read_input("2022/input_05.txt");
    let (stack_input, steps_input) = input.split_once("\n\n").unwrap();
    let steps: Vec<_> = steps_input.lines().map(parse_step).collect();
    for part in [Part::One, Part::Two] {
        let mut stacks = parse_stacks(stack_input);
        do_steps(&mut stacks, &steps, part);
        print!("{part}: ");
        for stack in stacks {
            print!("{}", stack.last().unwrap());
        }
        println!();
    }
}
