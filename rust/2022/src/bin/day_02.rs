use aoc::Part;

fn score_line(line: &str, part: Part) -> i32 {
    let line = line.as_bytes();
    let them = (line[0] - b'A') as i32;
    let hint = (line[2] - b'X') as i32;
    let outcome;
    let you;
    match part {
        Part::One => {
            you = hint;
            outcome = (you - them + 1).rem_euclid(3);
        }
        Part::Two => {
            outcome = hint;
            you = (them + outcome - 1).rem_euclid(3);
        }
    }
    you + 1 + outcome * 3
}

fn main() {
    let input = aoc::read_input("2022/input_02.txt");
    for part in [Part::One, Part::Two] {
        let total: i32 = input.lines().map(|line| score_line(line, part)).sum();
        println!("{part}: {total}");
    }
}
