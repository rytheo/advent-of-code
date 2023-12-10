use aoc::Part;

fn extrapolate(values: &[i32], part: Part) -> i32 {
    if values.iter().all(|&v| v == 0) {
        return 0;
    }
    let diff: Vec<_> = values.windows(2).map(|pair| pair[1] - pair[0]).collect();
    match part {
        Part::One => values.last().unwrap() + extrapolate(&diff, part),
        Part::Two => values.first().unwrap() - extrapolate(&diff, part),
    }
}

fn main() {
    let input = aoc::read_input("2023/input_09.txt");
    let histories: Vec<Vec<_>> = input
        .lines()
        .map(|line| line.split(' ').map(|s| s.parse().unwrap()).collect())
        .collect();
    for part in [Part::One, Part::Two] {
        let sum: i32 = histories.iter().map(|hist| extrapolate(hist, part)).sum();
        println!("{part}: {sum}");
    }
}
