use regex::Regex;
use std::sync::OnceLock;

fn required_amounts(s: &str) -> [usize; 3] {
    static RE: OnceLock<Regex> = OnceLock::new();
    let re = RE.get_or_init(|| Regex::new(r"(\d+) (red|green|blue)").unwrap());
    let mut amounts = [0, 0, 0];
    for m in re.captures_iter(s) {
        let amount = m[1].parse().unwrap();
        let index = match &m[2] {
            "red" => 0,
            "green" => 1,
            "blue" => 2,
            _ => unreachable!("capture should be a valid color"),
        };
        amounts[index] = amounts[index].max(amount);
    }
    amounts
}

fn validate_game(amounts: &[usize; 3]) -> bool {
    amounts.into_iter().zip(&[12, 13, 14]).all(|(a, b)| a <= b)
}

fn main() {
    let input = aoc::read_input("2023/input_02.txt");
    let arrays: Vec<_> = input.lines().map(required_amounts).collect();
    let id_sum: usize = arrays
        .iter()
        .enumerate()
        .filter_map(|(i, amounts)| validate_game(amounts).then_some(i + 1))
        .sum();
    println!("Part 1: {id_sum}");
    let power_sum = arrays
        .iter()
        .map(|amounts| amounts.iter().product::<usize>())
        .sum::<usize>();
    println!("Part 2: {power_sum}");
}
