use std::fs;

fn main() {
    let text = fs::read_to_string("../input/2017/input_01.txt").unwrap();
    let digits: Vec<_> = text.chars().filter_map(|c| c.to_digit(10)).collect();
    for (n, offset) in (1..).zip([1, digits.len() / 2]) {
        let total: u32 = (0..digits.len())
            .filter_map(|i| (digits[i] == digits[(i + offset) % digits.len()]).then_some(digits[i]))
            .sum();
        println!("Part {n}: {total}");
    }
}
