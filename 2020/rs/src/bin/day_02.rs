use std::fs;

fn main() {
    let input = fs::read_to_string("../input/input_02.txt").unwrap();
    let mut total_1 = 0u32;
    let mut total_2 = 0u32;
    for line in input.split('\n').filter(|s| s.len() > 0) {
        let tokens: Vec<_> = line.split(&[' ', '-'][..]).collect();
        let a: usize = tokens[0].parse().unwrap();
        let b: usize = tokens[1].parse().unwrap();
        let c: u8 = tokens[2].as_bytes()[0];
        let password = tokens[3].as_bytes();
        let c_count = password.iter().filter(|&&byte| byte == c).count();
        if a <= c_count && c_count <= b {
            total_1 += 1;
        }
        if (password[a-1] == c) != (password[b-1] == c) {
            total_2 += 1;
        }
    }
    println!("Part 1: {}", total_1);
    println!("Part 2: {}", total_2);
}
