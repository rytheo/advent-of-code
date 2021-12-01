use std::fs;

fn main() {
    let text = fs::read_to_string("../input/2019/input_01.txt").unwrap();
    let module_fuel: i32 = text.lines().map(|line| line.parse::<i32>().unwrap() / 3 - 2).sum();
    println!("Part 1: {}", module_fuel);
    let total_fuel: i32 = text.lines().map(|line| {
        let mut total = 0;
        let mut mass = line.parse::<i32>().unwrap() / 3 - 2;
        while mass > 0 {
            total += mass;
            mass = mass / 3 - 2;
        }
        total
    }).sum();
    println!("Part 2: {}", total_fuel);
}
