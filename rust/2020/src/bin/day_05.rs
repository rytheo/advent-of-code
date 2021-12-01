use std::fs;

fn main() {
    let input = fs::read_to_string("../input/2020/input_05.txt").unwrap();
    let mut seat_ids: Vec<_> = input.lines().map(|s| {
        let binary = s
            .replace(|c| "BR".contains(c), "1")
            .replace(|c| "FL".contains(c), "0");
        u16::from_str_radix(&binary, 2).unwrap()
    }).collect();
    seat_ids.sort();
    println!("Part 1: {}", seat_ids[seat_ids.len()-1]);
    for (i, sid) in seat_ids.iter().enumerate().skip(1) {
        if sid - seat_ids[i-1] > 1 {
            println!("Part 2: {}", sid - 1);
            break;
        }
    }
}
