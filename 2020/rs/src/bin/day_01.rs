use std::fs;

fn find(vals: &[u64], sum: u64, prod: u64, depth: usize) -> Option<u64> {
    for (i, &x) in vals.iter().enumerate() {
        let new_sum = sum + x;
        let new_prod = prod * x;
        if new_sum > 2020 {
            return None;
        }
        if depth == 1 {
            if new_sum == 2020 {
                return Some(new_prod);
            }
        } else if let Some(x) = find(&vals[i+1..], new_sum, new_prod, depth-1) {
            return Some(x);
        }
    }
    None
}

fn main() {
    let input = fs::read_to_string("../input/input_01.txt").unwrap();
    let mut entries: Vec<u64> = input.split_terminator('\n')
        .map(|s| s.parse().unwrap())
        .collect();
    entries.sort();
    println!("Part 1: {}", find(&entries, 0, 1, 2).unwrap());
    println!("Part 2: {}", find(&entries, 0, 1, 3).unwrap());
}
