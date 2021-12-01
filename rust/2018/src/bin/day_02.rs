use std::collections::HashMap;
use std::fs;

fn single_diff(a: &str, b: &str) -> Option<usize> {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    let mut diff = None;
    for k in 0..a.len() {
        if a[k] != b[k] {
            match diff {
                None => diff = Some(k),
                Some(_) => return None,
            }
        }
    }
    diff
}

fn main() {
    let text = fs::read_to_string("../input/input_02.txt").unwrap();
    let (mut two, mut three) = (0, 0);
    let ids: Vec<_> = text.lines().collect();
    for id in &ids {
        let mut counter: HashMap<_, u32> = HashMap::new();
        for b in id.bytes() {
            *counter.entry(b).or_default() += 1;
        }
        if counter.values().any(|&v| v == 2) {
            two += 1;
        }
        if counter.values().any(|&v| v == 3) {
            three += 1;
        }
    }
    println!("Part 1: {}", two * three);
    for i in 0..ids.len() {
        for j in i+1..ids.len() {
            let (a, b) = (ids[i], ids[j]);
            if let Some(k) = single_diff(a, b) {
                println!("Part 2: {}{}", &a[..k], &a[k+1..]);
                return;
            }
        }
    }
}
