use std::fs;

fn react(bytes: &[u8], remove: Option<u8>) -> usize {
    let mut stack = Vec::with_capacity(bytes.len());
    for b in bytes {
        // Skip letters to be removed
        if let Some(r) = remove {
            if b.eq_ignore_ascii_case(&r) {
                continue;
            }
        }
        // Destroy adjacent letters of opposite case
        if let Some(a) = stack.last() {
            if a ^ b == 32 {
                stack.pop();
                continue;
            }
        }
        stack.push(*b);
    }
    stack.len()
}

fn main() {
    let text = fs::read_to_string("../input/2018/input_05.txt").unwrap();
    let bytes = text.trim().as_bytes();
    println!("Part 1: {}", react(bytes, None));
    println!(
        "Part 2: {}",
        (b'a'..=b'z').map(|r| react(bytes, Some(r))).min().unwrap()
    );
}
