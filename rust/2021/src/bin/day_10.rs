use std::fs;

fn autocomplete(line: &str) -> Result<u64, u64> {
    let mut stack = vec![];
    // Read brackets into stack
    for byte in line.bytes() {
        match byte {
            b'(' | b'[' | b'{' | b'<' => stack.push(byte),
            close => match stack.last() {
                // Check matching brackets by comparing ASCII value difference
                Some(&open) if open < close && close - open < 3 => {
                    stack.pop();
                }
                _ => {
                    return Err(match close {
                        b')' => 3,
                        b']' => 57,
                        b'}' => 1197,
                        b'>' => 25137,
                        _ => panic!("Invalid byte"),
                    })
                }
            },
        }
    }
    // Calculate auto-completion score
    Ok(stack.iter().rfold(0, |acc, &open| {
        5 * acc
            + match open {
                b'(' => 1,
                b'[' => 2,
                b'{' => 3,
                b'<' => 4,
                _ => panic!("Invalid byte"),
            }
    }))
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_10.txt").unwrap();
    let mut corrupt_total = 0;
    let mut auto_scores = vec![];
    for result in input.lines().map(autocomplete) {
        match result {
            Ok(score) => auto_scores.push(score),
            Err(score) => corrupt_total += score,
        }
    }
    auto_scores.sort();
    println!("Part 1: {}", corrupt_total);
    println!("Part 2: {}", auto_scores[auto_scores.len() / 2])
}
