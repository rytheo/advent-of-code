use std::fs;
use std::collections::{HashMap, HashSet};

type Signal = HashSet<u8>;

fn count_easy(entry: &str) -> usize {
    let (_, output) = entry.split_once(" | ").unwrap();
    output.split(' ').filter(|w| [2, 4, 3, 7].contains(&w.len())).count()
}

fn parse_entry(entry: &str) -> usize {
    let (input, output) = entry.split_once(" | ").unwrap();
    let mut len2sig: HashMap<usize, Vec<Signal>> = HashMap::new();
    for token in input.split(' ') {
        len2sig.entry(token.len()).or_default().push(token.bytes().collect());
    }
    // Start with the numbers of unique length
    let mut sigs: HashMap<usize, Signal> = [
        (1, len2sig[&2][0].clone()), (4, len2sig[&4][0].clone()),
        (7, len2sig[&3][0].clone()), (8, len2sig[&7][0].clone()),
    ].into_iter().collect();
    // Derive the rest
    let adg = len2sig[&5].iter().fold(sigs[&8].clone(), |acc, sig| &acc & sig);
    sigs.insert(3, &sigs[&1] | &adg);
    sigs.insert(9, &sigs[&4] | &adg);
    let be = &sigs[&8] - &sigs[&3];
    let d = &(&sigs[&4] - &sigs[&1]) - &be;
    sigs.insert(0, &sigs[&8] - &d);
    sigs.insert(6, len2sig[&6].iter().filter(|&s| *s != sigs[&0] && *s != sigs[&9]).next().unwrap().clone());
    sigs.insert(5, &sigs[&9] & &sigs[&6]);
    let ce = &sigs[&9] ^ &sigs[&6];
    sigs.insert(2, &adg | &ce);
    // Parse the output signals
    let digits: Vec<_> = output.split(' ').map(|s| {
        let signal: Signal = s.bytes().collect();
        for (num, sig) in &sigs {
            if *sig == signal {
                return *num;
            }
        }
        panic!("Unrecognized signal")
    }).collect();
    digits.iter().fold(0, |acc, &b| 10 * acc + b)
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_08.txt").unwrap();
    println!("Part 1: {}", input.lines().map(count_easy).sum::<usize>());
    println!("Part 2: {}", input.lines().map(parse_entry).sum::<usize>());
}
