use std::collections::{HashMap, HashSet};
use std::fs;

fn find_pair<'a>(tracker: &HashMap<&'a str, HashSet<usize>>) -> Option<(&'a str, usize)> {
    for (&field, positions) in tracker {
        if positions.len() == 1 {
            return Some((field, *positions.iter().next().unwrap()));
        }
    }
    None
}

fn main() {
    let input = fs::read_to_string("../input/input_16.txt").unwrap();
    let blocks: Vec<_> = input.trim().split("\n\n").collect();
    let rules: HashMap<_, _> = blocks[0].split('\n').map(|s| {
        let kv: Vec<_> = s.split(": ").collect();
        let nums: Vec<u64> = kv[1].split(&[' ', '-', 'o', 'r'][..])
            .filter_map(|n| n.parse().ok())
            .collect();
        (kv[0], vec![nums[0]..=nums[1], nums[2]..=nums[3]])
    }).collect();
    let your_ticket: Vec<u64> = blocks[1].split(&['\n', ','][..]).skip(1)
        .map(|t| t.parse().unwrap())
        .collect();
    let nearby_tickets: Vec<Vec<u64>> = blocks[2].split('\n').skip(1)
        .map(|s| s.split(',').map(|t| t.parse().unwrap()).collect())
        .collect();
    // Determine which tickets are invalid
    let error_rates: Vec<u64> = nearby_tickets.iter().map(|t| {
        t.iter().filter(|v| !rules.values().flatten().any(|r| r.contains(v))).sum()
    }).collect();
    println!("Part 1: {}", error_rates.iter().sum::<u64>());
    // Find possible positions for each field
    let valid_tickets: Vec<Vec<u64>> = nearby_tickets.into_iter().zip(error_rates)
        .filter_map(|(v, rate)| if rate == 0 { Some(v) } else { None })
        .collect();
    let mut tracker: HashMap<_, _> = rules.keys().map(|&k| (k, HashSet::new())).collect();
    for (field, ranges) in rules {
        for pos in 0..your_ticket.len() {
            if valid_tickets.iter().all(|t| ranges.iter().any(|r| r.contains(&t[pos]))) {
                tracker.get_mut(field).unwrap().insert(pos);
            }
        }
    }
    // Deduce field positions
    let mut pos_names = vec![""; your_ticket.len()];
    while let Some((field, pos)) = find_pair(&tracker) {
        pos_names[pos] = field;
        tracker.remove(field);
        for s in tracker.values_mut() {
            s.remove(&pos);
        }
    }
    println!("Part 2: {}", your_ticket.iter().zip(pos_names)
        .filter_map(|(val, field)| if field.starts_with("departure") { Some(val) } else { None })
        .product::<u64>()
    );
}
