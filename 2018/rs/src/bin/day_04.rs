use std::collections::HashMap;
use std::fs;

fn main() {
    let text = fs::read_to_string("../input/input_04.txt").unwrap();
    let mut logs: Vec<_> = text.lines().collect();
    logs.sort();
    let mut tracker: HashMap<u32, HashMap<u32, u32>> = HashMap::new();
    let (mut guard, mut sleep) = (0, 0);
    for log in logs {
        let parts: Vec<_> = log.split(' ').collect();
        let minute = parts[1][3..5].parse().unwrap();
        match parts[2] {
            "Guard" => guard = parts[3][1..].parse().unwrap(),
            "falls" => sleep = minute,
            "wakes" => for t in sleep..minute {
                *tracker.entry(guard).or_default().entry(t).or_default() += 1;
            }
            _ => unreachable!(),
        }
    }
    let sleepers = [
        tracker.keys().max_by_key(|k| tracker[k].values().sum::<u32>()).unwrap(),
        tracker.keys().max_by_key(|k| tracker[k].values().max().unwrap()).unwrap(),
    ];
    for (i, &sleepy) in sleepers.iter().enumerate() {
        let minute = tracker[sleepy].keys().max_by_key(|k| tracker[sleepy][k]).unwrap();
        println!("Part {}: {}", i+1, sleepy * minute);
    }
}
