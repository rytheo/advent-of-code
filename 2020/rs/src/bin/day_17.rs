use std::collections::{HashMap, HashSet};
use std::fs;

fn find_adj(deltas: &Vec<Vec<i32>>, cell: &Vec<i32>) -> Vec<Vec<i32>> {
    deltas.iter().map(|delta| {
        cell.iter().zip(delta).map(|(x, d)| x + d).collect()
    }).collect()
}

fn simulate(text: &str, deltas: &Vec<Vec<i32>>) -> usize {
    let mut active = HashSet::new();
    for (y, row) in text.split('\n').enumerate() {
        for (x, c) in row.chars().enumerate() {
            if c == '#' {
                match deltas[0].len() == 3 {
                    true => active.insert(vec![x as i32, y as i32, 0]),
                    false => active.insert(vec![x as i32, y as i32, 0, 0]),
                };
            }
        }
    }
    for _ in 0..6 {
        // Track all cells next to active cells
        let mut activity: HashMap<Vec<i32>, u32> = HashMap::new();
        for cell in &active {
            for adj in find_adj(deltas, cell) {
                *activity.entry(adj).or_default() += 1;
            }
        }
        // Compute next cycle based on adjacency counts
        active = activity.into_iter().filter_map(|(cell, count)| {
            if count == 3 || count == 2 && active.contains(&cell) { Some(cell) } else { None }
        }).collect();
    }
    active.len()
}

fn main() {
    let input = fs::read_to_string("../input/input_17.txt").unwrap();
    let d = [0, -1, 1];
    let d3: Vec<_> = (1..27).map(|i| vec![d[i % 3], d[i / 3 % 3], d[i / 9 % 3]]).collect();
    let d4: Vec<_> = (1..81).map(|i| vec![d[i % 3], d[i / 3 % 3], d[i / 9 % 3], d[i / 27 % 3]]).collect();
    println!("Part 1: {}", simulate(&input, &d3));
    println!("Part 2: {}", simulate(&input, &d4));
}
