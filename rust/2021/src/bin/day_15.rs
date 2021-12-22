use std::fs;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};

fn shortest_path(grid: &HashMap<(isize, isize), u32>) -> u32 {
    let mut costs = HashMap::new();
    let mut to_visit = BinaryHeap::from([(Reverse(0), (0, 0))]);
    // Compute minimum cost to enter each space
    while let Some((Reverse(cost), (x, y))) = to_visit.pop() {
        for adj in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)] {
            // Skip out-of-bounds
            if !grid.contains_key(&adj) {
                continue;
            }
            let new_cost = cost + grid[&adj];
            if !costs.contains_key(&adj) || new_cost < costs[&adj] {
                costs.insert(adj, new_cost);
                to_visit.push((Reverse(new_cost), adj));
            }
        }
    }
    let max_y = grid.keys().map(|(_, y)| *y).max().unwrap();
    let max_x = grid.keys().map(|(x, _)| *x).max().unwrap();
    costs[&(max_x, max_y)]
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_15.txt").unwrap();
    let mut grid: HashMap<_, _> = input.lines().enumerate().map(|(y, line)| {
        line.bytes().enumerate().map(move |(x, b)| ((x as isize, y as isize), (b - b'0') as u32))
    }).flatten().collect();
    println!("Part 1: {}", shortest_path(&grid));
    // Make the grid bigger
    let height = grid.keys().map(|(_, y)| *y).max().unwrap() + 1;
    let width = grid.keys().map(|(x, _)| *x).max().unwrap() + 1;
    for ((x, y), risk) in grid.clone() {
        for x_scale in 0..5 {
            for y_scale in 0..5 {
                // Skip the top-left area since we start with that data
                if x_scale == 0 && y_scale == 0 {
                    continue;
                }
                // Sub 1, mod 9, add 1 since we need digits in 1-9 inclusive
                grid.insert((x + x_scale * width, y + y_scale * height), (risk + (x_scale + y_scale) as u32 - 1) % 9 + 1);
            }
        }
    }
    println!("Part 2: {}", shortest_path(&grid));
}
