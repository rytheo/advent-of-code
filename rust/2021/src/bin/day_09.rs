use std::fs;
use std::collections::{HashMap, HashSet};

type Point = (isize, isize);
const DELTAS: [Point; 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

fn basin_size(grid: &HashMap<Point, u32>, low: Point) -> usize {
    let mut visited = HashSet::new();
    let mut to_visit = vec![low];
    while let Some(here @ (y, x)) = to_visit.pop() {
        visited.insert(here);
        for (dy, dx) in DELTAS {
            let adj = (y + dy, x + dx);
            if let Some(&h) = grid.get(&adj) {
                if grid[&here] < h && h < 9 && !visited.contains(&adj) {
                    to_visit.push(adj);
                }
            }
        }
    }
    visited.len()
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_09.txt").unwrap();
    let grid: HashMap<_, _> = input.lines().enumerate().map(|(y, line)| {
        line.bytes().enumerate().map(move |(x, b)| ((y as isize, x as isize), (b - b'0') as u32))
    }).flatten().collect();
    let low_points: Vec<_> = grid.keys().filter(|(y, x)| {
        DELTAS.iter().all(|&(dy, dx)| {
            let adj = (y + dy, x + dx);
            grid[&(*y, *x)] < *grid.get(&adj).unwrap_or(&9)
        })
    }).collect();
    println!("Part 1: {}", low_points.iter().map(|p| grid[p] + 1).sum::<u32>());
    let mut basin_sizes: Vec<_> = low_points.iter().map(|&&p| basin_size(&grid, p)).collect();
    basin_sizes.sort();
    println!("Part 2: {}", basin_sizes.iter().rev().take(3).product::<usize>());
}
