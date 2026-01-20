use aoc::geometry::{vecN, Vector};
use std::collections::{HashMap, HashSet};
use std::fs;

fn basin_size(grid: &HashMap<Vector<2>, u32>, low: Vector<2>) -> usize {
    let mut visited = HashSet::new();
    let mut to_visit = vec![low];
    while let Some(here) = to_visit.pop() {
        visited.insert(here);
        for adj in here.adjacents() {
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
    let grid: HashMap<_, _> = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.bytes()
                .enumerate()
                .map(move |(x, b)| (vecN!(x as i32, y as i32), (b - b'0') as u32))
        })
        .collect();
    let low_points: Vec<_> = grid
        .keys()
        .filter(|point| {
            point
                .adjacents()
                .all(|adj| grid[point] < *grid.get(&adj).unwrap_or(&9))
        })
        .collect();
    println!(
        "Part 1: {}",
        low_points.iter().map(|p| grid[p] + 1).sum::<u32>()
    );
    let mut basin_sizes: Vec<_> = low_points.iter().map(|&&p| basin_size(&grid, p)).collect();
    basin_sizes.sort();
    println!(
        "Part 2: {}",
        basin_sizes.iter().rev().take(3).product::<usize>()
    );
}
