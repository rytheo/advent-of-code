use aoc::geometry::Vector;
use aoc::vecN;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::fs;

fn shortest_path(grid: &HashMap<Vector<2>, u32>) -> u32 {
    let &corner = grid.keys().max_by_key(|p| p[0] + p[1]).unwrap();
    let mut costs = HashMap::from([(Vector::zero(), 0)]);
    let mut to_visit = BinaryHeap::from([(Reverse(0), Vector::zero())]);
    // Compute minimum cost to enter each space
    while let Some((Reverse(cost), point)) = to_visit.pop() {
        if point == corner {
            return cost;
        }
        for adj in point.adjacents() {
            // Skip points that are out of bounds or visited
            if !grid.contains_key(&adj) || costs.contains_key(&adj) {
                continue;
            }
            let new_cost = cost + grid[&adj];
            costs.insert(adj, new_cost);
            to_visit.push((Reverse(new_cost), adj));
        }
    }
    panic!("Exhausted grid without finding corner cost")
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_15.txt").unwrap();
    let mut grid: HashMap<_, _> = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.bytes()
                .enumerate()
                .map(move |(x, b)| (vecN!(x as i32, y as i32), (b - b'0') as u32))
        })
        .collect();
    println!("Part 1: {}", shortest_path(&grid));
    // Make the grid bigger
    let corner = grid.keys().max_by_key(|p| p[0] + p[1]).unwrap();
    let width = corner[0] + 1;
    let height = corner[1] + 1;
    for (point, risk) in grid.clone() {
        for x_scale in 0..5 {
            for y_scale in 0..5 {
                // Skip the top-left area since we start with that data
                if x_scale == 0 && y_scale == 0 {
                    continue;
                }
                grid.insert(
                    vecN!(point[0] + width * x_scale, point[1] + height * y_scale),
                    // Sub 1, mod 9, add 1 since we need digits in 1-9 inclusive
                    (risk + (x_scale + y_scale) as u32 - 1) % 9 + 1,
                );
            }
        }
    }
    println!("Part 2: {}", shortest_path(&grid));
}
