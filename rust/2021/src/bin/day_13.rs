use std::fs;
use std::collections::HashSet;

fn fold_grid(grid: &mut HashSet<(u32, u32)>, axis: char, val: u32) {
    for (x, y) in grid.clone() {
        if axis == 'x' && val < x {
            grid.remove(&(x, y));
            grid.insert((2 * val - x, y));
        } else if axis == 'y' && val < y {
            grid.remove(&(x, y));
            grid.insert((x, 2 * val - y));
        }
    }
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_13.txt").unwrap();
    let (points, orders) = input.split_once("\n\n").unwrap();
    // Parse grid and instructions
    let mut grid: HashSet<(u32, u32)> = points.lines().map(|line| {
        let (x, y) = line.split_once(',').unwrap();
        (x.parse().unwrap(), y.parse().unwrap())
    }).collect();
    let folds: Vec<(char, u32)> = orders.lines().map(|line| {
        let (axis, val) = line.split_once('=').unwrap();
        (axis.chars().last().unwrap(), val.parse().unwrap())
    }).collect();
    // Perform first fold
    let mut fold_iter = folds.into_iter();
    let (axis, val) = fold_iter.next().unwrap();
    fold_grid(&mut grid, axis, val);
    println!("Part 1: {}", grid.len());
    // Perform the rest
    for (axis, val) in fold_iter {
        fold_grid(&mut grid, axis, val);
    }
    // Print resulting dots
    let x_max = *grid.iter().map(|(x, _)| x).max().unwrap();
    let y_max = *grid.iter().map(|(_, y)| y).max().unwrap();
    println!("Part 2:");
    for y in 0..=y_max {
        for x in 0..=x_max {
            print!("{}", if grid.contains(&(x, y)) { '#' } else { ' ' })
        }
        println!();
    }
}
