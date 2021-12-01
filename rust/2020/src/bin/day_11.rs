use std::fs;

fn in_bounds(grid: &Vec<Vec<char>>, y: i32, x: i32) -> bool {
    0 <= y && 0 <= x && (y as usize) < grid.len() && (x as usize) < grid[0].len()
}

fn neighbors(grid: &Vec<Vec<char>>, y: i32, x: i32) -> u32 {
    let mut count = 0;
    for &y1 in &[y-1, y, y+1] {
        for &x1 in &[x-1, x, x+1] {
            if in_bounds(grid, y1, x1) && (y1 != y || x1 != x) && grid[y1 as usize][x1 as usize] == '#' {
                count += 1
            }
        }
    }
    count
}

fn visible(grid: &Vec<Vec<char>>, y: i32, x: i32) -> u32 {
    let mut count = 0;
    for &dy in &[-1, 0, 1] {
        for &dx in &[-1, 0, 1] {
            if dy == 0 && dx == 0 {
                continue;
            }
            let (mut y1, mut x1) = (y, x);
            loop {
                y1 += dy;
                x1 += dx;
                if !in_bounds(grid, y1, x1) || grid[y1 as usize][x1 as usize] == 'L' {
                    break;
                }
                if grid[y1 as usize][x1 as usize] == '#' {
                    count += 1;
                    break;
                }
            }
        }
    }
    count
}

fn simulate(mut grid: Vec<Vec<char>>, advanced: bool) -> usize {
    let mut stable = false;
    while !stable {
        stable = true;
        let mut g_prime = grid.clone();
        for y in 0..grid.len() {
            for x in 0..grid[0].len() {
                let n = if advanced { visible(&grid, y as i32, x as i32) } else { neighbors(&grid, y as i32, x as i32) };
                if n == 0 && grid[y][x] == 'L' {
                    g_prime[y][x] = '#';
                    stable = false;
                } else if n >= 4 + (advanced as u32) && grid[y][x] == '#' {
                    g_prime[y][x] = 'L';
                    stable = false;
                }
            }
        }
        grid = g_prime;
    }
    grid.iter().flatten().filter(|&&s| s == '#').count()
}

fn main() {
    let input = fs::read_to_string("../input/input_11.txt").unwrap();
    let grid: Vec<Vec<char>> = input.lines().map(|s| s.chars().collect()).collect();
    println!("Part 1: {}", simulate(grid.clone(), false));
    println!("Part 2: {}", simulate(grid, true));
}
