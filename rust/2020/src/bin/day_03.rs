use std::fs;

fn tree_count(grid: &[&[u8]], dx: usize, dy: usize) -> usize {
    let (mut x, mut y) = (0, 0);
    let mut count = 0;
    while y < grid.len() {
        if grid[y][x] == b'#' {
            count += 1;
        }
        x = (x + dx) % grid[0].len();
        y += dy;
    }
    count
}

fn main() {
    let input = fs::read_to_string("../input/2020/input_03.txt").unwrap();
    let grid: Vec<_> = input
        .lines()
        .filter(|s| !s.is_empty())
        .map(|s| s.as_bytes())
        .collect();
    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    println!("Part 1: {}", tree_count(&grid, 3, 1));
    let p2: usize = slopes
        .iter()
        .map(|&(dx, dy)| tree_count(&grid, dx, dy))
        .product();
    println!("Part 2: {}", p2);
}
