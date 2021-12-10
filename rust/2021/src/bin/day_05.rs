use std::fs;
use std::collections::HashMap;

fn count_intersections(lines: &[(i32, i32, i32, i32)], diag_ok: bool) -> usize {
    let mut grid: HashMap<(i32, i32), usize> = HashMap::new();
    for &(x1, y1, x2, y2) in lines {
        let dx = (x2 - x1).clamp(-1, 1);
        let dy = (y2 - y1).clamp(-1, 1);
        if !(dx == 0 || dy == 0 || diag_ok) {
            continue;
        }
        let (mut x, mut y) = (x1, y1);
        for _ in 0..=(x2 - x1).abs().max((y2 - y1).abs()) {
            *grid.entry((x, y)).or_default() += 1;
            x += dx;
            y += dy;
        }
    }
    grid.values().filter(|&&v| v > 1).count()
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_05.txt").unwrap();
    let lines: Vec<_> = input.lines().map(|line| {
        let nums: Vec<i32> = line.split(&[' ', '-', '>', ','][..])
            .filter_map(|s| s.parse().ok())
            .collect();
        (nums[0], nums[1], nums[2], nums[3])
    }).collect();
    println!("Part 1: {}", count_intersections(&lines, false));
    println!("Part 2: {}", count_intersections(&lines, true));
}
