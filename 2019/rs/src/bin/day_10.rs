use gcd::Gcd;
use std::fs;

fn scan(grid: &[&[u8]], loc: (isize, isize), vectors: &[(isize, isize)], vaporize: bool) -> usize {
    let mut total = 0;
    for &(dy, dx) in vectors {
        let (mut y, mut x) = (loc.0 + dy, loc.1 + dx);
        while 0 <= y && (y as usize) < grid.len() && 0 <= x && (x as usize) < grid[0].len() {
            if grid[y as usize][x as usize] == b'#' {
                total += 1;
                if vaporize && total == 200 {
                    return (100 * x + y) as usize;
                }
                break;
            }
            y += dy;
            x += dx;
        }
    }
    total
}

fn main() {
    let text = fs::read_to_string("../input/input_10.txt").unwrap();
    let grid: Vec<_> = text.lines().map(|s| s.as_bytes()).collect();
    let (h, w) = (grid.len() as isize, grid[0].len() as isize);
    let mut vectors = vec![];
    for y in -h..h {
        for x in -w..w {
            if x.unsigned_abs().gcd(y.unsigned_abs()) == 1 {
                vectors.push((y, x));
            }
        }
    }
    vectors.sort_by(|&(y0, x0), &(y1, x1)| {
        let a = -(x0 as f32).atan2(y0 as f32);
        let b = -(x1 as f32).atan2(y1 as f32);
        a.partial_cmp(&b).unwrap()
    });
    let mut station = (0, 0);
    let mut best = 0;
    for y in 0..h {
        for x in 0..w {
            if grid[y as usize][x as usize] == b'#' {
                let n = scan(&grid, (y, x), &vectors, false);
                if n > best {
                    best = n;
                    station = (y, x);
                }
            }
        }
    }
    println!("Part 1: {}", best);
    println!("Part 2: {}", scan(&grid, station, &vectors, true));
}
