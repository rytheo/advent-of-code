use std::{array, collections::HashMap, ops::Neg};

use aoc::{geometry::Vector, vecN};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Dir {
    North,
    East,
    South,
    West,
}

impl Dir {
    fn as_vector(&self) -> Vector<2> {
        use Dir::*;
        match self {
            North => vecN![-1, 0],
            East => vecN![0, 1],
            South => vecN![1, 0],
            West => vecN![0, -1],
        }
    }
}

impl Neg for Dir {
    type Output = Self;

    fn neg(self) -> Self::Output {
        use Dir::*;
        match self {
            North => South,
            South => North,
            East => West,
            West => East,
        }
    }
}

type Point = Vector<2>;
type Pipe = [Dir; 2];

enum Tile {
    Pipe(Pipe),
    Start,
    Ground,
}

fn parse_tile(c: char) -> Tile {
    use Dir::*;
    let pipe = match c {
        '|' => [North, South],
        '-' => [East, West],
        'L' => [North, East],
        'J' => [North, West],
        '7' => [South, West],
        'F' => [South, East],
        '.' => return Tile::Ground,
        'S' => return Tile::Start,
        _ => panic!("invalid char: {c}"),
    };
    Tile::Pipe(pipe)
}

/// Replaces the start point with a pipe segment and returns its location.
fn compute_start(grid: &mut HashMap<Point, Tile>) -> Point {
    let (&start, _) = grid
        .iter()
        .find(|(_, tile)| matches!(tile, Tile::Start))
        .unwrap();
    let all_dirs =
        [Dir::North, Dir::East, Dir::South, Dir::West].map(|side| (side, side.as_vector()));
    let mut dirs: Vec<_> = all_dirs
        .iter()
        .filter_map(|(side, delta)| match grid.get(&(start + *delta))? {
            Tile::Pipe([a, b]) if *a == -*side || *b == -*side => Some(side),
            _ => None,
        })
        .collect();
    dirs.sort();
    grid.insert(start, Tile::Pipe([*dirs[0], *dirs[1]]));
    start
}

/// Returns a hash map containing only the segments that loop from the start point.
fn extract_loop(grid: &HashMap<Point, Tile>, start: Point) -> HashMap<Point, Pipe> {
    let mut pipe_map = HashMap::new();
    let Tile::Pipe(pipe @ [mut exit, _]) = grid[&start] else {
        panic!("start should be a pipe")
    };
    pipe_map.insert(start, pipe);
    let mut here = start + exit.as_vector();
    while here != start {
        let Tile::Pipe(pipe @ [a, b]) = grid[&here] else {
            panic!("loop should be closed")
        };
        pipe_map.insert(here, pipe);
        let entry = -exit;
        exit = if a == entry { b } else { a };
        here += exit.as_vector();
    }
    pipe_map
}

/// Returns the number of tiles inside the loop.
fn count_inner_tiles(pipe_map: &HashMap<Point, Pipe>) -> usize {
    // Find the max coordinates
    let [max_y, max_x] = array::from_fn(|i| pipe_map.keys().map(|p| p[i]).max().unwrap());
    // Start scanning
    let mut count = 0;
    for y in 0..=max_y {
        let mut inside = false;
        let mut corner_dir = None;
        for x in 0..=max_x {
            if let Some(pipe) = pipe_map.get(&vecN![y, x]) {
                match pipe {
                    [Dir::East, Dir::West] => (),
                    [Dir::North, Dir::South] => inside = !inside,
                    [d @ (Dir::North | Dir::South), _] | [_, d @ (Dir::North | Dir::South)] => {
                        match corner_dir.take() {
                            Some(prev) if d != prev => inside = !inside,
                            Some(_) => (),
                            None => corner_dir = Some(d),
                        }
                    }
                    _ => panic!("unsupported pipe: {pipe:?}"),
                }
            } else if inside {
                count += 1;
            }
        }
    }
    count
}

fn main() {
    let input = aoc::read_input("2023/input_10.txt");
    let mut grid: HashMap<_, _> = input
        .lines()
        .enumerate()
        .flat_map(|(y, row)| {
            row.chars()
                .enumerate()
                .map(move |(x, c)| (vecN![y as i32, x as i32], parse_tile(c)))
        })
        .collect();
    let start = compute_start(&mut grid);
    let pipe_map = extract_loop(&grid, start);
    println!("Part 1: {}", pipe_map.len() / 2);
    println!("Part 2: {}", count_inner_tiles(&pipe_map));
}
