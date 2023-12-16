use std::collections::HashSet;

use aoc::Part;

#[derive(Default)]
struct Pair {
    occupied: [usize; 2],
    empty: [usize; 2],
}

fn compute_length_sum(pairs: &[Pair], scale: usize) -> usize {
    pairs
        .iter()
        .map(|p| {
            p.occupied.iter().sum::<usize>() + p.empty.iter().map(|d| d * scale).sum::<usize>()
        })
        .sum()
}

fn main() {
    let input = aoc::read_input("2023/input_11.txt");
    let mut galaxies = vec![];
    let mut occupied = <[HashSet<usize>; 2]>::default();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                galaxies.push([y, x]);
                occupied[0].insert(y);
                occupied[1].insert(x);
            }
        }
    }
    let pairs: Vec<_> = galaxies
        .iter()
        .enumerate()
        .flat_map(|(n, [y1, x1])| {
            galaxies.iter().skip(n + 1).map(|[y2, x2]| {
                let mut pair = Pair::default();
                let ranges = [*y1..*y2, *x1.min(x2)..*x1.max(x2)];
                for (i, range) in ranges.into_iter().enumerate() {
                    for x in range {
                        match occupied[i].contains(&x) {
                            true => pair.occupied[i] += 1,
                            false => pair.empty[i] += 1,
                        }
                    }
                }
                pair
            })
        })
        .collect();
    for (part, scale) in [(Part::One, 1), (Part::Two, 1_000_000)] {
        println!("{part}: {}", compute_length_sum(&pairs, scale))
    }
}
