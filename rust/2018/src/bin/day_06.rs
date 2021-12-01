use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fs;

fn dist(a: (i32, i32), b: (i32, i32)) -> u32 {
    (a.0 - b.0).unsigned_abs() + (a.1 - b.1).unsigned_abs()
}

fn main() {
    let text = fs::read_to_string("../input/2018/input_06.txt").unwrap();
    // Collect points and save min/max coords
    let mut areas: HashMap<(i32, i32), u32> = HashMap::new();
    let (mut x0, mut y0, mut x1, mut y1) = (i32::MAX, i32::MAX, 0, 0);
    for line in text.lines() {
        let (sx, sy) = line.split_once(", ").unwrap();
        let (x, y) = (sx.parse().unwrap(), sy.parse().unwrap());
        x0 = x0.min(x);
        y0 = y0.min(y);
        x1 = x1.max(x);
        y1 = y1.max(y);
        areas.insert((x, y), 0);
    }
    // Calculate areas and track points nearest to edges
    let mut inf_points = HashSet::new();
    for ex in x0..=x1 {
        for ey in y0..=y1 {
            // Find the target nearest to this point
            // If there are multiple nearest targets, skip all of them
            let mut nearest = vec![];
            let mut min_dist = u32::MAX;
            for &p in areas.keys() {
                let d = dist((ex, ey), p);
                match d.cmp(&min_dist) {
                    Ordering::Less => {
                        min_dist = d;
                        nearest.clear();
                        nearest.push(p);
                    },
                    Ordering::Equal => nearest.push(p),
                    Ordering::Greater => (),
                }
            }
            // Increment the area of the single nearest target
            if nearest.len() == 1 {
                *areas.get_mut(&nearest[0]).unwrap() += 1;
                // If this point is an edge, the target has infinite area
                if ex == x0 || ex == x1 || ey == y0 || ey == y1 {
                    inf_points.insert(nearest[0]);
                }
            }
        }
    }
    let area = areas.iter().filter_map(|(p, &area)| {
        if inf_points.contains(p) { None } else { Some(area) }
    }).max().unwrap();
    println!("Part 1: {}", area);

    let mut region_size = 0u32;
    // Do a breadth-first search outward from the median
    let (mut x, mut y) = ((x0 + x1) / 2, (y0 + y1) / 2);
    let mut edge_len = 0u32;
    // Loop until a full spiral layer contains no new points
    let mut finding = true;
    while finding {
        finding = false;
        // Do the next spiral layer
        for (dx, dy) in [(1, 0), (0, 1), (-1, 0), (0, -1)] {
            // Increase edge length every two edges
            if dy == 0 {
                edge_len += 1;
            }
            // Walk along one edge
            for _ in 0..edge_len {
                // Compute total distance
                let total: u32 = areas.keys().map(|&p| dist((x, y), p)).sum();
                if total < 10_000 {
                    region_size += 1;
                    finding = true;
                }
                // Move forward one space
                x += dx;
                y += dy;
            }
        }
    }
    println!("Part 2: {}", region_size);
}
