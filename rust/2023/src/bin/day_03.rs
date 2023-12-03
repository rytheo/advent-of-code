use std::collections::{HashMap, HashSet};

use regex::Regex;

type Point = [i32; 2];

fn find_nearby_symbols(
    symbol_points: &HashSet<Point>,
    y: i32,
    start: i32,
    end: i32,
) -> impl Iterator<Item = Point> + '_ {
    let above = ((start - 1)..=end).map(move |x| [y - 1, x]);
    let middle = [[y, start - 1], [y, end]];
    let below = ((start - 1)..=end).map(move |x| [y + 1, x]);
    above
        .chain(middle)
        .chain(below)
        .filter(|p| symbol_points.contains(p))
}

fn main() {
    let input = aoc::read_input("2023/input_03.txt");
    // Store the location of each symbol
    let mut symbol_points = HashSet::new();
    // Keep track of numbers adjacent to asterisks
    let mut gear_map = HashMap::<Point, Vec<u32>>::new();
    let re = Regex::new(r"[^.0-9]").unwrap();
    for (y, line) in input.lines().enumerate() {
        for m in re.find_iter(line) {
            let point = [y as i32, m.start() as i32];
            symbol_points.insert(point);
            if m.as_str() == "*" {
                gear_map.insert(point, vec![]);
            }
        }
    }
    // Search for part numbers
    let mut part_sum = 0;
    let re = Regex::new(r"\d+").unwrap();
    for (y, line) in input.lines().enumerate() {
        for m in re.find_iter(line) {
            let number = m.as_str().parse::<u32>().unwrap();
            let mut points =
                find_nearby_symbols(&symbol_points, y as i32, m.start() as i32, m.end() as i32)
                    .peekable();
            // Add to the part sum if the number is next to a symbol
            if points.peek().is_some() {
                part_sum += number;
            }
            // Add to each adjacent gear's number list
            for point in points {
                if let Some(numbers) = gear_map.get_mut(&point) {
                    numbers.push(number);
                }
            }
        }
    }
    println!("Part 1: {part_sum}");
    let gear_ratio_sum: u32 = gear_map
        .values()
        .filter_map(|nums| (nums.len() == 2).then(|| nums.iter().product::<u32>()))
        .sum();
    println!("Part 2: {gear_ratio_sum}");
}
