use std::ops::Range;

#[derive(Debug)]
struct RangeMap {
    src: Range<u64>,
    dst_start: u64,
}

impl RangeMap {
    fn get(&self, key: u64) -> Option<u64> {
        self.src
            .contains(&key)
            .then_some(key - self.src.start + self.dst_start)
    }
}

#[derive(Debug)]
struct CatMap(Vec<RangeMap>);

impl CatMap {
    fn get(&self, key: u64) -> u64 {
        self.0.iter().find_map(|map| map.get(key)).unwrap_or(key)
    }

    fn expand_range(&self, mut range: Range<u64>) -> Vec<Range<u64>> {
        // The input range will be split based on this map
        let mut output = vec![];
        // The maps have been pre-sorted by source start
        for map in &self.0 {
            // First check if the range starts before the map
            if range.start < map.src.start {
                // The part before the map just maps to itself
                let split = range.end.min(map.src.start);
                output.push(range.start..split);
                range.start = split;
                if range.start >= range.end {
                    break;
                }
            }
            // Now check if the range overlaps with the map
            if range.start < map.src.end {
                let split = range.end.min(map.src.end);
                let to_convert = range.start..split;
                let converted = (to_convert.start - map.src.start + map.dst_start)
                    ..(to_convert.end - map.src.start + map.dst_start);
                output.push(converted);
                range.start = split;
                if range.start >= range.end {
                    break;
                }
            }
        }
        // Add whatever is leftover
        if range.start < range.end {
            output.push(range);
        }
        output
    }
}

fn parse_seeds(line: &str) -> Vec<u64> {
    line.split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect()
}

fn parse_map(line: &str) -> RangeMap {
    let mut vals = line.split_whitespace().map(|s| s.parse().unwrap());
    let dest_start = vals.next().unwrap();
    let src_start = vals.next().unwrap();
    let length = vals.next().unwrap();
    RangeMap {
        src: src_start..(src_start + length),
        dst_start: dest_start,
    }
}

fn parse_cat_map(block: &str) -> CatMap {
    let mut maps: Vec<_> = block.lines().skip(1).map(parse_map).collect();
    maps.sort_by_key(|m| m.src.start);
    CatMap(maps)
}

fn find_location(maps: &[CatMap], seed: u64) -> u64 {
    let mut value = seed;
    for map in maps {
        value = map.get(value);
    }
    value
}

fn find_location_ranges(maps: &[CatMap], seed_ranges: Vec<Range<u64>>) -> Vec<Range<u64>> {
    let mut ranges = seed_ranges;
    for map in maps {
        ranges = ranges
            .into_iter()
            .flat_map(|r| map.expand_range(r))
            .collect();
    }
    ranges
}

fn main() {
    let input = aoc::read_input("2023/input_05.txt");
    let mut blocks = input.split("\n\n");
    let seed_line = blocks.next().unwrap();
    let seed_nums = parse_seeds(seed_line);
    let maps: Vec<_> = blocks.map(parse_cat_map).collect();
    let part_1 = seed_nums
        .iter()
        .map(|seed| find_location(&maps, *seed))
        .min()
        .unwrap();
    println!("Part 1: {part_1}");
    let seed_ranges: Vec<_> = seed_nums
        .chunks_exact(2)
        .map(|chunk| chunk[0]..(chunk[0] + chunk[1]))
        .collect();
    let part_2 = find_location_ranges(&maps, seed_ranges)
        .iter()
        .map(|r| r.start)
        .min()
        .unwrap();
    println!("Part 2: {part_2}");
}
