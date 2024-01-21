fn hash(s: &str) -> usize {
    s.bytes().fold(0, |acc, b| (acc + b as usize) * 17 % 256)
}

struct Lens<'a> {
    label: &'a str,
    length: u8,
}

struct Map<'a> {
    bins: [Vec<Lens<'a>>; 256],
}

impl<'a> Map<'a> {
    fn new() -> Self {
        Self {
            bins: std::array::from_fn(|_| Vec::new()),
        }
    }

    fn insert(&mut self, lens: Lens<'a>) {
        let bin = &mut self.bins[hash(lens.label)];
        match bin.iter_mut().find(|v| v.label == lens.label) {
            Some(entry) => entry.length = lens.length,
            None => bin.push(lens),
        }
    }

    fn remove(&mut self, label: &str) {
        self.bins[hash(label)].retain(|v| v.label != label);
    }
}

fn main() {
    let input = aoc::read_input("2023/input_15.txt");
    let checksum: usize = input.trim().split(',').map(hash).sum();
    println!("Part 1: {checksum}");
    let mut map = Map::new();
    for step in input.trim().split(',') {
        let (label, length) = step.split_once(['-', '=']).unwrap();
        match length.parse::<u8>() {
            Ok(length) => map.insert(Lens { label, length }),
            Err(_) => map.remove(label),
        }
    }
    let power_sum = map
        .bins
        .iter()
        .enumerate()
        .flat_map(|(num, bin)| {
            bin
                .iter()
                .enumerate()
                .map(move |(slot, lens)| (1 + num) * (1 + slot) * lens.length as usize)
        })
        .sum::<usize>();
    println!("Part 2: {power_sum}");
}
