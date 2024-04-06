fn main() {
    let input = aoc::read_input("2022/input_01.txt");
    let mut counts: Vec<u32> = input
        .split("\n\n")
        .map(|block| block.lines().map(|s| s.parse::<u32>().unwrap()).sum())
        .collect();
    counts.sort_by_key(|&x| std::cmp::Reverse(x));
    println!("Part 1: {}", counts[0]);
    println!("Part 2: {}", counts[..3].iter().sum::<u32>());
}
