fn parse_nums(line: &str) -> impl Iterator<Item = u64> + '_ {
    line.split_whitespace().filter_map(|s| s.parse().ok())
}

fn parse_single_num(line: &str) -> u64 {
    line.chars()
        .filter(char::is_ascii_digit)
        .collect::<String>()
        .parse()
        .unwrap()
}

fn compute_dist(time_held: u64, total_time: u64) -> u64 {
    let time_left = total_time - time_held;
    time_held * time_left
}

fn count_wins(time: u64, best: u64) -> usize {
    (1..time)
        .filter(|time_held| compute_dist(*time_held, time) > best)
        .count()
}

fn main() {
    let input = aoc::read_input("2023/input_06.txt");
    let lines: Vec<_> = input.lines().collect();
    let times = parse_nums(lines[0]);
    let records = parse_nums(lines[1]);
    let part_1: usize = std::iter::zip(times, records)
        .map(|(time, best)| count_wins(time, best))
        .product();
    println!("Part 1: {part_1}");
    let real_time = parse_single_num(lines[0]);
    let real_record = parse_single_num(lines[1]);
    println!("Part 2: {}", count_wins(real_time, real_record));
}
