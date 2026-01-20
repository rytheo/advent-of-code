use std::fs;

fn main() {
    let input = fs::read_to_string("../input/2021/input_04.txt").unwrap();
    let (order, blocks) = input.split_once("\n\n").unwrap();
    let numbers: Vec<i32> = order.split(',').map(|s| s.parse().unwrap()).collect();
    // Parse boards
    let mut boards: Vec<_> = blocks
        .split("\n\n")
        .map(|s| {
            let mut board: [[i32; 5]; 5] = Default::default();
            for (y, row) in s.lines().enumerate() {
                for (x, token) in row.split_whitespace().enumerate() {
                    board[y][x] = token.parse().unwrap();
                }
            }
            board
        })
        .collect();
    // Run the game
    let mut scores = vec![];
    for num in numbers {
        for val in boards.iter_mut().flatten().flatten() {
            if *val == num {
                *val = -1;
            }
        }
        let (winners, rest): (Vec<_>, Vec<_>) = boards.into_iter().partition(|b| {
            (0..5).any(|y| b[y].iter().all(|&v| v == -1))
                || (0..5).any(|x| (0..5).all(|y| b[y][x] == -1))
        });
        boards = rest;
        scores.extend(
            winners
                .iter()
                .map(|b| num * b.iter().flatten().filter(|&&v| v >= 0).sum::<i32>()),
        );
    }
    println!("Part 1: {}", scores[0]);
    println!("Part 2: {}", scores[scores.len() - 1]);
}
