use std::fs;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;
const LSIZE: usize = WIDTH * HEIGHT;

fn main() {
    let text = fs::read_to_string("../input/input_08.txt").unwrap();
    let data = text.trim().as_bytes();
    let layers: Vec<_> = (0..data.len()).step_by(LSIZE)
        .map(|i| &data[i..i+LSIZE])
        .collect();
    let min_layer = layers.iter().min_by_key(|layer| layer.iter().filter(|&&b| b == b'0').count()).unwrap();
    let prod: usize = (b'1'..=b'2').map(|t| min_layer.iter().filter(|&&b| b == t).count()).product();
    println!("Part 1: {}", prod);
    println!("Part 2:");
    for row in 0..HEIGHT {
        for col in 0..WIDTH {
            for layer in &layers {
                match layer[row * WIDTH + col] {
                    b'0' => print!(" "),
                    b'1' => print!("#"),
                    _ => continue,
                }
                break;
            }
        }
        println!();
    }
}
