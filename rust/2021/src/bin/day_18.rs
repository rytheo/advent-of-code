use std::fs;
use std::iter::Peekable;
use std::ops::Add;
use std::str::Chars;

use Element::{Number, Pair};
use Direction::{Left, Right};

enum Direction {
    Left,
    Right,
}

#[derive(Clone)]
enum Element {
    Number(u8),
    Pair(Box<Element>, Box<Element>),
}

impl Element {
    fn magnitude(&self) -> u32 {
        match self {
            Number(v) => *v as u32,
            Pair(a, b) => 3 * a.magnitude() + 2 * b.magnitude(),
        }
    }

    fn insert(&mut self, dir: Direction, val: &mut Option<u8>) -> bool {
        match self {
            Number(n) => if let Some(v) = val.take() {
                *n += v;
                true
            } else { false }
            Pair(a, b) => match dir {
                Left => a.insert(Left, val) || b.insert(Left, val),
                Right => b.insert(Right, val) || a.insert(Right, val),
            }
        }
    }

    fn explode(&mut self, depth: usize) -> (bool, Option<u8>, Option<u8>) {
        match self {
            Pair(a, b) if depth == 4 => {
                let (x, y) = match (a.as_ref(), b.as_ref()) {
                    (Number(x), Number(y)) => (*x, *y),
                    _ => panic!("Exploding pair must be two numbers"),
                };
                *self = Number(0);
                (true, Some(x), Some(y))
            }
            Pair(a, b) => {
                if let (true, x, mut y) = a.explode(depth + 1) {
                    b.insert(Left, &mut y);
                    (true, x, y)
                } else if let (true, mut x, y) = b.explode(depth + 1) {
                    a.insert(Right, &mut x);
                    (true, x, y)
                } else {
                    (false, None, None)
                }
            }
            Number(_) => (false, None, None),
        }
    }

    fn split(&mut self) -> bool {
        match self {
            Number(v) if *v > 9 => {
                let (a, b) = (*v / 2, *v / 2 + *v % 2);
                *self = Pair(Box::new(Number(a)), Box::new(Number(b)));
                true
            }
            Pair(a, b) => a.split() || b.split(),
            _ => false,
        }
    }
}

impl Add for Element {
    type Output = Element;

    fn add(self, rhs: Self) -> Element {
        let mut element = Pair(Box::new(self), Box::new(rhs));
        while element.explode(0).0 || element.split() { /* repeat */ }
        element
    }
}

fn parse_element(chars: &mut Peekable<Chars>) -> Element {
    match chars.next() {
        Some('[') => {
            let a = parse_element(chars);
            chars.next(); // Discard ','
            let b = parse_element(chars);
            chars.next(); // Discard ']'
            Pair(Box::new(a), Box::new(b))
        }
        Some(c) => {
            let mut digits = String::from(c);
            while chars.peek().unwrap().is_ascii_digit() {
                digits.push(chars.next().unwrap());
            }
            Number(digits.parse().unwrap())
        }
        _ => panic!("Empty iterator")
    }
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_18.txt").unwrap();
    let elements: Vec<_> = input.lines()
        .map(|line| parse_element(&mut line.chars().peekable()))
        .collect();
    let final_sum = elements.iter().cloned().reduce(|a, b| a + b).unwrap();
    println!("Part 1: {}", final_sum.magnitude());
    let mut largest = 0;
    for (i, a) in elements.iter().enumerate() {
        for (j, b) in elements.iter().enumerate() {
            if i != j {
                largest = largest.max((a.clone() + b.clone()).magnitude());
            }
        }
    }
    println!("Part 2: {}", largest);
}
