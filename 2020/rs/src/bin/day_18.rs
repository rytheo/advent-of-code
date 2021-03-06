use std::fs;
use Token::*;
use Operator::*;

enum Token {
    Num(u64),
    Op(Operator),
    LeftParen,
    RightParen,
}

enum Operator {
    Add,
    Mul,
}

fn eval(expr: &[Token], advanced: bool) -> u64 {
    // Shunting yard algorithm to convert infix to postfix
    let mut queue = Vec::with_capacity(expr.len());
    let mut stack = Vec::with_capacity(expr.len());
    for token in expr {
        match token {
            Num(_) => queue.push(token),
            LeftParen => stack.push(token),
            Op(op) => {
                // Pop operators of equal or higher precedence into the queue
                while let Some(Op(o2)) = stack.last() {
                    match (advanced, op, o2) {
                        (false, ..) | (true, Mul, Add) => queue.push(stack.pop().unwrap()),
                        _ => break,
                    }
                }
                stack.push(token);
            }
            // Pop tokens into the queue until the next left parenthesis
            RightParen => while let Some(t) = stack.pop() {
                match t {
                    LeftParen => break,
                    _ => queue.push(t),
                }
            }
        }
    }
    // Pop the rest of the stack into the queue
    queue.extend(stack.into_iter().rev());
    // Evaluate the postfix expression
    let mut stack = Vec::with_capacity(expr.len());
    for token in queue {
        match token {
            Num(x) => stack.push(*x),
            Op(op) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match op {
                    Add => stack.push(a + b),
                    Mul => stack.push(a * b),
                }
            }
            _ => unreachable!(),
        }
    }
    stack[0]
}

fn main() {
    let input = fs::read_to_string("../input/input_18.txt").unwrap();
    let exprs: Vec<Vec<_>> = input.split_terminator('\n').map(|s| {
        s.chars().filter_map(|c| match c {
            ' ' => None,
            '(' => Some(LeftParen),
            ')' => Some(RightParen),
            '+' => Some(Op(Add)),
            '*' => Some(Op(Mul)),
            _ => Some(Num(c.to_digit(10).unwrap() as u64)),
        }).collect()
    }).collect();
    println!("Part 1: {}", exprs.iter().map(|v| eval(v, false)).sum::<u64>());
    println!("Part 2: {}", exprs.iter().map(|v| eval(v, true)).sum::<u64>());
}
