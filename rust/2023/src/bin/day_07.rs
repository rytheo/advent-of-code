use aoc::{counter::Counter, Part};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Card {
    Joker,
    Num(u8),
    Jack,
    Queen,
    King,
    Ace,
}

type Hand = [Card; 5];

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeKind,
    FullHouse,
    FourKind,
    FiveKind,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct HandData {
    ty: HandType,
    hand: Hand,
    bid: u32,
}

fn parse_card(c: char, part: Part) -> Card {
    use Card::*;
    match c {
        '2'..='9' => Num(c.to_digit(10).unwrap() as u8),
        'T' => Num(10),
        'J' => match part {
            Part::One => Jack,
            Part::Two => Joker,
        },
        'Q' => Queen,
        'K' => King,
        'A' => Ace,
        _ => panic!("Invalid character: {c:?}"),
    }
}

fn parse_hand(s: &str, part: Part) -> Hand {
    let mut card_iter = s.chars().map(|c| parse_card(c, part));
    std::array::from_fn(|_| card_iter.next().unwrap())
}

fn compute_hand_type(hand: &Hand) -> HandType {
    use HandType::*;
    let card_counter: Counter<Card> = hand.iter().copied().collect();
    // Initially exclude Jokers from the descending counts
    let mut counts: Vec<_> = card_counter
        .iter()
        .filter_map(|(&card, &count)| (card != Card::Joker).then_some(count))
        .collect();
    counts.sort_by(|a, b| b.cmp(a));
    let num_jokers = card_counter[&Card::Joker];
    if counts.is_empty() {
        counts.push(num_jokers);
    } else {
        counts[0] += num_jokers;
    }
    match counts.as_slice() {
        [5] => FiveKind,
        [4, 1] => FourKind,
        [3, 2] => FullHouse,
        [3, 1, 1] => ThreeKind,
        [2, 2, 1] => TwoPair,
        [2, 1, 1, 1] => OnePair,
        [1, 1, 1, 1, 1] => HighCard,
        _ => unreachable!(),
    }
}

fn parse_hand_data(s: &str, part: Part) -> HandData {
    let (cards, bid) = s.split_once(' ').unwrap();
    let hand = parse_hand(cards, part);
    HandData {
        ty: compute_hand_type(&hand),
        hand,
        bid: bid.parse().unwrap(),
    }
}

fn main() {
    let input = aoc::read_input("2023/input_07.txt");
    for part in [Part::One, Part::Two] {
        let mut data: Vec<_> = input
            .lines()
            .map(|line| parse_hand_data(line, part))
            .collect();
        data.sort();
        let total = std::iter::zip(1.., data)
            .map(|(rank, data)| rank * data.bid)
            .sum::<u32>();
        println!("{part}: {total}");
    }
}
