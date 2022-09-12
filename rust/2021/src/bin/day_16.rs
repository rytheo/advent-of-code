use std::fs;
use std::num::ParseIntError;
use std::iter;

struct PacketMeta {
    version_sum: u32,
    bit_length: usize,
    value: u64,
}

struct Cursor<'a> {
    slice: &'a str,
    bits_read: usize,
}

impl<'a> Cursor<'a> {
    fn new(s: &'a str) -> Self {
        Self { slice: s, bits_read: 0 }
    }

    fn read(&mut self, n: usize) -> &'a str {
        let (taken, rest) = self.slice.split_at(n);
        self.slice = rest;
        self.bits_read += n;
        taken
    }
}

fn parse_packet(bits: &str) -> Result<PacketMeta, ParseIntError> {
    let mut cur = Cursor::new(bits);
    let mut version_sum = u32::from_str_radix(cur.read(3), 2)?;
    let type_id = u8::from_str_radix(cur.read(3), 2)?;
    let value = if type_id == 4 {
        let mut value = 0;
        let mut prefix = "1";
        while prefix == "1" {
            prefix = cur.read(1);
            value = (value << 4) + u64::from_str_radix(cur.read(4), 2)?;
        }
        value
    } else {
        let length_type_id = cur.read(1);
        let next_n = if length_type_id == "0" { 15 } else { 11 };
        let val = usize::from_str_radix(cur.read(next_n), 2)?;
        let mut subpacket_bits = 0;
        let mut subpacket_count = 0;
        let mut vals = iter::from_fn(|| {
            let tracker = if length_type_id == "0" { subpacket_bits } else { subpacket_count };
            if tracker >= val {
                return None;
            }
            let meta = parse_packet(cur.slice).unwrap();
            version_sum += meta.version_sum;
            cur.read(meta.bit_length);
            subpacket_bits += meta.bit_length;
            subpacket_count += 1;
            Some(meta.value)
        });
        match type_id {
            0 => vals.sum(),
            1 => vals.product(),
            2 => vals.min().unwrap(),
            3 => vals.max().unwrap(),
            5 => (vals.next().unwrap() > vals.next().unwrap()) as u64,
            6 => (vals.next().unwrap() < vals.next().unwrap()) as u64,
            7 => (vals.next().unwrap() == vals.next().unwrap()) as u64,
            _ => panic!("Invalid type ID"),
        }
    };
    Ok(PacketMeta {
        version_sum,
        bit_length: cur.bits_read,
        value,
    })
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_16.txt").unwrap();
    let bits: String = input.chars()
        .filter_map(|c| c.to_digit(16).map(|x| format!("{:04b}", x)))
        .collect();
    let meta = parse_packet(&bits).unwrap();
    println!("Part 1: {}", meta.version_sum);
    println!("Part 2: {}", meta.value);
}
