use std::fs;
use std::num::ParseIntError;

enum PacketData {
    Literal(u64),
    Operator {
        type_id: u8,
        packets: Vec<Packet>,
    }
}

struct Packet {
    bit_count: usize,
    version: u8,
    data: PacketData,
}

fn eval(packet: &Packet) -> u64 {
    match &packet.data {
        PacketData::Literal(val) => *val,
        PacketData::Operator { type_id, packets } => {
            let vals: Vec<_> = packets.iter().map(eval).collect();
            match type_id {
                0 => vals.into_iter().sum(),
                1 => vals.into_iter().product(),
                2 => vals.into_iter().min().unwrap(),
                3 => vals.into_iter().max().unwrap(),
                5 => (vals[0] > vals[1]) as u64,
                6 => (vals[0] < vals[1]) as u64,
                7 => (vals[0] == vals[1]) as u64,
                _ => panic!("Invalid type ID"),
            }
        }
    }
}

fn version_sum(packet: &Packet) -> u32 {
    let mut sum = packet.version as u32;
    if let PacketData::Operator { packets, .. } = &packet.data {
        sum += packets.iter().map(|p| version_sum(p)).sum::<u32>();
    }
    sum
}

fn parse_packet(bits: &str) -> Result<Packet, ParseIntError> {
    let version = u8::from_str_radix(&bits[..3], 2)?;
    let type_id = u8::from_str_radix(&bits[3..6], 2)?;
    let mut i = 6;
    let data = if type_id == 4 { // Literal
        let mut literal_bits = String::new();
        // Consume 5-bit chunks while the chunk prefix is 1
        // Include the last chunk whose prefix is 0
        let mut prefix = "1";
        while prefix == "1" {
            prefix = &bits[i..i+1];
            literal_bits.push_str(&bits[i+1..i+5]);
            i += 5;
        }
        PacketData::Literal(u64::from_str_radix(&literal_bits, 2)?)
    } else { // Operator
        let mut packets = vec![];
        let length_type_id = &bits[i..i+1];
        i += 1;
        let next_n = if length_type_id == "0" { 15 } else { 11 };
        let val = usize::from_str_radix(&bits[i..i+next_n], 2)?;
        i += next_n;
        // Parse packets until the limit is reached
        let mut subpacket_bits = 0;
        while if length_type_id == "0" { subpacket_bits < val } else { packets.len() < val } {
            let packet = parse_packet(&bits[i..])?;
            i += packet.bit_count;
            subpacket_bits += packet.bit_count;
            packets.push(packet);
        }
        PacketData::Operator { type_id, packets }
    };
    Ok(Packet { bit_count: i, version, data })
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_16.txt").unwrap();
    let bits: String = input.chars()
        .filter_map(|c| c.to_digit(16).map(|x| format!("{:04b}", x)))
        .collect();
    let packet = parse_packet(&bits).unwrap();
    println!("Part 1: {}", version_sum(&packet));
    println!("Part 2: {}", eval(&packet));
}
