use std::collections::HashMap;
use std::fs;

fn valid(passport: &HashMap<&str, &str>, check_values: bool) -> bool {
    let fields_exist = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].iter().all(|k| passport.contains_key(k));
    if !fields_exist || !check_values {
        return fields_exist;
    }
    passport.iter().all(|(&k, &v)| match k {
        "byr" => v.parse().map(|n| 1920 <= n && n <= 2002).unwrap_or(false),
        "iyr" => v.parse().map(|n| 2010 <= n && n <= 2020).unwrap_or(false),
        "eyr" => v.parse().map(|n| 2020 <= n && n <= 2030).unwrap_or(false),
        "hgt" => v.len() > 2 && match &v[v.len()-2..] {
            "cm" => v[..v.len()-2].parse().map(|n| 150 <= n && n <= 194).unwrap_or(false),
            "in" => v[..v.len()-2].parse().map(|n| 59 <= n && n <= 77).unwrap_or(false),
            _ => false,
        },
        "hcl" => v.len() == 7 && &v[..1] == "#" && v.chars().skip(1).all(|c| c.is_digit(16)),
        "ecl" => ["amb", "blu", "brn", "gry", "grn", "grn", "hzl", "oth"].contains(&v),
        "pid" => v.len() == 9 && v.chars().all(char::is_numeric),
        _ => true,
    })
}

fn main() {
    let input = fs::read_to_string("../input/2020/input_04.txt").unwrap();
    let passports: Vec<HashMap<_, _>> = input.split("\n\n")
        .map(|s| s.split_whitespace().map(|t| (&t[..3], &t[4..])).collect())
        .collect();
    println!("Part 1: {}", passports.iter().filter(|p| valid(&p, false)).count());
    println!("Part 2: {}", passports.iter().filter(|p| valid(&p, true)).count());
}
