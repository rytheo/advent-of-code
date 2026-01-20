use regex::Regex;
use std::collections::HashMap;
use std::fs;

fn expand<'c, 'a>(
    cache: &'c mut HashMap<&'a str, String>,
    patterns: &HashMap<&str, &'a str>,
    key: &'a str,
) -> &'c str {
    if !cache.contains_key(key) {
        let expr: String = patterns[key]
            .split(' ')
            .map(|t| {
                if t.starts_with('"') {
                    // Extract letter from string
                    String::from(&t[1..2])
                } else if t.parse::<u8>().is_ok() {
                    // Recurse and expand intermediate patterns
                    format!("({})", expand(cache, patterns, t))
                } else {
                    String::from(t)
                }
            })
            .collect();
        cache.insert(key, expr);
    }
    &cache[key]
}

fn count_matches(patterns: &HashMap<&str, &str>, messages: &[&str]) -> usize {
    // Cache intermediate expansions to save time
    let mut cache = HashMap::new();
    let expr = expand(&mut cache, patterns, "0");
    let full_expr = format!("^({})$", expr);
    let re = Regex::new(&full_expr).unwrap();
    messages.iter().filter(|s| re.is_match(s)).count()
}

fn main() {
    let input = fs::read_to_string("../input/2020/input_19.txt").unwrap();
    let blocks: Vec<_> = input.split("\n\n").collect();
    let mut patterns: HashMap<_, _> = blocks[0]
        .lines()
        .map(|s| {
            let mut kv = s.split(": ");
            (kv.next().unwrap(), kv.next().unwrap())
        })
        .collect();
    let messages: Vec<_> = blocks[1].lines().collect();
    println!("Part 1: {}", count_matches(&patterns, &messages));
    // Make pattern replacements for part 2
    patterns.insert("8", "42 +");
    // Use a regular version of non-regular rule 11
    let parts: Vec<_> = (1..5)
        .map(|n| format!("42 {{{}}} 31 {{{}}}", n, n))
        .collect();
    let nonreg = parts.join(" | ");
    patterns.insert("11", &nonreg);
    println!("Part 2: {}", count_matches(&patterns, &messages));
}
