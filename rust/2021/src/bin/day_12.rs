use std::collections::{HashMap, HashSet};
use std::fs;

type Graph<'a> = HashMap<&'a str, Vec<&'a str>>;

fn explore<'a>(
    graph: &Graph<'a>,
    visited: &mut HashSet<&'a str>,
    current: &'a str,
    extra_time: bool,
) -> usize {
    if current == "end" {
        return 1;
    }
    let mut paths = 0;
    for adj in &graph[current] {
        if adj.bytes().all(|b| b.is_ascii_uppercase()) || !visited.contains(adj) {
            visited.insert(adj);
            paths += explore(graph, visited, adj, extra_time);
            visited.remove(adj);
        } else if extra_time && *adj != "start" {
            paths += explore(graph, visited, adj, false);
        }
    }
    paths
}

fn main() {
    let input = fs::read_to_string("../input/2021/input_12.txt").unwrap();
    let mut graph: Graph = HashMap::new();
    for line in input.lines() {
        let (a, b) = line.split_once('-').unwrap();
        for (start, end) in [(a, b), (b, a)] {
            graph.entry(start).or_default().push(end);
        }
    }
    let mut visited = HashSet::from(["start"]);
    println!("Part 1: {}", explore(&graph, &mut visited, "start", false));
    println!("Part 2: {}", explore(&graph, &mut visited, "start", true));
}
