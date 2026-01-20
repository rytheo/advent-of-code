use std::fs;

#[derive(Clone, Copy)]
struct Line {
    start: (i32, i32),
    end: (i32, i32),
    prev: i32,
}

struct Wire {
    horis: Vec<Line>,
    verts: Vec<Line>,
}

struct Intersect {
    x: i32,
    y: i32,
    v: Line,
    h: Line,
}

impl Intersect {
    fn dist(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }

    fn cost(&self) -> i32 {
        (self.x - self.h.start.0).abs()
            + self.h.prev
            + (self.y - self.v.start.1).abs()
            + self.v.prev
    }

    fn find(v: &Line, h: &Line) -> Option<Intersect> {
        let (x0, x1) = if h.start.0 < h.end.0 {
            (h.start.0, h.end.0)
        } else {
            (h.end.0, h.start.0)
        };
        let (y0, y1) = if v.start.1 < v.end.1 {
            (v.start.1, v.end.1)
        } else {
            (v.end.1, v.start.1)
        };
        match (x0..x1).contains(&v.start.0) && (y0..y1).contains(&h.start.1) {
            true => Some(Intersect {
                x: v.start.0,
                y: h.start.1,
                v: *v,
                h: *h,
            }),
            false => None,
        }
    }
}

fn main() {
    let text = fs::read_to_string("../input/2019/input_03.txt").unwrap();
    // Parse wire paths from input file
    let wires: Vec<_> = text
        .lines()
        .map(|line| {
            let mut wire = Wire {
                horis: vec![],
                verts: vec![],
            };
            let (mut x1, mut y1) = (0, 0);
            let mut prev = 0;
            for step in line.split(',') {
                let (x0, y0) = (x1, y1);
                let length: i32 = step[1..].parse().unwrap();
                match &step[..1] {
                    "U" => y1 -= length,
                    "R" => x1 += length,
                    "D" => y1 += length,
                    "L" => x1 -= length,
                    s => panic!("Unknown direction: {}", s),
                }
                let line = Line {
                    start: (x0, y0),
                    end: (x1, y1),
                    prev,
                };
                match &step[..1] {
                    "U" | "D" => wire.verts.push(line),
                    "L" | "R" => wire.horis.push(line),
                    _ => unreachable!(),
                }
                prev += length;
            }
            wire
        })
        .collect();
    // Find all wire intersections
    let mut intersections = vec![];
    for (a, b) in [(&wires[0], &wires[1]), (&wires[1], &wires[0])].iter() {
        for v in &a.verts {
            for h in &b.horis {
                if let Some(intersect) = Intersect::find(v, h) {
                    intersections.push(intersect);
                }
            }
        }
    }
    println!(
        "Part 1: {}",
        intersections.iter().map(|i| i.dist()).min().unwrap()
    );
    println!(
        "Part 2: {}",
        intersections.iter().map(|i| i.cost()).min().unwrap()
    );
}
