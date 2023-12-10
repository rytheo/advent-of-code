use std::fmt;
use std::path::{Path, PathBuf};

pub mod counter;
pub mod geometry;

#[derive(Clone, Copy, Debug)]
pub enum Part {
    One = 1,
    Two = 2,
}

impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Part {}", *self as u8)
    }
}

pub fn read_input(path: &str) -> String {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let root = manifest_dir.ancestors().nth(2).unwrap();
    let input_path: PathBuf = [root, Path::new("input"), Path::new(path)].into_iter().collect();
    std::fs::read_to_string(input_path).unwrap()
}
