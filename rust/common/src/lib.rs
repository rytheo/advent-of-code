use std::path::{Path, PathBuf};

pub mod counter;
pub mod geometry;

pub fn read_input(path: &str) -> String {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let root = manifest_dir.ancestors().nth(2).unwrap();
    let input_path: PathBuf = [root, Path::new("input"), Path::new(path)].into_iter().collect();
    std::fs::read_to_string(input_path).unwrap()
}
