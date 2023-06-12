use std::path::PathBuf;

fn main() {

    let dir: PathBuf = ["tree-sitter-julia", "src"].iter().collect();
    let mut b = cc::Build::new();
    b.warnings(false);
    b.include(&dir)
    .file(dir.join("parser.c"))
    .file(dir.join("scanner.c"))
    .compile("tree-sitter-julia");

    println!("cargo:rerun-if-changed=build.rs");
}
