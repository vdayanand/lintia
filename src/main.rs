use tree_sitter::{Parser, Language};

extern "C" { fn tree_sitter_julia() -> Language; }

fn main() {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let source_code = "f(x)=1";
    let tree = parser.parse(source_code, None).unwrap();
    let root_node = tree.root_node();
    assert_eq!(root_node.kind(), "source_file");
    assert_eq!(root_node.start_position().column, 0);
    assert_eq!(root_node.end_position().column, 6);
    println!("Hello, world!");
}
