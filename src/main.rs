use tree_sitter::{Language, Parser};

extern "C" {
    fn tree_sitter_julia() -> Language;
}

fn print_node(node: &tree_sitter::Node, source_code: &str, indent: usize) {
    let separator = "  ".repeat(indent);

    println!("{}- node: {}", separator, node.to_sexp());
    println!("{}  kind: {}", separator, node.kind());
    println!(
        "{}  start: {}, {}",
        separator,
        node.start_position().row,
        node.start_position().column
    );
    println!(
        "{}  end: {}, {}",
        separator,
        node.end_position().row,
        node.end_position().column
    );
    let chunk: String = source_code
        .chars()
        .skip(node.start_byte())
        .take(node.end_byte() - node.start_byte())
        .collect();
    println!("{}  content: {}", separator, chunk);
}

fn main() {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let source_code = "f(x)=1";
    let mut indent = 0;
    let tree = parser.parse(source_code, None).unwrap();
    let mut tree_cursor = tree.walk();
    let mut level = 0;
    let mut child_printed = false;

    loop {
        if !child_printed && tree_cursor.goto_first_child() {
            indent += 1;
            print_node(&tree_cursor.node(), &source_code, indent);
        } else if tree_cursor.goto_next_sibling() {
            child_printed = false;
            print_node(&tree_cursor.node(), &source_code, indent);
            if tree_cursor.goto_first_child() {
                indent += 1;
                level += 1;
                print_node(&tree_cursor.node(), &source_code, indent);
            }
        } else {
            if level > 0 {
                tree_cursor.goto_parent();
                child_printed = true;
                level -= 1;
                indent -= 1;
            } else {
                break;
            }
        }
    }
}
