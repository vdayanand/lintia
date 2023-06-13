use colored::Colorize;
use tree_sitter::{Language, Node, Parser};

extern "C" {
    fn tree_sitter_julia() -> Language;
}
struct UndefVar {
    symbol: String,
    row: usize,
    column: usize
}

fn print_node(node: &tree_sitter::Node, src: &String) {
    println!("Kind: {}", node.kind().cyan());
    println!("has error: {}", node.has_error());
    println!("Child count: {}", node.named_child_count());
    let mut tc = node.walk();
    for (id, child) in node.named_children(&mut tc).enumerate() {
        println!("Child({}): Kind => {}", id, child.kind().yellow());
        println!(
            "Child({}): value => {}",
            id,
            node_value(&child, src).yellow()
        );
    }
}

fn row(node: &Node, src: &String) -> usize {
    let p = node.start_position();
    return p.row;
}
fn col(node: &Node, src: &String) -> usize {
    let p = node.start_position();
    return p.column;
}

fn node_value(node: &Node, src: &String) -> String {
    if let Ok(val) = node.utf8_text(src.as_bytes()) {
        return val.to_string();
    }
    return "".to_string();
}

fn analyse(node: &Node, src: &String, env: &Vec<String>) -> Option<UndefVar>{
    if let Ok(val) = node.utf8_text(src.as_bytes()) {
        let sym = val.to_string();
        if !env.contains(&sym.to_string()) {
            let r = row(&node, src);
            let c = col(&node, src);
            return Some(UndefVar {symbol: sym, row: r, column: c})
        }
        return None

    }
    return None
}
fn eval(node: &Node, src: &String, env: &Vec<String>) -> Vec<Option<UndefVar,>>{
    let mut result = Vec::<Option<UndefVar>>::new();
    match node.kind() {
        "source_file" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                result.extend(eval(&child, src, env));
            }
        }
        "assignment_expression" => {
            if let Some(rnode) = node.named_child(1) {
                result.extend(eval(&rnode, src, env));
            }
        }
        "return_statement" => {
            print_node(node, src);
            if let Some(rnode) = node.named_child(0) {
                if rnode.kind() == "identifier" {
                    result.push(analyse(&rnode, src, env))
                } else {
                    result.extend(eval(&rnode, src, env));
                }
            }
        }
        "subscript_expression" => {
            print_node(node, src);
            if let Some(rnode) = node.named_child(0) {
                if rnode.kind() == "identifier" {
                    result.push(analyse(&rnode, src, env))
                } else {
                    result.extend(eval(&rnode, src, env))
                }
            }
        }
        "parenthesized_expression" => {
            if let Some(rnode) = node.named_child(0) {
               result.extend(eval(&rnode, src, env));
            }
        }
        "number" => (),
        "binary_expression" => {
            if let Some(firstnode) = node.named_child(0) {
                if firstnode.kind() == "identifier" {
                    result.push(analyse(&firstnode, src, env));
                } else {
                    result.extend(eval(&firstnode, src, env));
                }
            }
            if let Some(secondnode) = node.named_child(1) {
                if secondnode.kind() == "identifier" {
                    result.push(analyse(&secondnode, src, env));
                } else {
                    result.extend(eval(&secondnode, src, env));
                }
            }
        },
        "if_statement" => {
            let mut tc = node.walk();
            let mut ifenv = Vec::<String>::new();
            ifenv.extend_from_slice(env);
            for child in node.named_children(&mut tc).skip(1)            {
                if child.kind() == "assignment_expression" {
                    if let Some(lhs) = child.named_child(0) {
                        if lhs.kind() == "identifier"{
                            ifenv.push(node_value(&lhs, src));
                        }
                    }
                    if let Some(rhs) = child.named_child(1) {
                         result.extend(eval(&child, src, &ifenv.clone()));
                    }

                }
                else {
                    result.extend(eval(&child, src, &ifenv.clone()));
                }
            }
        },
        _ => {
            println!("Unimplemented kind {}", node.kind());
        }
    };
    return result
}

fn lint(src: &str) -> Vec<Option<UndefVar>> {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let root_node = tree.root_node();
    let env = Vec::<String>::new();
    return eval(&root_node, &String::from(src), &env)
}

fn main() {
    let source_code = r#"
     if x
        y = 10
        x = z + 1
     end
     "#;
    let errs = lint(source_code);
    for err in errs {
        if let Some(undef) = err {
            println!(
                "Undefined symbol {} found at {}:{} ",
                undef.symbol.red(),
                undef.row,
                undef.column
            );
        }
    }
}
