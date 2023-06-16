use colored::Colorize;
use tree_sitter::{Language, Node, Parser};

extern "C" {
    fn tree_sitter_julia() -> Language;
}
#[derive(Debug)]
struct UndefVar {
    symbol: String,
    row: usize,
    column: usize,
}

fn print_node(node: &tree_sitter::Node, src: &String) {
    println!("Kind: {}", node.kind().cyan());
    println!("has error: {}", node.has_error());
    let mut tc = node.walk();
    for (id, child) in node.named_children(&mut tc).enumerate() {
        println!(
            "Child({}): {}::{}",
            id,
            node_value(&child, src).yellow(),
            child.kind().yellow()
        );
    }
}

fn row(node: &Node) -> usize {
    let p = node.start_position();
    return p.row;
}
fn col(node: &Node) -> usize {
    let p = node.start_position();
    return p.column;
}

fn node_value(node: &Node, src: &String) -> String {
    if let Ok(val) = node.utf8_text(src.as_bytes()) {
        return val.to_string();
    }
    return "".to_string();
}

fn analyse(node: &Node, src: &String, env: &Vec<String>) -> Option<UndefVar> {
    if let Ok(val) = node.utf8_text(src.as_bytes()) {
        let sym = val.to_string();
        if !env.contains(&sym.to_string()) {
            let r = row(&node);
            let c = col(&node);
            return Some(UndefVar {
                symbol: sym,
                row: r,
                column: c,
            });
        }
        return None;
    }
    return None;
}
fn eval_mut_env(node: &Node, src: &String, env: &Vec<String>, result: &mut Vec<UndefVar>) {
    let mut tc = node.walk();
    let mut newenv = Vec::<String>::new();
    newenv.extend_from_slice(env);
    for child in node.named_children(&mut tc) {
        match child.kind() {
            "assignment_expression"
            | "variable_declaration"
            | "for_binding"
            | "const statement" => {
                if let Some(lhs) = child.named_child(0) {
                    if lhs.kind() == "identifier" {
                        newenv.push(node_value(&lhs, src));
                    }
                    if lhs.kind() == "tuple_expression" {
                        let mut tc = lhs.walk();
                        for param in lhs.named_children(&mut tc) {
                            newenv.push(node_value(&param, src));
                        }
                    }
                }
            }
            "const_statement" => {
                if let Some(vardec) = child.named_child(0) {
                    if let Some(lhs) = vardec.named_child(0) {
                        newenv.push(node_value(&lhs, src))
                    }
                }
            }
            _ => (),
        };
        result.extend(eval(&child, src, &newenv));
    }
}

fn eval(node: &Node, src: &String, env: &Vec<String>) -> Vec<UndefVar> {
    let mut result = Vec::<UndefVar>::new();
    match node.kind() {
        "ternary_expression"
        | "tuple_expression"
        | "call_expression"
        | "broadcast_call_expression"
        | "spread_expression"
        | "array_expression"
        | "pair_expression"
        | "range_expression"
        | "command_string" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                result.extend(eval(&child, src, env));
            }
        }
        "named_field" => {
            if let Some(rnode) = node.named_child(1) {
                result.extend(eval(&rnode, src, env));
            }
        }
        "const_statement" => {
            if let Some(rnode) = node.named_child(1) {
                result.extend(eval(&rnode, src, env));
            }
        }
        "return_statement" => {
            if let Some(rnode) = node.named_child(0) {
                if rnode.kind() == "identifier" {
                    if let Some(failed) = analyse(&rnode, src, env) {
                        result.push(failed);
                    }
                } else {
                    result.extend(eval(&rnode, src, env));
                }
            }
        }
        "subscript_expression" => {
            if let Some(rnode) = node.named_child(0) {
                if rnode.kind() == "identifier" {
                    if let Some(failed) = analyse(&rnode, src, env) {
                        result.push(failed);
                    }
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
        "source_file" | "let_statement" | "if_statement" | "for_statement" | "while_statement"
        | "argument_list" => eval_mut_env(node, src, env, &mut result),

        "number" | "comment" | "continue_statement" | "break_statement" | "quote_expression"
        | "string" => (),

        "identifier" => {
            if let Some(failed) = analyse(&node, src, env) {
                result.push(failed);
            }
        }

        "assignment_expression" | "variable_declaration" | "for_binding" => {
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, env))
            }
        }

        "binary_expression" => {
            if let Some(firstnode) = node.named_child(0) {
                if firstnode.kind() == "identifier" {
                    if let Some(failed) = analyse(&firstnode, src, env) {
                        result.push(failed);
                    }
                } else {
                    result.extend(eval(&firstnode, src, env));
                }
            }
            if let Some(secondnode) = node.named_child(1) {
                if secondnode.kind() == "identifier" {
                    if let Some(failed) = analyse(&secondnode, src, env) {
                        result.push(failed);
                    }
                } else {
                    result.extend(eval(&secondnode, src, env));
                }
            }
        }
        _ => {
            print_node(node, src);
            println!("Unimplemented kind {}", node.kind());
        }
    }
    return result;
}

fn lint(src: &str, env: &Vec<String>) -> Vec<UndefVar> {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let root_node = tree.root_node();
    return eval(&root_node, &String::from(src), &env);
}

fn main() {
    let source_code = r#"
      ## comment
      const x = 111
      y = m
      for i in 1:100
      end
     "#;
    let env = Vec::<String>::new();
    let errs = lint(source_code, &env);
    for err in errs {
        println!(
            "Undefined symbol {} found at {}:{} ",
            err.symbol.red(),
            err.row,
            err.column
        );
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_if() {
        let source_code = r#"
         if x
            y = 10
            x = z + 1
            (i, j) = (1, 2)
            x = x + 10
            m
         end
         "#;
        let env = vec!["x".to_string()];
        let mut errs = lint(&source_code, &env);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 3);
        assert_eq!(one.column, 16);

        let two = errs.remove(0);
        assert_eq!(two.symbol, "m".to_string());
        assert_eq!(two.row, 6);
        assert_eq!(two.column, 12);
    }
    #[test]
    fn test_let() {
        let source_code = r#"
         let x
            x
            z
         end
         "#;
        let env = vec!["x".to_string()];
        let mut errs = lint(&source_code, &env);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 3);
        assert_eq!(one.column, 12);
    }
    #[test]
    fn test_callexp() {
        let source_code = r#"
         z(x)
         "#;
        let env = vec!["x".to_string()];
        let mut errs = lint(&source_code, &env);
        let one = errs.remove(0);
        assert_eq!(errs.len(), 0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 9);
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        let one = errs.remove(0);
        assert_eq!(errs.len(), 1);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 9);
        let two = errs.remove(0);
        assert_eq!(errs.len(), 0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 1);
        assert_eq!(two.column, 11);
    }
}
