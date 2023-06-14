use colored::Colorize;
use tree_sitter::{Language, Node, Parser};

extern "C" {
    fn tree_sitter_julia() -> Language;
}
#[derive(Debug)]
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

fn analyse(node: &Node, src: &String, env: &Vec<String>) -> Option<UndefVar>{
    if let Ok(val) = node.utf8_text(src.as_bytes()) {
        let sym = val.to_string();
        if !env.contains(&sym.to_string()) {
            let r = row(&node);
            let c = col(&node);
            return Some(UndefVar {symbol: sym, row: r, column: c})
        }
        return None

    }
    return None
}
fn eval_expression_body(node: &Node, src: &String, env: &Vec<String>, from: usize, result: &mut Vec<Option<UndefVar,>>) {
    let mut tc = node.walk();
    let mut ifenv = Vec::<String>::new();
    ifenv.extend_from_slice(env);
    for child in node.named_children(&mut tc).skip(from) {
       print_node(&child, src);
       if child.kind() == "assignment_expression" {
           if let Some(lhs) = child.named_child(0) {
               if lhs.kind() == "identifier"{
                   ifenv.push(node_value(&lhs, src));
               }
               if lhs.kind() == "tuple_expression"{
                   let mut tc = lhs.walk();
                   for param in lhs.named_children(&mut tc) {
                       ifenv.push(node_value(&param, src));
                   }
               }
           }
           if let Some(_) = child.named_child(1) {
                result.extend(eval(&child, src, &ifenv.clone()));
           }
       }
       else {
           result.extend(eval(&child, src, &ifenv.clone()));
       }
    }
}
fn eval(node: &Node, src: &String, env: &Vec<String>) -> Vec<Option<UndefVar,>>{
    let mut result = Vec::<Option<UndefVar>>::new();
    match node.kind() {
        "source_file" | "ternary_expression" | "tuple_expression" => {
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
        "identifier" => result.push(analyse(&node, src, env)),
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
        "if_statement" => eval_expression_body(node, src, env, 1, &mut result),
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
        z = (1, 2,)
        (i, j) = (1,2,)
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
        let mut errs = lint(&source_code);
        println!("test {:?}", errs);
        let first = errs.remove(0);
        if first.is_some() ==  false {
            panic!("Expected undefvar for z")
        }

        let one = first.unwrap();
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 3);
        assert_eq!(one.column, 16);

        //remove none
        let _ = errs.remove(0);

        let sec = errs.remove(0);
        if sec.is_some() ==  false {
            panic!("Expected undefvar for k")
        }
        let two = sec.unwrap();
        assert_eq!(two.symbol, "m".to_string());
        assert_eq!(two.row, 6);
        assert_eq!(two.column, 12);
    }
}
