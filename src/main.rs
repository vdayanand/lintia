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
fn scoped_eval(
    node: &Node,
    src: &String,
    env: &Vec<String>,
    result: &mut Vec<UndefVar>,
    skipval: usize,
) {
    let mut tc = node.walk();
    let mut newenv = Vec::<String>::new();
    newenv.extend_from_slice(env);
    for child in node.named_children(&mut tc).skip(skipval) {
        match child.kind() {
            "assignment_expression"
            | "variable_declaration"
            | "parameter_list"
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
                    if lhs.kind() == "typed_expression" {
                        if let Some(x) = lhs.named_child(0) {
                            if x.kind() == "identifier" {
                                newenv.push(node_value(&x, src));
                            }
                        }
                    }
                }
            }
            "argument_list" => {
                let mut tc = child.walk();
                for arg in child.named_children(&mut tc){
                    newenv.push(node_value(&arg, src));
                }
            }
            "import_statement" => {
                if let Some(child) = child.named_child(0) {
                    if child.kind() == "identifier" {
                        newenv.push(node_value(&child, src));
                    }
                    if child.kind() == "selected_import" {
                        let mut tc = child.walk();
                        for arg in child.named_children(&mut tc).skip(1){
                            newenv.push(node_value(&arg, src));
                        }
                    }
                }
            }
            "abstract_definition" => {
                if let Some(child) = child.named_child(0) {
                    if child.kind() == "identifier" {
                        newenv.push(node_value(&child, src));
                    }
                }
            }
            "struct_definition" => {
                if let Some(child) = child.named_child(0) {
                    if child.kind() == "identifier" {
                        newenv.push(node_value(&child, src));
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
            "catch_clause" => {
                break
            }
            _ => (),
        };
        result.extend(eval(&child, src, &newenv));
    }
}

fn eval_mut_env_func(node: &Node, src: &String, env: &Vec<String>, result: &mut Vec<UndefVar>) {
    let mut newenv = Vec::<String>::new();
    newenv.extend_from_slice(env);
    if let Some(name) = node.named_child(0) {
        newenv.push(node_value(&name, src))
    }
    scoped_eval(node, src, env, result, 1)
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
        | "command_string" | "macro_argument_list" => {
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
        | "argument_list" => scoped_eval(node, src, env, &mut result, 0),

        "number" | "comment" | "continue_statement" | "break_statement" | "quote_expression"
            | "string" | "import_statement" | "abstract_definition"  => (),
        "identifier" => {
            if let Some(failed) = analyse(&node, src, env) {
                result.push(failed);
            }
        }
        "typed_expression" => {
            if let Some(lhs) = node.named_child(0) {
               if let Some(failed) = analyse(&lhs, src, env) {
                   result.push(failed);
               }
            }
            if let Some(rhs) = node.named_child(1) {
               if let Some(failed) = analyse(&rhs, src, env) {
                   result.push(failed);
               }
            }
        }
        "subtype_clause" => {
            if let Some(rhs) = node.named_child(0) {
                result.extend(eval(&rhs, src, env))
            }
        }
        "struct_definition" => {
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, env))
            }
        }
        "variable_declaration" | "for_binding" | "parameter_list" => {
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, env))
            }
        }
        "assignment_expression" => {
            let mut newenv = Vec::<String>::new();
            newenv.extend_from_slice(env);
            if let Some(lhs) = node.named_child(0) {
                if lhs.kind() == "call_expression" {
                    if let Some(fname) = lhs.named_child(0) {
                        newenv.push(node_value(&fname, src))
                    }
                    if let Some(args) = lhs.named_child(1) {
                        scoped_eval(&lhs, src, env, &mut result, 1);
                        let mut tc = args.walk();
                        for child in args.named_children(&mut tc) {
                            if child.kind() == "identifier"{
                                newenv.push(node_value(&child, src))
                            }
                            else {
                                print_node(&child, src);
                                panic!("Unimplemented type");
                            }
                        }
                    }
                }
                if lhs.kind() == "typed_expression" {
                    if let Some(fname) = lhs.named_child(0) {
                        newenv.push(node_value(&fname, src))
                    }
                    if let Some(args) = lhs.named_child(1) {
                       if let Some(failed) = analyse(&args, src, env) {
                           result.push(failed);
                       }
                    }
                }
            }
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, &newenv));
            }
        }

        "function_definition" | "macro_definition" => eval_mut_env_func(node, src, env, &mut result),
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
        "macro_expression" => {
            if let Some(firstnode) = node.named_child(0) {
                if let Some(name) = firstnode.named_child(0) {
                    if let Some(failed) = analyse(&name, src, env){
                        result.push(failed);
                    }
                }
            }
            if let Some(macro_arg) = node.named_child(1){
                result.extend(eval(&macro_arg, src, env));
            }
        }
        "catch_clause" | "do_clause" => {
            let mut newenv = Vec::<String>::new();
            newenv.extend_from_slice(env);
            if let Some(firstnode) = node.named_child(0) {
                if firstnode.kind() == "identifier" {
                    newenv.push(node_value(&firstnode, src));
                    scoped_eval(&node, src, &newenv, &mut result, 1);
                }
                else {
                    scoped_eval(&node, src, env, &mut result, 0);
                }
            }
        }
        "try_statement" => {
            scoped_eval(&node, src, env, &mut result, 0);
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() == "catch_clause" {
                    result.extend(eval(&child, src, env));
                }
                if child.kind() == "finally_clause" {
                    scoped_eval(&child, src, env, &mut result, 0);
                }
            }
         }
        "module_definition" => {
            let mut newenv = Vec::<String>::new();
            newenv.extend_from_slice(env);
            if let Some(firstnode) = node.named_child(0) {
                 newenv.push(node_value(&firstnode, src));
                 scoped_eval(&node, src, &newenv, &mut result, 1);
            }
         }
        _ => {
            print_node(&node, src);
            panic!("Unimplemented kind {}", node.kind());
        }
    }
    return result;
}

fn lint(src: &str, env: &Vec<String>) -> Vec<UndefVar>{
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let root_node = tree.root_node();
    let mut result: Vec<UndefVar> = vec![];
    scoped_eval(&root_node, &String::from(src), &env, &mut result, 0);
    return result;
}

fn main() {
    let source_code = r#"
     x::Animal = 1
     x
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
    #[test]
    fn test_func() {
        let source_code = r#"
         function hello(x)
            y
         end
         "#;
        let env = vec!["y".to_string()];
        let errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 0);
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 12);
    }
    #[test]
    fn test_oneline_func() {
        let source_code = r#"
        f(x, y) = f(x, y-1)+1
        "#;
        let env: Vec<String> = vec![];
        let errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 0);
    }
    #[test]
    fn test_try() {
        let source_code = r#"
        try
            x = 1
            x = x + 1
            y
        catch ex
            x
            ex
        finally
            zx
        end
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 12);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 6);
        assert_eq!(two.column, 12);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "zx".to_string());
        assert_eq!(three.row, 9);
        assert_eq!(three.column, 12);
    }
    #[test]
    fn test_module_imp() {
        let source_code = r#"
        module X
          import A
          import A: c, v
          import A: d
          using A: e
          c
          v
          d
          e
          f
          X
        end
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "f".to_string());
        assert_eq!(one.row, 10);
        assert_eq!(one.column, 10);
    }
    #[test]
    fn test_struct() {
        let source_code = r#"
        abstract type Animal end
        struct Typee <: Animal
           Y
           Z
        end
        struct Typee2 <: Animal2
           Y
           Z
        end
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Animal2".to_string());
        assert_eq!(one.row, 6);
        assert_eq!(one.column, 25);
    }
    #[test]
    fn test_doclause() {
        let source_code = r#"
        test(x, y) do z
           z
           y
           t
        end
        "#;
        let env = vec!["test".to_string(), "x".to_string()];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 16);
        let second = errs.remove(0);
        assert_eq!(second.symbol, "y".to_string());
        assert_eq!(second.row, 3);
        assert_eq!(second.column, 11);
        let second = errs.remove(0);
        assert_eq!(second.symbol, "t".to_string());
        assert_eq!(second.row, 4);
        assert_eq!(second.column, 11);
    }
    #[test]
    fn test_typedassign() {
        let source_code = r#"
         x::Animal = 1
         x
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Animal".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 12);
    }
}
