use colored::Colorize;
use std::fs;
use toml::Value;
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
    let sym = node_value(&node, src);
    if !env.contains(&sym) {
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

fn macro_analyse(node: &Node, src: &String, env: &Vec<String>) -> Option<UndefVar> {
    let value = node_value(node, src);
    let sym = format!("@{}", value);
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
        // print_node(&node, src);
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
                    if lhs.kind() == "typed_expression" {
                        if let Some(x) = lhs.named_child(0) {
                            if x.kind() == "identifier" {
                                newenv.push(node_value(&x, src));
                            }
                        }
                    }
                }
            }
            "argument_list" | "parameter_list" => {
                let mut tc = child.walk();
                for arg in child.named_children(&mut tc) {
                    if arg.kind() == "typed_expression" || arg.kind() == "typed_parameter" {
                        if let Some(x) = arg.named_child(0) {
                            if x.kind() == "identifier" {
                                newenv.push(node_value(&x, src));
                            }
                        }
                    } else if arg.kind() == "optional_parameter" {
                        if let Some(lhs) = arg.named_child(0) {
                            if lhs.kind() == "identifier" {
                                newenv.push(node_value(&lhs, src));
                            }
                            if lhs.kind() == "typed_parameter" {
                                if let Some(lhsvar) = lhs.named_child(0) {
                                    newenv.push(node_value(&lhsvar, src));
                                }
                            }
                        }
                    } else if arg.kind() == "keyword_parameters" {
                        let mut tc = child.walk();
                        for keyword in arg.named_children(&mut tc) {
                            if let Some(lhs) = keyword.named_child(0) {
                                if lhs.kind() == "identifier" {
                                    newenv.push(node_value(&lhs, src));
                                }
                                if lhs.kind() == "typed_parameter" {
                                    if let Some(lhsvar) = lhs.named_child(0) {
                                        newenv.push(node_value(&lhsvar, src));
                                    }
                                }
                            }
                        }
                    } else {
                        newenv.push(node_value(&arg, src));
                    }
                }
            }
            "import_statement" => {
                if let Some(child) = child.named_child(0) {
                    if child.kind() == "identifier" {
                        newenv.push(node_value(&child, src));
                    }
                    if child.kind() == "selected_import" {
                        let mut tc = child.walk();
                        for arg in child.named_children(&mut tc).skip(1) {
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
            "elseif_clause" | "else_clause" => {
                result.extend(eval(&child, src, &env));
                continue;
            }
            "function_definition" | "macro_definition" => {
                if let Some(name) = child.named_child(0) {
                    if let Some(dot) = child.named_child(1) {
                        if dot.kind() == "ERROR" {
                            newenv.push(node_value(&dot, src))
                        } else {
                            newenv.push(node_value(&name, src))
                        }
                    }
                }
            }
            "catch_clause" => break,
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
        | "command_string"
        | "macro_argument_list"
        | "type_argument_list" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() != "ERROR" {
                    result.extend(eval(&child, src, env));
                }
            }
        }
        "parameterized_identifier" => {
            if let Some(rnode) = node.named_child(0) {
                result.extend(eval(&rnode, src, env));
            }
            if let Some(rnode) = node.named_child(1) {
                result.extend(eval(&rnode, src, env));
            }
        }
        "named_argument" => {
            if let Some(rnode) = node.named_child(2) {
                result.extend(eval(&rnode, src, env));
            }
        }
        "unary_expression" => {
            if let Some(rnode) = node.named_child(0) {
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
        "source_file" | "let_statement" | "for_statement" | "while_statement" | "argument_list"
        | "parameter_list" | "keyword_parameters" => {
            scoped_eval(node, src, env, &mut result, 0);
        }
        "elseif_clause" => scoped_eval(&node, src, env, &mut result, 0),
        "else_clause" => scoped_eval(&node, src, env, &mut result, 0),
        "if_statement" => scoped_eval(&node, src, env, &mut result, 0),
        "number"
        | "comment"
        | "triple_string"
        | "continue_statement"
        | "break_statement"
        | "quote_expression"
        | "string"
        | "import_statement"
        | "abstract_definition"
        | "export_statement" => (),
        "identifier" => {
            if let Some(failed) = analyse(&node, src, env) {
                result.push(failed);
            }
        }
        "typed_expression" | "field_expression" => {
            if let Some(lhs) = node.named_child(0) {
                if let Some(failed) = analyse(&lhs, src, env) {
                    result.push(failed);
                }
            }
            if let Some(rhs) = node.named_child(1) {
                if rhs.kind() == "identifier" {
                    if let Some(failed) = analyse(&rhs, src, env) {
                        result.push(failed);
                    }
                } else {
                    result.extend(eval(&rhs, src, env))
                }
            }
        }
        "optional_parameter" => {
            if let Some(rhs) = node.named_child(0) {
                if rhs.kind() == "typed_parameter" {
                    if let Some(rhs_type) = rhs.named_child(1) {
                        result.extend(eval(&rhs_type, src, env))
                    }
                }
            }
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, env))
            }
        }
        "subtype_clause" => {
            if let Some(rhs) = node.named_child(0) {
                result.extend(eval(&rhs, src, env))
            }
        }
        "typed_parameter" => {
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, env))
            }
        }
        "struct_definition" => {
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, env))
            }
        }
        "variable_declaration" | "for_binding" => {
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
                        let mut tc = args.walk();
                        for child in args.named_children(&mut tc) {
                            if child.kind() == "identifier" {
                                newenv.push(node_value(&child, src));
                            }
                            if child.kind() == "typed_expression" {
                                if let Some(name) = child.named_child(0) {
                                    newenv.push(node_value(&name, src));
                                }
                            }
                        }
                        scoped_eval(&lhs, src, env, &mut result, 1);
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
                if lhs.kind() == "subscript_expression" {
                    result.extend(eval(&lhs, src, &newenv));
                }
            }
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(&rhs, src, &newenv));
            }
        }

        "function_definition" | "macro_definition" => {
            if let Some(_) = node.named_child(0) {
                if let Some(dot) = node.named_child(1) {
                    if dot.kind() == "ERROR" {
                        scoped_eval(node, src, env, &mut result, 2);
                    } else {
                        scoped_eval(node, src, env, &mut result, 1);
                    }
                }
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
        "macro_expression" => {
            if let Some(firstnode) = node.named_child(0) {
                if let Some(name) = firstnode.named_child(0) {
                    if let Some(failed) = macro_analyse(&name, src, env) {
                        result.push(failed);
                    }
                }
            }
            if let Some(macro_arg) = node.named_child(1) {
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
                } else {
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

fn lint(src: &str, env: &Vec<String>) -> Vec<UndefVar> {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let root_node = tree.root_node();
    let mut result: Vec<UndefVar> = vec![];
    scoped_eval(&root_node, &String::from(src), &env, &mut result, 0);
    return result;
}

fn load_env(file: &str) -> Vec<String> {
    let toml_str = fs::read_to_string(file).expect("Failed to read file");
    let parsed_toml: Value = toml::from_str(&toml_str).expect("Failed to parse TOML");
    if let Some(envs) = parsed_toml.get("envs") {
        if let Some(envs_array) = envs.as_array() {
            let envs_vec: Vec<String> = envs_array
                .iter()
                .filter_map(|env_value| env_value.as_str().map(String::from))
                .collect();
            return envs_vec;
        }
    }
    return Vec::<String>::new();
}

fn load_jl_file(file: &str) -> String {
    let jl_str = fs::read_to_string(file).expect("Failed to read file");
    return jl_str;
}

fn main() {
    let src = load_jl_file("test.jl");
    let env = load_env("src/pkgs/base.toml");
    let errs = lint(&src, &env);
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
         elseif y
              y
         else
           x
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
        let three = errs.remove(0);
        assert_eq!(three.symbol, "y".to_string());
        assert_eq!(three.row, 7);
        assert_eq!(three.column, 16);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "y".to_string());
        assert_eq!(four.row, 8);
        assert_eq!(four.column, 14);
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
    fn test_assign_subscript() {
        let source_code = r#"
           RG[]["ds"] = "d"
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "RG".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 11);
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
    #[test]
    fn test_oneline_func_typed() {
        let source_code = r#"
           f(x::Int, y)=1
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Int".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 16);
    }
    #[test]
    fn test_call_typed() {
        let source_code = r#"
        f(y, x::Int, y=1, j=2; k=1, m=2)
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 4);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "f".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 8);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "y".to_string());
        assert_eq!(two.row, 1);
        assert_eq!(two.column, 10);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "x".to_string());
        assert_eq!(three.row, 1);
        assert_eq!(three.column, 13);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "Int".to_string());
        assert_eq!(four.row, 1);
        assert_eq!(four.column, 16);
    }
    #[test]
    fn test_func_typed() {
        let source_code = r#"
           function f(x::Int, y=1, j::Int=2; k=1, m::Int2=2)
              x
              y
              z
              k
           end
        "#;
        let env: Vec<String> = vec![];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 4);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Int".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 25);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "Int".to_string());
        assert_eq!(two.row, 1);
        assert_eq!(two.column, 38);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "Int2".to_string());
        assert_eq!(three.row, 1);
        assert_eq!(three.column, 53);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "z".to_string());
        assert_eq!(four.row, 4);
        assert_eq!(four.column, 14);
    }
    #[test]
    fn test_parameterized_ids() {
        let source_code = r#"
        function main(x::Vector{<:Unit})
            y::Vector{Unit2}
            k::Vector{<:AbstractString, Int}
        end
        "#;
        let env: Vec<String> = vec!["y".to_string(), "Vector".to_string(), "Int".to_string()];
        let mut errs = lint(&source_code, &env);
        assert_eq!(errs.len(), 4);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Unit".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 34);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "Unit2".to_string());
        assert_eq!(two.row, 2);
        assert_eq!(two.column, 22);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "k".to_string());
        assert_eq!(three.row, 3);
        assert_eq!(three.column, 12);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "AbstractString".to_string());
        assert_eq!(four.row, 3);
        assert_eq!(four.column, 24);
    }
}
