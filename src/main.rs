use clap::{command, Arg};
use colored::Colorize;
use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::collections::HashMap;
use toml::Value;
use tree_sitter::{Language, Node, Parser, Tree};
use serde::{Serialize, Deserialize};
use serde_json;

extern "C" {
    fn tree_sitter_julia() -> Language;
}

#[derive(Debug)]
struct UndefVar {
    symbol: String,
    row: usize,
    column: usize,
    filepath: String
}

#[derive(Serialize, Deserialize, Debug)]
struct Module {
    name: String,
    symbols: Symbols,
    children: Vec<Module>,
}

#[derive(Serialize, Deserialize, Debug)]
struct Symbols {
    exported: Vec<String>,
    toplevel: Vec<String>,
}

#[derive(Debug)]
struct Ctx {
    src_module_root: Option<Module>,
    current_module: String,
    loaded_modules: HashMap<String, Module>,
    default_env: Vec<String>,
}

fn print_node(node: &tree_sitter::Node, src: &Src) {
    println!("Kind: {}", node.kind().cyan());
    println!("has error: {}", node.has_error());
    println!("val: {}", node_value(node, src));
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
    return p.row+1;
}

fn col(node: &Node) -> usize {
    let p = node.start_position();
    return p.column;
}

fn node_value(node: &Node, src: &Src) -> String {
    if let Ok(val) = node.utf8_text(src.src_str.as_bytes()) {
        return val.to_string();
    }
    return "".to_string();
}

fn is_symbol_defined(
    root: &Module,
    module_name: &String,
    symbol_name: &String,
    acc: &String,
) -> bool {
    if acc == module_name {
        return root.symbols.toplevel.contains(symbol_name);
    } else {
        for child in &root.children {
            let newmod = format!("{}.{}", acc, child.name);
            if is_symbol_defined(&child, module_name, symbol_name, &newmod) {
                return true;
            };
        }
    }
    return false;
}
fn analyse(ctx: &Ctx, node: &Node, src: &Src, env: &Vec<String>, idtype: &str) -> Option<UndefVar> {
    let sym = if idtype == "string_macro" {
        let mut b = String::from("@");
        b.push_str(&String::from(node_value(&node, src)));
        b.push_str(&String::from("_str"));
        b
    } else if idtype == "macro" {
        let mut b = String::from("@");
        b.push_str(&String::from(node_value(&node, src)));
        b
    } else {
        node_value(&node, src)
    };
    let found = if let Some(current_mo) = &ctx.src_module_root {
        is_symbol_defined(current_mo, &ctx.current_module, &sym, &current_mo.name)
    } else {
        false
    };
    if env.contains(&sym) || found || ctx.default_env.contains(&sym) {
        return None;
    }

    let r = row(&node);
    let c = col(&node);
    return Some(UndefVar {
        symbol: sym,
        row: r,
        column: c,
        filepath: src.src_path.clone()
    });
}

fn unex(node: &Node) {
    panic!("Unexpected kind {}", node.kind())
}

fn syms_function(node: &Node, src: &Src, syms: &mut Vec<String>) {
    if let Some(fname) = node.named_child(0) {
        if fname.kind() == "identifier" {
            syms.push(node_value(&fname, src));
        } else if fname.kind() == "field_expression" {
            if let Some(second) = fname.named_child(1) {
                syms.push(node_value(&second, src));
            }
        } else if fname.kind() == "function_object" || fname.kind() == "parameter_list" {
        } else if fname.kind() == "ERROR" {
            print_syntax_error(&fname, src);
        } else {
            print_node(&node, src);
            unex(&fname);
        }
    }
}
fn get_exported_symbols(ctx: &Ctx, module_name: &String) -> Vec<String> {
    for (name, moduleobj) in ctx.loaded_modules.iter() {
        if name == module_name {
            println!("moduleobj => {:?}", moduleobj);
            return moduleobj.symbols.exported.clone()
        }
    }
    return vec![]
}
fn toplevel_symbol(node: &Node, src: &Src) -> Vec<String> {
    let mut syms = Vec::<String>::new();
    match node.kind() {
        "const_declaration" => {
            if let Some(assign) = node.named_child(0) {
                if assign.kind() == "assignment" {
                    if let Some(var) = assign.named_child(0) {
                        if var.kind() == "identifier" {
                            syms.push(node_value(&var, src));
                        } else if var.kind() == "bare_tuple" || var.kind() == "tuple_expression" {
                            let mut tc = var.walk();
                            for val in var.named_children(&mut tc) {
                                syms.push(node_value(&val, src));
                            }
                        } else if var.kind() == "typed_expression" {
                            if let Some(typevar) = var.named_child(0) {
                                if typevar.kind() == "identifier" {
                                    syms.push(node_value(&typevar, src));
                                } else {
                                    print_node(&typevar, src);
                                    panic!("Unexpected")
                                }
                            }
                        }
                    }
                } else {
                    print_node(&node, src);
                    panic!("Unexpected!!")
                }
            }
        }
        "function_definition" | "short_function_definition" => syms_function(node, src, &mut syms),
        "macro_definition" => {
            if let Some(name) = node.named_child(0) {
                let mut b = String::from("@");
                b.push_str(&String::from(node_value(&name, src)));
                syms.push(b);
            }
        }
        "import_statement" => {
            if let Some(child) = node.named_child(0) {
                if child.kind() == "identifier" {
                    syms.push(node_value(&child, src));
                }
                if child.kind() == "selected_import" {
                    let mut tc = child.walk();
                    for arg in child.named_children(&mut tc).skip(1) {
                        syms.push(node_value(&arg, src));
                    }
                }
            }
        }
        "abstract_definition" | "struct_definition" | "module_definition" => {
            if let Some(child) = node.named_child(0) {
                if child.kind() == "identifier" {
                    syms.push(node_value(&child, src));
                }
            }
        }
        "local_declaration" | "global_declaration" => {
            let mut tc = node.walk();
            for var in node.named_children(&mut tc) {
                syms.push(node_value(&var, src))
            }
        }
        _ => (),
    }
    return syms;
}
fn sym_for_binding(node: &Node, src: &Src, env: &mut Vec<String>) {
    if let Some(lhs) = node.named_child(0) {
        if lhs.kind() == "identifier" {
            env.push(node_value(&lhs, src));
        }
        if lhs.kind() == "tuple_expression" {
            let mut tc = lhs.walk();
            for param in lhs.named_children(&mut tc) {
                env.push(node_value(&param, src));
            }
        }
        if lhs.kind() == "typed_expression" {
            if let Some(x) = lhs.named_child(0) {
                if x.kind() == "identifier" {
                    env.push(node_value(&x, src));
                }
            }
        }
    }
}
fn scoped_eval(
    ctx: &mut Ctx,
    node: &Node,
    src: &Src,
    env: &Vec<String>,
    result: &mut Vec<UndefVar>,
    skip: usize,
) {
    let mut tc = node.walk();
    let mut newenv = Vec::<String>::new();
    newenv.extend_from_slice(env);

    for child in node.named_children(&mut tc).skip(skip) {
        if child.kind() == "elseif_clause" || child.kind() == "else_clause" {
            result.extend(eval(ctx, &child, src, &env));
            continue;
        }
        if child.kind() == "function_definition" || child.kind() == "short_function_definition" {
            syms_function(&child, src, &mut newenv);
            let mut tc = child.walk();
            let mut funenv: Vec<String> = vec![];
            funenv.extend_from_slice(&newenv);
            for name in child.named_children(&mut tc) {
                if name.kind() == "where_clause" {
                    if let Some(ch) = name.named_child(0) {
                        if ch.kind() == "curly_expression" {
                            let mut tc = ch.walk();
                            for c in ch.named_children(&mut tc) {
                                funenv.push(node_value(&c, src));
                            }
                        }
                        if ch.kind() == "identifier" {
                            funenv.push(node_value(&ch, src));
                        }
                        println!("funenv => {:?}", funenv);
                    }
                }
            }
            result.extend(eval(ctx, &child, src, &funenv));
            continue;
        }
        if child.kind() == "parameter_list" {
            let mut tc = child.walk();
            for param in child.named_children(&mut tc) {
                if param.kind() == "identifier" {
                    newenv.push(node_value(&param, src));
                } else if param.kind() == "optional_parameter" {
                    if let Some(lhs) = param.named_child(0) {
                        if lhs.kind() == "identifier" {
                            newenv.push(node_value(&lhs, src));
                        } else if lhs.kind() == "typed_parameter" {
                            if let Some(typevar) = lhs.named_child(0) {
                                newenv.push(node_value(&typevar, src));
                            }
                        } else {
                            print_node(&lhs, src);
                            panic!("Unxpected type");
                        }
                    }
                } else if param.kind() == "keyword_parameters" {
                    let mut tc = param.walk();
                    for x in param.named_children(&mut tc) {
                        if x.kind() == "identifier" {
                            newenv.push(node_value(&x, src));
                        } else if x.kind() == "optional_parameter" {
                            if let Some(lhs) = x.named_child(0) {
                                if lhs.kind() == "identifier" {
                                    newenv.push(node_value(&lhs, src));
                                } else if lhs.kind() == "typed_parameter" {
                                    if let Some(typevar) = lhs.named_child(0) {
                                        newenv.push(node_value(&typevar, src));
                                    }
                                } else {
                                    print_node(&lhs, src);
                                    panic!("Unxpected type");
                                }
                            }
                        }
                    }
                } else if param.kind() == "typed_parameter" {
                    if let Some(lhs) = param.named_child(0) {
                        if lhs.kind() == "identifier" {
                            newenv.push(node_value(&lhs, src));
                        }
                    }
                } else if param.kind() == "slurp_parameter" {
                    if let Some(lhs) = param.named_child(0) {
                        if lhs.kind() == "identifier" {
                            newenv.push(node_value(&lhs, src));
                        }
                    }
                } else {
                    print_node(&param, src);
                    panic!("Howzzz");
                }
            }
        }
        if child.kind() == "import_statement" {
            if let Some(nmodule) = child.named_child(0) {
                if nmodule.kind() == "identifier"{
                    let module_name = node_value(&nmodule, src);
                    let mut exported = get_exported_symbols(&ctx, &module_name);
                    println!("exported => {:?} module_name => {}", exported, module_name);
                    exported.push(module_name);
                    newenv.extend_from_slice(&exported);
                }
            }
        }
        if child.kind() == "assignment"
            || child.kind() == "typed_parameter"
            || child.kind() == "optional_parameter"
        {
            if let Some(lhs) = child.named_child(0) {
                if lhs.kind() == "identifier" {
                    newenv.push(node_value(&lhs, src));
                } else if lhs.kind() == "typed_expression" {
                    if let Some(lhstyped) = lhs.named_child(0) {
                        newenv.push(node_value(&lhstyped, src));
                    }
                } else if lhs.kind() == "tuple_expression" || lhs.kind() == "bare_tuple" {
                    let mut tc = lhs.walk();
                    for val in lhs.named_children(&mut tc) {
                        newenv.push(node_value(&val, src));
                    }
                }
            }
        }
        if child.kind() == "let_statement" {
            if let Some(args) = child.named_child(0) {
                if args.kind() == "identifier" {
                    newenv.push(node_value(&args, src));
                }
                if args.kind() == "let_binding" {
                    if let Some(name) = args.named_child(0) {
                        newenv.push(node_value(&name, src));
                    }
                }
            }
        }
        if child.kind() == "catch_clause" {
            break;
        }
        if child.kind() == "function_object" {
            if let Some(first) = child.named_child(0) {
                newenv.push(node_value(&first, src));
            }
        }
        if child.kind() == "local_declaration" || child.kind() == "global_declaration" {
            let mut tc = child.walk();
            for var in child.named_children(&mut tc) {
                newenv.push(node_value(&var, src))
            }
        }
        if child.kind() == "for_binding" {
            sym_for_binding(&child, src, &mut newenv);
        }
        result.extend(eval(ctx, &child, src, &newenv));
    }
}
fn print_syntax_error(node: &Node, src: &Src) {
    panic!("Syntax Error at line {} in {}", row(node) + 1, src.src_path)
}

fn eval(ctx: &mut Ctx, node: &Node, src: &Src, env: &Vec<String>) -> Vec<UndefVar> {
    let mut result = Vec::<UndefVar>::new();
    //    print_node(&node, src);
    match node.kind() {
        "string_literal"
        | "boolean_literal"
        | "float_literal"
        | "break_statement"
        | "character_literal"
        | "integer_literal"
        | "abstract_definition"
        | "import_statement"
        | "continue_statement"
        | "line_comment"
        | "block_comment"
        | "command_literal"
        | "where_clause"
        | "quote_expression"
        | "quote_statement"
        | "local_declaration"
        | "macro_definition" => (),

        "function_definition"
        | "let_statement"
        | "while_statement"
        | "short_function_definition"
        | "compound_statement"
        | "finally_clause" => scoped_eval(ctx, node, src, env, &mut result, 0),
        "variable_declaration" | "for_binding" | "let_binding" => {
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(ctx, &rhs, src, env))
            }
        }
        "for_statement" => {
            scoped_eval(ctx, &node, src, &env, &mut result, 0);
        }
        "where_expression" => {
            let mut newenv = Vec::<String>::new();
            newenv.extend_from_slice(&env);

            if let Some(defs) = node.named_child(1) {
                if defs.kind() == "identifier" {
                    newenv.push(node_value(&defs, src));
                } else {
                    unex(&defs);
                }
            }
            if let Some(body) = node.named_child(0) {
                result.extend(eval(ctx, &body, src, &newenv))
            }
        }
        "function_object" => {
            if let Some(second) = node.named_child(1) {
                result.extend(eval(ctx, &second, src, &env));
            }
        }
        "comprehension_expression" => {
            let mut newenv = Vec::<String>::new();
            newenv.extend_from_slice(&env);
            if let Some(clause) = node.named_child(1) {
                if clause.kind() == "for_clause" {
                    if let Some(binding) = clause.named_child(0) {
                        if let Some(var) = binding.named_child(0) {
                            newenv.push(node_value(&var, src))
                        }
                        if let Some(expr) = binding.named_child(1) {
                            result.extend(eval(ctx, &expr, src, &env));
                        }
                    }
                } else {
                    unex(&clause);
                }
            }
            if let Some(exp) = node.named_child(0) {
                result.extend(eval(ctx, &exp, src, &newenv));
            }
        }
        "macrocall_expression" => {
            if let Some(firstnode) = node.named_child(0) {
                if let Some(name) = firstnode.named_child(0) {
                    if let Some(failed) = analyse(&ctx, &name, src, env, "macro") {
                        result.push(failed);
                    }
                }
            }
            if let Some(macro_arg) = node.named_child(1) {
                result.extend(eval(ctx, &macro_arg, src, env));
            }
        }
        "parenthesized_expression" | "global_declaration" | "slurp_parameter" | "interpolation_expression"=> {
            if let Some(rnode) = node.named_child(0) {
                result.extend(eval(ctx, &rnode, src, env));
            }
        }
        "prefixed_string_literal" => {
            if let Some(name) = node.named_child(0) {
                if let Some(failed) = analyse(&ctx, &name, src, env, "string_macro") {
                    result.push(failed);
                }
            }
        }
        "struct_definition" => {
            let mut tc = node.walk();
            for field in node.named_children(&mut tc) {
                if field.kind() == "identifier" {
                    continue;
                } else if field.kind() == "typed_expression" {
                    if let Some(typed) = field.named_child(1) {
                        result.extend(eval(ctx, &typed, src, &env));
                    }
                } else if field.kind() == "type_clause" {
                    result.extend(eval(ctx, &field, src, &env));
                } else if field.kind() == "function_definition"
                    || field.kind() == "short_function_definition"
                {
                    let mut newenv: Vec<String> = vec![];
                    newenv.extend_from_slice(env);
                    if let Some(fname) = node.named_child(0) {
                        newenv.push(node_value(&fname, src));
                        newenv.push("new".to_string());
                    }
                    result.extend(eval(ctx, &field, src, &newenv));
                } else if field.kind() == "line_comment" || field.kind() == "block_comment" {
                } else {
                    unex(&field);
                }
            }
        }
        "try_statement" => {
            scoped_eval(ctx, &node, src, env, &mut result, 0);
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() == "catch_clause" {
                    result.extend(eval(ctx, &child, src, env));
                }
                if child.kind() == "finally_clause" {
                    scoped_eval(ctx, &child, src, env, &mut result, 0);
                }
            }
        }
        "ternary_expression"
        | "tuple_expression"
        | "broadcast_call_expression"
        | "spread_expression"
        | "splat_expression"
        | "array_expression"
        | "matrix_expression"
        | "matrix_row"
        | "macro_argument_list"
        | "bare_tuple"
        | "pair_expression"
        | "range_expression"
        | "command_string"
        | "type_argument_list"
        | "export_statement" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                result.extend(eval(ctx, &child, src, env));
            }
        }
        "return_statement" => {
            if let Some(rnode) = node.named_child(0) {
                if rnode.kind() == "identifier" {
                    if let Some(failed) = analyse(&ctx, &rnode, src, env, "normal") {
                        result.push(failed);
                    }
                } else {
                    result.extend(eval(ctx, &rnode, src, env));
                }
            }
        }
        "unary_expression" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() != "operator" {
                    result.extend(eval(ctx, &child, src, env));
                }
            }
        }
        "const_declaration" => {
            if let Some(rnode) = node.named_child(0) {
                result.extend(eval(ctx, &rnode, src, env));
            }
        }
        "module_definition" => {
            if let Some(name) = node.named_child(0) {
                ctx.current_module = format!("{}.{}", ctx.current_module, node_value(&name, src))
            } else {
                unex(node);
            }
            scoped_eval(ctx, &node, src, &env, &mut result, 1);
        }
        "identifier" | "macro_identifier" => {
            if let Some(failed) = analyse(&ctx, &node, src, env, "normal") {
                result.push(failed);
            }
        }
        "assignment" | "compound_assignment_expression" => {
            assert!(node.named_child_count() == 3);
            if let Some(rhs) = node.named_child(0) {
                if rhs.kind() == "typed_expression" {
                    result.extend(eval(ctx, &rhs, src, &env));
                }
                if rhs.kind() == "index_expression" {
                    result.extend(eval(ctx, &rhs, src, &env));
                }
            }
            if let Some(rhs) = node.named_child(2) {
                result.extend(eval(ctx, &rhs, src, &env));
            }
        }
        "field_expression" => {
            if let Some(first) = node.named_child(0) {
                result.extend(eval(ctx, &first, src, &env));
            }
        }
        "index_expression" | "vector_expression" | "keyword_parameters" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() != "operator" {
                    result.extend(eval(ctx, &child, src, &env));
                }
            }
        }
        "argument_list" => {
            let mut tc = node.walk();
            let mut newenv: Vec<String> = vec![];
            newenv.extend_from_slice(env);
            for child in node.named_children(&mut tc) {
                if child.kind() == "for_clause" {
                    if let Some(name) = child.named_child(0) {
                        sym_for_binding(&name, src, &mut newenv)
                    }
                }
            }
            for child in node.named_children(&mut tc) {
                if child.kind() == "for_clause" {
                    if let Some(name) = child.named_child(0) {
                        result.extend(eval(ctx, &name, src, &newenv));
                    }
                } else if child.kind() != "operator" {
                    result.extend(eval(ctx, &child, src, &newenv));
                }
            }
        }
        "call_expression" => {
            let mut tc = node.walk();
            if let Some(parseinfo) = include_node(&node, src) {
                let tree = parseinfo.tree;
                let newsrc = parseinfo.src;
                let root = tree.root_node();
                let mut tc = root.walk();
                change_pwd(&PathBuf::from(&newsrc.src_path));
                scoped_eval(ctx, &root, &newsrc, &Vec::<String>::new(), &mut result, 0);
                change_pwd(&PathBuf::from(&src.src_path));
            } else {
                for child in node.named_children(&mut tc) {
                    if child.kind() != "operator" {
                        result.extend(eval(ctx, &child, src, &env));
                    }
                }
            }
        }
        "binary_expression" => {
            let mut tc = node.walk();
            for operand in node.named_children(&mut tc) {
                if operand.kind() != "operator" {
                    result.extend(eval(ctx, &operand, src, &env));
                }
            }
        }
        "catch_clause" => {
            let mut newenv = Vec::<String>::new();
            newenv.extend_from_slice(env);
            if let Some(firstnode) = node.named_child(0) {
                if firstnode.kind() == "identifier" {
                    newenv.push(node_value(&firstnode, src));
                    scoped_eval(ctx, &node, src, &newenv, &mut result, 1);
                } else {
                    scoped_eval(ctx, &node, src, env, &mut result, 0);
                }
            }
        }
        "function_expression" => {
            let mut newenv = Vec::<String>::new();
            newenv.extend_from_slice(env);
            if let Some(firstnode) = node.named_child(0) {
                if firstnode.kind() == "identifier" {
                    newenv.push(node_value(&firstnode, src));
                    scoped_eval(ctx, &node, src, &newenv, &mut result, 1);
                } else {
                    scoped_eval(ctx, &node, src, env, &mut result, 0);
                }
            }
        }
        "parameter_list" | "do_clause" => scoped_eval(ctx, node, src, env, &mut result, 0),
        "typed_parameter" | "named_field" => {
            assert!(node.named_child_count() <= 2);
            if let Some(typenode) = node.named_child(1) {
                result.extend(eval(ctx, &typenode, src, &env));
            }
        }
        "named_argument" => {
            let mut tc = node.walk();
            for name in node.named_children(&mut tc).skip(1) {
                if name.kind() != "operator" {
                    result.extend(eval(ctx, &name, src, env));
                }
            }
        }

        "optional_parameter" => {
            assert!(node.named_child_count() <= 2);
            if let Some(typenode) = node.named_child(0) {
                if typenode.kind() == "typed_parameter" {
                    result.extend(eval(ctx, &typenode, src, &env))
                }
            } else if let Some(typenode) = node.named_child(1) {
                result.extend(eval(ctx, &typenode, src, &env));
            }
        }
        "typed_expression" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                result.extend(eval(ctx, &child, src, &env));
            }
        }
        "parametrized_type_expression" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                result.extend(eval(ctx, &child, src, &env));
            }
        }
        "type_clause" => {
            if let Some(typenode) = node.named_child(0) {
                result.extend(eval(ctx, &typenode, src, &env));
            }
        }
        "curly_expression" => {
            let mut tc = node.walk();
            for typenode in node.named_children(&mut tc) {
                result.extend(eval(ctx, &typenode, src, &env));
            }
        }
        "elseif_clause" => scoped_eval(ctx, &node, src, env, &mut result, 0),
        "else_clause" => scoped_eval(ctx, &node, src, env, &mut result, 0),

        "if_statement" => scoped_eval(ctx, &node, src, env, &mut result, 0),
        "ERROR" => {
            print_syntax_error(node, src);
        }
        _ => {
            print_node(&node, src);
            panic!("Unimplemented kind {}", node.kind());
        }
    }
    return result;
}

#[derive(Debug)]
struct Src {
    src_str: String,
    src_path: String,
}

#[derive(Debug)]
struct ParseInfo {
    tree: Tree,
    src: Src,
}

fn include_node(child: &Node, src: &Src) -> Option<ParseInfo> {
    if child.kind() == "call_expression" {
        if let Some(fname) = child.named_child(0) {
            if fname.kind() == "identifier" {
                let fnameval = node_value(&fname, src);
                if fnameval == "include" {
                    if let Some(fileargs) = child.named_child(1) {
                        if fileargs.kind() == "argument_list" {
                            let mut tc = fileargs.walk();
                            for filepath in fileargs.named_children(&mut tc) {
                                let newsrc_path_u = node_value(&filepath, src);
                                let newsrc_path = newsrc_path_u.trim_matches('"');
                                let fullpath =
                                    if let Ok(absolute_path) = fs::canonicalize(newsrc_path) {
                                        absolute_path
                                    } else {
                                        panic!(
                                            "Failed to get the absolute path {} cwd={:?}",
                                            newsrc_path,
                                            current_pwd()
                                        );
                                    };
                                let path = fullpath.to_string_lossy().into_owned();
                                let newsrc = load_jl_file(&fullpath);
                                let src = Src {
                                    src_str: newsrc,
                                    src_path: path,
                                };
                                let tree = parse_node(&src);
                                return Some(ParseInfo {
                                    tree: tree,
                                    src: src,
                                });
                            }
                        } else {
                            print_node(&fileargs, src);
                            unex(&fileargs);
                        }
                    } else {
                        unex(&child);
                    };
                }
            }
        }
    }
    return None;
}

fn construct_module_tree(mut current_mod: Module, root_node: &Node, src: &Src) -> Module {
    let mut tc = root_node.walk();
    let mut symbols = Symbols {
        exported: vec![],
        toplevel: current_mod.symbols.toplevel.to_vec(),
    };
    for child in root_node.named_children(&mut tc) {
        let toplevel = toplevel_symbol(&child, src);
        symbols.toplevel.extend_from_slice(&toplevel);

        if let Some(parseinfo) = include_node(&child, src) {
            let tree = parseinfo.tree;
            let newsrc = parseinfo.src;
            let root = tree.root_node();
            let mut tc = root.walk();
            change_pwd(&PathBuf::from(&newsrc.src_path));
            for child in root.named_children(&mut tc) {
                let toplevel = toplevel_symbol(&child, &newsrc);
                symbols.toplevel.extend_from_slice(&toplevel);
            }
            change_pwd(&PathBuf::from(&src.src_path));
        }
        if child.kind() == "export_statement" {
            let mut tc = child.walk();
            for expsym in child.named_children(&mut tc) {
                if expsym.kind() == "identifier" {
                    symbols.exported.push(node_value(&expsym, src));
                } else {
                    unex(&expsym);
                }
            }
        }
    }
    current_mod.symbols = symbols;
    for ele in root_node.named_children(&mut tc) {
        if ele.kind() == "module_definition" {
            let module_name = if let Some(module_name) = ele.named_child(0) {
                node_value(&module_name, src)
            } else {
                unex(&ele);
                "".to_string()
            };
            current_mod.children.push(construct_module_tree(
                Module {
                    name: module_name.clone(),
                    symbols: Symbols {
                        exported: vec![],
                        toplevel: vec![module_name.clone()],
                    },
                    children: vec![],
                },
                &ele,
                src,
            ));
        }
        if let Some(parseinfo) = include_node(&ele, src) {
            let tree = parseinfo.tree;
            let newsrc = parseinfo.src;
            let root = tree.root_node();
            let mut tc = root.walk();
            change_pwd(&PathBuf::from(&newsrc.src_path));
            for ch in root.named_children(&mut tc) {
                if ch.kind() == "module_definition" {
                    let module_name = if let Some(module_name) = ch.named_child(0) {
                        node_value(&module_name, &newsrc)
                    } else {
                        unex(&ele);
                        "".to_string()
                    };
                    current_mod.children.push(construct_module_tree(
                        Module {
                            name: module_name.clone(),
                            symbols: Symbols {
                                exported: vec![],
                                toplevel: vec![module_name.clone()],
                            },
                            children: vec![],
                        },
                        &ch,
                        &newsrc,
                    ));
                }
            }
            change_pwd(&PathBuf::from(&src.src_path));
        }
    }

    return current_mod;
}

fn parse_node(src: &Src) -> Tree {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(&src.src_str, None).unwrap();
    return tree;
}

fn module_from_file(modname: &str, file: &PathBuf) -> Module {
    let content = load_jl_file(file);
    let src = Src {src_str: content, src_path: file.to_string_lossy().into_owned()};
    let tree = parse_node(&src);
    let root_node = tree.root_node();
    let main_module = Module {
        name: modname.to_string(),
        symbols: Symbols {
            exported: vec![],
            toplevel: vec![modname.to_string()],
        },
        children: vec![],
    };
    let mut tc = root_node.walk();
    for modulenode in root_node.named_children(&mut tc) {
        if modulenode.kind() == "module_definition" {
            return construct_module_tree(main_module, &modulenode, &src)
        }
    }
    panic!("Unexpected state while constructing module")
}
fn write_file(file: &PathBuf, content: &String) -> Result<(), std::io::Error> {
    let mut file_handle = fs::File::create(file)?;
    file_handle.write_all(content.as_bytes())?;
    file_handle.flush()?;
    Ok(())
}
fn lint(ctx: &mut Ctx, src: &Src, env: &Vec<String>) -> Vec<UndefVar> {
    let tree = parse_node(src);
    let root_node = tree.root_node();
    let mut result: Vec<UndefVar> = vec![];
    let default_module = Module {
        name: "Main".to_string(),
        symbols: Symbols {
            exported: vec![],
            toplevel: vec!["Main".to_string()],
        },
        children: vec![],
    };
    let modtree = construct_module_tree(default_module, &root_node, src);
    ctx.src_module_root = Some(modtree);
    ctx.current_module = "Main".to_string();
    let uuidmod = module_from_file("UUIDs", &PathBuf::from("/Users/vdayanand/code/julia/stdlib/UUIDs/src/UUIDs.jl"));
    let json = serde_json::to_string(&uuidmod).unwrap();
    _ = write_file(&PathBuf::from("/Users/vdayanand/.lintia/test.json"), &json);
    ctx.loaded_modules.insert("UUIDs".to_string(),  uuidmod);
    scoped_eval(ctx, &root_node, src, env, &mut result, 0);
    return result;
}

fn get_env_vec(tomlstr: &str) -> Vec<String> {
    let parsed_toml: Value = toml::from_str(&tomlstr).expect("Failed to parse TOML");
    if let Some(envs) = parsed_toml.get("envs") {
        if let Some(envs_array) = envs.as_array() {
            return envs_array
                .iter()
                .filter_map(|env_value| env_value.as_str().map(String::from))
                .collect();
        }
    }
    return vec![];
}

fn load_jl_file(file: &PathBuf) -> String {
    let jl_str = if let Ok(jl_str) = fs::read_to_string(file) {
//        println!("loaded julia file {:?}", file);
        jl_str
    } else {
        panic!("failed to load julia file {:?}", file);
    };
    return jl_str;
}

fn change_pwd(path: &PathBuf) {
    let newpwd = path.parent().unwrap();
//    println!("Changing pwd to {:?} {:?}", newpwd, path);
    env::set_current_dir(newpwd).expect("Failed to change directory");
}

fn current_pwd() -> PathBuf {
    env::current_dir().unwrap()
}

fn main() {
    let matches = command!() // requires `cargo` feature
        .arg(Arg::new("file"))
        .get_matches();

    if let Ok(current_dir) = env::current_dir() {
        println!("Current working directory: {:?}", current_dir);
    } else {
        eprintln!("Failed to get the current working directory");
    }

    let file = if let Some(file) = matches.get_one::<String>("file") {
        if let Ok(absolute_path) = fs::canonicalize(file) {
            absolute_path
        } else {
            panic!(
                "Failed to get the absolute path {} cwd: {:?}",
                file,
                current_pwd()
            );
        }
    } else {
        println!("File is missing");
        return;
    };
    let src = load_jl_file(&file);
    //let env = load_env("src/pkgs");
    let boot_env = include_str!("pkgs/boot.toml");
    let mut boot_env_list = get_env_vec(boot_env);
    let base_env = include_str!("pkgs/base.toml");
    let mut base_env_list = get_env_vec(base_env);
    boot_env_list.append(&mut base_env_list);
    change_pwd(&file);
    let mut ctx = Ctx {
        src_module_root: None,
        current_module: "".to_string(),
        loaded_modules: HashMap::new(),
        default_env: boot_env_list,
    };
    let localenv = Vec::<String>::new();
    let src = Src {
        src_str: src,
        src_path: file.to_string_lossy().into_owned(),
    };
    let linfo = lint(&mut ctx, &src, &localenv);
    for err in linfo {
        println!(
            "Undefined symbol {} found at row:{} col:{} in {}",
            err.symbol.red(),
            err.row,
            err.column,
            err.filepath
        );
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_if() {
        let snip = r#"
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 16);

        let two = errs.remove(0);
        assert_eq!(two.symbol, "m".to_string());
        assert_eq!(two.row, 7);
        assert_eq!(two.column, 12);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "y".to_string());
        assert_eq!(three.row, 8);
        assert_eq!(three.column, 16);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "y".to_string());
        assert_eq!(four.row, 9);
        assert_eq!(four.column, 14);
    }
    #[test]
    fn test_let() {
        let snip = r#"
         let x
            x
            z
         end
         let x=1
            x
            z
         end
         "#;
        let env = vec!["x".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };

        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 12);
    }
    #[test]
    fn test_callexp() {
        let snip = r#"
         z(x)
         "#;
        let env = vec!["x".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);

        let one = errs.remove(0);
        assert_eq!(errs.len(), 0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 9);
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);

        let one = errs.remove(0);
        assert_eq!(errs.len(), 1);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 9);
        let two = errs.remove(0);
        assert_eq!(errs.len(), 0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 2);
        assert_eq!(two.column, 11);
    }
    #[test]
    fn test_func() {
        let snip = r#"
         module Test
         function hello(x)
            function test()
            end
            test()
            y
         end
         end
         "#;
        let env = vec!["y".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 0);
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 7);
        assert_eq!(one.column, 12);
    }
    #[test]
    fn test_oneline_func() {
        let snip = r#"
        f(x, y) = f(x, y-1)+1
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 0);
    }
    #[test]
    fn test_assign_subscript() {
        let snip = r#"
           RG[]["ds"] = "d"
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "RG".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 11);
    }
    #[test]
    fn test_try() {
        let snip = r#"
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 5);
        assert_eq!(one.column, 12);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 7);
        assert_eq!(two.column, 12);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "zx".to_string());
        assert_eq!(three.row, 10);
        assert_eq!(three.column, 12);
    }
    #[test]
    fn test_module_imp() {
        let snip = r#"
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        println!("errs {:?}", errs);
        assert_eq!(errs.len(), 1);

        let one = errs.remove(0);
        assert_eq!(one.symbol, "f".to_string());
        assert_eq!(one.row, 11);
        assert_eq!(one.column, 10);
    }
    #[test]
    fn test_struct() {
        let snip = r#"
        abstract type Animal end
        struct Typee <: Animal
            Y::Int
            Z
            Typee(U)=new(1,2)
            function Typee(U)
                x
                new(1,2)
            end
        end
        struct Typee2 <: Animal2
            T
            M
        end
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        println!("{:?}", errs);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Int".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 15);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 8);
        assert_eq!(two.column, 16);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "Animal2".to_string());
        assert_eq!(three.row, 12);
        assert_eq!(three.column, 25);
    }
    #[test]
    fn test_doclause() {
        let snip = r#"
        test(x, y) do z
           z
           y
           t
        end
        "#;
        let env = vec!["test".to_string(), "x".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 16);
        let second = errs.remove(0);
        assert_eq!(second.symbol, "y".to_string());
        assert_eq!(second.row, 4);
        assert_eq!(second.column, 11);
        let second = errs.remove(0);
        assert_eq!(second.symbol, "t".to_string());
        assert_eq!(second.row, 5);
        assert_eq!(second.column, 11);
    }
    #[test]
    fn test_typedassign() {
        let snip = r#"
         x::Animal = 1
         x
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Animal".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 12);
    }
    #[test]
    fn test_oneline_func_typed() {
        let snip = r#"
           f(x::Int, y)=1
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Int".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 16);
    }
    #[test]
    fn test_call_typed() {
        let snip = r#"
        f(y, x::Int, y=1, j=2; k=1, m=2)
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 4);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "f".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 8);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "y".to_string());
        assert_eq!(two.row, 2);
        assert_eq!(two.column, 10);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "x".to_string());
        assert_eq!(three.row, 2);
        assert_eq!(three.column, 13);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "Int".to_string());
        assert_eq!(four.row, 2);
        assert_eq!(four.column, 16);
    }
    #[test]
    fn test_func_typed() {
        let snip = r#"
           function f(x::Int, y=1, j::Int=2; k=1, m::Int2=2)
              x
              y
              z
              k
           end
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 4);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Int".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 25);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "Int".to_string());
        assert_eq!(two.row, 2);
        assert_eq!(two.column, 38);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "Int2".to_string());
        assert_eq!(three.row, 2);
        assert_eq!(three.column, 53);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "z".to_string());
        assert_eq!(four.row, 5);
        assert_eq!(four.column, 14);
    }
    #[test]
    fn test_parameterized_ids() {
        let snip = r#"
        function main(x::Vector{<:Unit})
            y::Vector{Unit2}
            k::Vector{<:AbstractString, Int}
        end
        "#;
        let env: Vec<String> = vec!["y".to_string(), "Vector".to_string(), "Int".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);

        assert_eq!(errs.len(), 4);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Unit".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 34);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "Unit2".to_string());
        assert_eq!(two.row, 3);
        assert_eq!(two.column, 22);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "k".to_string());
        assert_eq!(three.row, 4);
        assert_eq!(three.column, 12);
        let four = errs.remove(0);
        assert_eq!(four.symbol, "AbstractString".to_string());
        assert_eq!(four.row, 4);
        assert_eq!(four.column, 24);
    }
    #[test]
    fn test_const_declaration() {
        let snip = r#"
        const x = 1
        x
        y
        const z::Int = 1
        k
        z
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 8);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "Int".to_string());
        assert_eq!(two.row, 5);
        assert_eq!(two.column, 17);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "k".to_string());
        assert_eq!(three.row, 6);
        assert_eq!(three.column, 8);
    }
    #[test]
    fn test_field_expression() {
        let snip = r#"
        using X
        X.y
        X.y.x.x()
        Z.y
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Z".to_string());
        assert_eq!(one.row, 5);
        assert_eq!(one.column, 8);
    }
    #[test]
    fn test_macro_expression() {
        let snip = r#"
        macro x()
        end
        macro y_str()
        end
        y
        y"test"
        x
        @x 1
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 2);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 6);
        assert_eq!(one.column, 8);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 8);
        assert_eq!(two.column, 8);
    }
    #[test]
    fn test_function_object() {
        let snip = r#"
        (rx::Object)(x) = rx + x - z
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 2);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Object".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 13);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "z".to_string());
        assert_eq!(two.row, 2);
        assert_eq!(two.column, 35);
    }
    #[test]
    fn test_comprehension() {
        let snip = r#"
        [x+y for x in 1:100]
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 11);
    }
    #[test]
    fn test_export() {
        let snip = r#"
         export f, g
         f(x)=1
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "g".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 19);
    }
    #[test]
    fn test_tuple_assign() {
        let snip = r#"
        (x, _) = pair()
        j, _ = pair()
        x
        y
        j
        "#;
        let env: Vec<String> = vec!["pair".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 5);
        assert_eq!(one.column, 8);
    }
    #[test]
    fn test_where_exp() {
        let snip = r#"
        const T1 = Array{Array{T,N,Y} where T where N, 1}
        "#;
        let env: Vec<String> = vec!["Array".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Y".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 35);
    }
    #[test]
    fn test_where_clause() {
        let snip = r#"
        function store_events(fac, events::Vector{EVENT_TYPE}) where {EVENT_TYPE, X}
            fac
            EVENT_TYPE
            X
            z
        end
        EVENT_TYPE
        "#;
        let env: Vec<String> = vec!["Vector".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        println!("errs => {:?}", errs);
        assert_eq!(errs.len(), 2);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 6);
        assert_eq!(one.column, 12);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "EVENT_TYPE".to_string());
        assert_eq!(two.row, 8);
        assert_eq!(two.column, 8);
    }
    #[test]
    fn test_for_clause() {
        let snip = r#"
         local Dict
         Dict(string(k) => nothing for (k, v) in pairs(tab))
        "#;
        let env: Vec<String> = vec![
            "string".to_string(),
            "pairs".to_string(),
            "nothing".to_string(),
        ];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        println!("errs => {:?}", errs);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "tab".to_string());
        assert_eq!(one.row, 3);
        assert_eq!(one.column, 55);
    }
    #[test]
    fn test_for_bind() {
        let snip = r#"
        for x in 1:100
             x
             y
        end
        x
        for (x, y) in 1:100
             x
             y
        end
        y
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        println!("errs => {:?}", errs);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 13);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 6);
        assert_eq!(two.column, 8);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "y".to_string());
        assert_eq!(three.row, 11);
        assert_eq!(three.column, 8);
    }
}
