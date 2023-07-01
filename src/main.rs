use colored::Colorize;
use std::fs;
//use std::rc::Rc;
use toml::Value;
use tree_sitter::{Language, Node, Parser};
//use std::io::Read;

extern "C" {
    fn tree_sitter_julia() -> Language;
}

#[derive(Debug)]
struct UndefVar {
    symbol: String,
    row: usize,
    column: usize,
}

#[derive(Debug)]
struct Module {
    name: String,
    symbols: Symbols,
    children: Vec<Module>,
}

#[derive(Debug)]
struct Symbols {
    exported: Vec<String>,
    toplevel: Vec<String>,
}

#[derive(Debug)]
struct Ctx {
    src_module_root: Option<Module>,
    current_module: String,
    loaded_modules: Vec<Module>,
    default_env: Vec<String>,
}

//fn read_file_contents(path: &str) -> Result<String, std::io::Error> {
//   println!("path {}", path);
//   // Open the file
//   let mut file = fs::File::open(path)?;
//
//   // Read the contents of the file into a string
//   let mut contents = String::new();
//   file.read_to_string(&mut contents)?;
//   Ok(contents)
//}

fn print_node(node: &tree_sitter::Node, src: &String) {
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

/*
fn include_toplevel(node: &Node, src: &String, env: &Vec<String>) -> Option<LintInfo> {
    if let Some(fileargs) = node.named_child(1) {
        if fileargs.kind() == "argument_list" {
            let mut tc = fileargs.walk();
            for filepath in fileargs.named_children(&mut tc) {
                let mut path = String::from("/Users/vdayanand/rs/lintia/");
                path.push_str(node_value(&filepath, src).as_str().trim_matches('"'));
                let src = load_jl_file(&path);
                return Some(lint(&src, env));
            }
        } else {
            print_node(&fileargs, src);
            unex(&fileargs);
        }
    }
    None
}*/

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
fn analyse(
    ctx: &Ctx,
    node: &Node,
    src: &String,
    env: &Vec<String>,
    idtype: &str,
) -> Option<UndefVar> {
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
    });
}

fn unex(node: &Node) {
    panic!("Unexpected kind {}", node.kind())
}
fn toplevel_symbol(node: &Node, src: &String) -> Vec<String> {
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
        "function_definition" | "short_function_definition" => {
            if let Some(fname) = node.named_child(0) {
                if fname.kind() == "identifier" {
                    syms.push(node_value(&fname, src));
                } else if fname.kind() == "field_expression" {
                    if let Some(second) = fname.named_child(1) {
                        syms.push(node_value(&second, src));
                    }
                } else if fname.kind() == "function_object" {
                } else {
                    print_node(&fname, src);
                    unex(&fname);
                }
            }
        }
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
        _ => (),
    }
    return syms;
}

fn scoped_eval(
    ctx: &mut Ctx,
    node: &Node,
    src: &String,
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
        if child.kind() == "call_expression" {
            if let Some(fname) = child.named_child(0) {
                if fname.kind() == "identifier" {
                    let fnameval = node_value(&fname, src);
                    if fnameval == "include" {
                        /*      if let Some(lintinfo) = include_toplevel(&child, src, &newenv) {
                            result.extend(lintinfo.undefvars);
                            newenv.extend(lintinfo.toplevel);
                        }*/
                    }
                }
            }
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

        if child.kind() == "for_binding" {
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
        result.extend(eval(ctx, &child, src, &newenv));
    }
}

fn eval(ctx: &mut Ctx, node: &Node, src: &String, env: &Vec<String>) -> Vec<UndefVar> {
    let mut result = Vec::<UndefVar>::new();
    //    print_node(&node, src);
    match node.kind() {
        "string_literal"
        | "boolean_literal"
        | "character_literal"
        | "integer_literal"
        | "abstract_definition"
        | "import_statement"
        | "continue_statement"
        | "line_comment"
        | "command_literal"
        | "quote_expression"
        | "macro_definition" => (),

        "function_definition"
        | "let_statement"
        | "for_statement"
        | "while_statement"
        | "short_function_definition"
        | "compound_statement"
        | "finally_clause" => scoped_eval(ctx, node, src, env, &mut result, 0),
        "variable_declaration" | "for_binding" => {
            if let Some(rhs) = node.named_child(1) {
                result.extend(eval(ctx, &rhs, src, env))
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
        "parenthesized_expression" | "global_declaration" | "slurp_parameter" => {
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
        "index_expression" | "vector_expression" | "argument_list" | "keyword_parameters" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() != "operator" {
                    result.extend(eval(ctx, &child, src, &env));
                }
            }
        }
        "call_expression" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() != "operator" {
                    result.extend(eval(ctx, &child, src, &env));
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
            if let Some(rnode) = node.named_child(1) {
                result.extend(eval(ctx, &rnode, src, env));
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
        "curly_expression" | "type_clause" => {
            if let Some(typenode) = node.named_child(0) {
                result.extend(eval(ctx, &typenode, src, &env));
            }
        }
        "elseif_clause" => scoped_eval(ctx, &node, src, env, &mut result, 0),
        "else_clause" => scoped_eval(ctx, &node, src, env, &mut result, 0),

        "if_statement" => scoped_eval(ctx, &node, src, env, &mut result, 0),
        _ => {
            print_node(&node.parent().unwrap(), src);
            panic!("Unimplemented kind {}", node.kind());
        }
    }
    return result;
}
/*
fn lint_ex(src: &str, env: &Vec<String>) -> Vec<UndefVar> {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let root_node = tree.root_node();
    let mut result: Vec<UndefVar> = vec![];
    scoped_eval(&root_node, &String::from(src), &env, &mut result, 0);
    return result;
}
*/
fn construct_module_tree(mut current_mod: Module, root_node: &Node, src: &String) -> Module {
    let mut tc = root_node.walk();
    let mut symbols = Symbols {
        exported: vec![],
        toplevel: current_mod.symbols.toplevel.to_vec(),
    };
    for child in root_node.named_children(&mut tc) {
        let mut tc = child.walk();
        let toplevel = toplevel_symbol(&child, src);
        symbols.toplevel.extend_from_slice(&toplevel);
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
    }
    return current_mod;
}

fn lint(ctx: &mut Ctx, src: &str, env: &Vec<String>) -> Vec<UndefVar> {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(src, None).unwrap();
    let root_node = tree.root_node();
    let mut tc = root_node.walk();
    let mut result: Vec<UndefVar> = vec![];
    let mut default_module = Module {
        name: "Main".to_string(),
        symbols: Symbols {
            exported: vec![],
            toplevel: vec!["Main".to_string()],
        },
        children: vec![],
    };
    let modtree = construct_module_tree(default_module, &root_node, &String::from(src));
    ctx.src_module_root = Some(modtree);
    ctx.current_module = "Main".to_string();
    scoped_eval(ctx, &root_node, &String::from(src), env, &mut result, 0);
    return result;
}

fn load_env(dir: &str) -> Vec<String> {
    // Read the directory
    let entries = fs::read_dir(dir).expect("Failed to read directory");
    let mut envs_list = Vec::<String>::new();
    // Iterate over the directory entries
    for entry in entries {
        if let Ok(entry) = entry {
            let path = entry.path();
            if let Some(extension) = path.extension() {
                if extension == "toml" {
                    if let Ok(contents) = fs::read_to_string(&path) {
                        let parsed_toml: Value =
                            toml::from_str(&contents).expect("Failed to parse TOML");
                        if let Some(envs) = parsed_toml.get("envs") {
                            if let Some(envs_array) = envs.as_array() {
                                let envs_vec: Vec<String> = envs_array
                                    .iter()
                                    .filter_map(|env_value| env_value.as_str().map(String::from))
                                    .collect();
                                envs_list.extend_from_slice(&envs_vec);
                            }
                        }
                    }
                }
            }
        }
    }
    return envs_list;
}

fn load_jl_file(file: &str) -> String {
    let jl_str = fs::read_to_string(file).expect("Failed to read file");
    return jl_str;
}

fn main() {
    let src = load_jl_file("test.jl");
    let env = load_env("src/pkgs");
    let mut ctx = Ctx {
        src_module_root: None,
        current_module: "".to_string(),
        loaded_modules: vec![],
        default_env: env,
    };
    let localenv = Vec::<String>::new();
    let linfo = lint(&mut ctx, &src, &localenv);
    for err in linfo {
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };

        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);

        let one = errs.remove(0);
        assert_eq!(errs.len(), 0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 9);
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };

        let mut errs = lint(&mut ctx, &source_code, &env);

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
         module Test
         function hello(x)
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
        let errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 0);
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 3);
        assert_eq!(one.column, 12);
    }
    #[test]
    fn test_oneline_func() {
        let source_code = r#"
        f(x, y) = f(x, y-1)+1
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 0);
    }
    #[test]
    fn test_assign_subscript() {
        let source_code = r#"
           RG[]["ds"] = "d"
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        println!("errs {:?}", errs);
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
           Y::Int
           Z
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
        let mut errs = lint(&mut ctx, &source_code, &env);
        println!("{:?}", errs);
        assert_eq!(errs.len(), 2);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Int".to_string());
        assert_eq!(one.row, 3);
        assert_eq!(one.column, 14);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "Animal2".to_string());
        assert_eq!(two.row, 6);
        assert_eq!(two.column, 25);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
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
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);

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
    #[test]
    fn test_const_declaration() {
        let source_code = r#"
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
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 3);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 3);
        assert_eq!(one.column, 8);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "Int".to_string());
        assert_eq!(two.row, 4);
        assert_eq!(two.column, 17);
        let three = errs.remove(0);
        assert_eq!(three.symbol, "k".to_string());
        assert_eq!(three.row, 5);
        assert_eq!(three.column, 8);
    }
    #[test]
    fn test_field_expression() {
        let source_code = r#"
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
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Z".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 8);
    }
    #[test]
    fn test_macro_expression() {
        let source_code = r#"
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
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 2);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 5);
        assert_eq!(one.column, 8);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "x".to_string());
        assert_eq!(two.row, 7);
        assert_eq!(two.column, 8);
    }
    #[test]
    fn test_function_object() {
        let source_code = r#"
        (rx::Object)(x) = rx + x - z
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 2);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "Object".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 13);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "z".to_string());
        assert_eq!(two.row, 1);
        assert_eq!(two.column, 35);
    }
    #[test]
    fn test_comprehension() {
        let source_code = r#"
        [x+y for x in 1:100]
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: vec![],
            default_env: vec![],
        };
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 11);
    }
    #[test]
    fn test_export() {
        let source_code = r#"
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
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "g".to_string());
        assert_eq!(one.row, 1);
        assert_eq!(one.column, 19);
    }
    #[test]
    fn test_tuple_assign() {
        let source_code = r#"
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
        let mut errs = lint(&mut ctx, &source_code, &env);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "y".to_string());
        assert_eq!(one.row, 4);
        assert_eq!(one.column, 8);
    }
}
