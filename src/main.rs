use byteorder::{ByteOrder, LittleEndian};
use clap::Parser;
use colored::Colorize;
use crc32c::{crc32c, crc32c_append};
use hex;
use serde::{Deserialize, Serialize};
use serde_json;
use shellexpand::tilde;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use toml::Value;
use tree_sitter::{Language, Node, Tree};
use uuid::Uuid;

extern "C" {
    fn tree_sitter_julia() -> Language;
}

const SLUG_CHARS: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
fn reverse_string(input: &str) -> String {
    let mut chars: Vec<char> = input.chars().collect();
    chars.reverse();
    let reversed: String = chars.into_iter().collect();
    reversed
}

fn slug(x: u32, p: i32) -> String {
    let mut y: u32 = x;
    let slug_chars: Vec<char> = SLUG_CHARS.chars().collect();
    let n = slug_chars.len();
    let mut result = String::with_capacity(p as usize);
    for _ in 0..p {
        let d = (y % n as u32) as usize;
        y /= n as u32;
        result.insert(0, slug_chars[d]);
    }
    reverse_string(result.as_str())
}

fn string_to_bytes(input: &str) -> Vec<u8> {
    hex::decode(input).unwrap()
}

fn crc32c_uuidhash(uuid: Uuid, sha: &str) -> u32 {
    let uuid_value: u128 = uuid.as_u128();
    let mut bytes: [u8; 16] = [0; 16];
    LittleEndian::write_u128(&mut bytes, uuid_value);
    let crcuuid = crc32c(&bytes);
    let bytes = string_to_bytes(sha);
    return crc32c_append(crcuuid, &bytes);
}

fn version_slug(uuid: &str, sha: &str) -> String {
    let uuidobj = Uuid::parse_str(uuid).unwrap();
    let hash = crc32c_uuidhash(uuidobj, sha);
    return slug(hash, 5);
}

#[derive(Debug)]
struct UndefVar {
    symbol: String,
    row: usize,
    column: usize,
    filepath: String,
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
    return p.row + 1;
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
        filepath: src.src_path.clone(),
    });
}

fn unex(node: &Node) {
    println!("Unexpected kind {}", node.kind())
}

fn syms_assignment(child: &Node, src: &Src, syms: &mut Vec<String>) {
    if let Some(lhs) = child.named_child(0) {
        if lhs.kind() == "identifier" {
            syms.push(node_value(&lhs, src));
        } else if lhs.kind() == "typed_expression" || lhs.kind() == "typed_parameter" {
            if let Some(lhstyped) = lhs.named_child(0) {
                syms.push(node_value(&lhstyped, src));
            }
        } else if lhs.kind() == "tuple_expression" || lhs.kind() == "bare_tuple" {
            let mut tc = lhs.walk();
            for val in lhs.named_children(&mut tc) {
                syms.push(node_value(&val, src));
            }
        }
    }
    if let Some(rhs) = child.named_child(2) {
        if rhs.kind() == "assignment" {
            syms_assignment(&rhs, src, syms)
        }
    }
}
fn syms_function(node: &Node, src: &Src, syms: &mut Vec<String>) {
    if let Some(fname) = node.named_child(0) {
        if fname.kind() == "identifier" || fname.kind() == "operator" {
            syms.push(node_value(&fname, src));
        } else if fname.kind() == "field_expression" {
            if let Some(second) = fname.named_child(1) {
                syms.push(node_value(&second, src));
            }
        } else if fname.kind() == "function_object"
            || fname.kind() == "parameter_list"
            || fname.kind() == "parameter_list"
        {
        } else if fname.kind() == "ERROR" {
            //print_syntax_error(&fname, src);
        } else {
            print_node(&fname, src);
            unex(&fname);
        }
    }
}

fn module_height(module: &Module, current_module: &String) -> Option<i32> {
    let main: Vec<&str> = module.name.split(".").collect();
    let current: Vec<&str> = current_module.split(".").collect();
    if main[0] != current[0] {
        return None;
    }
    let mut count = 0;
    for element in current.iter() {
        if !main.contains(element) {
            count += 1;
        }
    }
    return Some(count);
}

fn module_no(name: &String) -> (String, i32) {
    let mut count = 0;
    let mut newname: Vec<char> = vec![];
    for c in name.chars() {
        if c == '.' {
            count += 1;
        } else {
            newname.push(c);
        }
    }
    return (newname.iter().collect(), count);
}

fn exported(
    src_module: &Module,
    parent_name: &String,
    module_name: &String,
    acc: &String,
) -> Vec<String> {
    if acc == parent_name {
        for child in src_module.children.iter() {
            if child.name == *module_name {
                return child.symbols.exported.clone();
            }
        }
        return vec![];
    }
    for child in src_module.children.iter() {
        let newmod = format!("{}.{}", acc, child.name);
        let exported_syms = exported(child, parent_name, module_name, &newmod);
        if exported_syms.len() != 0 {
            return exported_syms;
        }
    }
    return vec![];
}

fn get_exported_symbols(ctx: &Ctx, module_name: &String) -> Vec<String> {
    for (name, moduleobj) in ctx.loaded_modules.iter() {
        if name == module_name {
            return moduleobj.symbols.exported.clone();
        }
    }
    if let Some(first_character) = module_name.chars().next() {
        if first_character != '.' {
            return vec![];
        }
    }
    if ctx.src_module_root.is_none() {
        return vec![];
    }
    let src_module = &ctx.src_module_root;
    let (module_name_real, rel) = module_no(module_name);
    let src_mod = src_module.as_ref().unwrap();
    if let Some(module_height) = module_height(src_mod, &ctx.current_module) {
        let mod_parts: Vec<String> = ctx
            .current_module
            .split(".")
            .map(|s| s.to_string())
            .collect();
        let parent_height: i32 = if rel == 1 {
            module_height
        } else {
            module_height - rel
        };
        if 0 <= parent_height && parent_height < mod_parts.len().try_into().unwrap() {
            let ancestor_name = mod_parts[0..(parent_height as usize) + 1]
                .to_vec()
                .join(".");
            return exported(src_mod, &ancestor_name, &module_name_real, &src_mod.name);
        }
        return vec![];
    }
    panic!("Unexpected!!");
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
                    unex(&node)
                }
            }
        }
        "call_expression" => {
            if let Some(parseinfo) = include_node(&node, src) {
                let tree = parseinfo.tree;
                let newsrc = parseinfo.src;
                let root = tree.root_node();
                change_pwd(&PathBuf::from(&newsrc.src_path));
                let mut tc = root.walk();
                for child in root.named_children(&mut tc) {
                    let ss = toplevel_symbol(&child, &newsrc);
                    syms.extend_from_slice(&ss);
                }
                change_pwd(&PathBuf::from(&src.src_path));
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
        "macrocall_expression" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                if child.kind() == "macro_argument_list" {
                    if let Some(arg) = child.named_child(0) {
                        if arg.kind() == "function_definition" || arg.kind() == "struct_definition"
                        {
                            if let Some(id) = arg.named_child(0) {
                                if id.kind() == "identifier" {
                                    syms.push(node_value(&id, src));
                                }
                            }
                        }
                    }
                }
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
        if child.kind() == "abstract_definition" || child.kind() == "struct_definition" || child.kind() == "module_definition"  {
            if let Some(lhs) = child.named_child(0) {
                if lhs.kind() == "identifier" {
                    newenv.push(node_value(&lhs, src));
                }
            }
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
                            unex(&lhs);
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
                                    unex(&lhs);
                                }
                            }
                        } else if x.kind() == "slurp_parameter" {
                            if let Some(param) = x.named_child(0) {
                                newenv.push(node_value(&param, src));
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
                } else if param.kind() == "macrocall_expression" {
                } else {
                    print_node(&param, src);
                    unex(&param);
                }
            }
        }
        if child.kind() == "import_statement" {
            if let Some(nmodule) = child.named_child(0) {
                if nmodule.kind() == "identifier" || nmodule.kind() == "relative_qualifier" {
                    let module_name = node_value(&nmodule, src);
                    let mut exported = get_exported_symbols(&ctx, &module_name);
                    let (module_name_real, _) = module_no(&module_name);
                    exported.push(module_name_real);
                    newenv.extend_from_slice(&exported);
                }
            }
        }
        if child.kind() == "assignment" {
            syms_assignment(&child, src, &mut newenv)
        }
        if child.kind() == "typed_parameter" || child.kind() == "optional_parameter" {
            if let Some(lhs) = child.named_child(0) {
                if lhs.kind() == "identifier" {
                    newenv.push(node_value(&lhs, src));
                } else if lhs.kind() == "typed_expression" || lhs.kind() == "typed_parameter" {
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
            scoped_eval(ctx, &node, src, env, &mut result, 0);
        }
        "parenthesized_expression"
        | "global_declaration"
        | "slurp_parameter"
        | "interpolation_expression" => {
            if let Some(rnode) = node.named_child(0) {
                result.extend(eval(ctx, &rnode, src, env));
            }
        }
        "macro_identifier" => {
            if let Some(name) = node.named_child(0) {
                if let Some(id) = name.named_child(0) {
                    if let Some(failed) = analyse(&ctx, &id, src, env, "macro") {
                        result.push(failed);
                    }
                }
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
                } else if field.kind() == "type_clause"
                    || field.kind() == "let_statement"
                    || field.kind() == "global_declaration"
                {
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
                } else if field.kind() == "line_comment"
                    || field.kind() == "block_comment"
                    || field.kind() == "macrocall_expression"
                {
                } else if field.kind() == "const_declaration" {
                    if let Some(typed) = field.named_child(1) {
                        result.extend(eval(ctx, &typed, src, &env));
                    }
                } else {
                    print_node(&field, src);
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
            let current_module = ctx.current_module.clone();
            if let Some(name) = node.named_child(0) {
                ctx.current_module = format!("{}.{}", ctx.current_module, node_value(&name, src));
            } else {
                unex(node);
            }
            scoped_eval(ctx, &node, src, &vec![], &mut result, 1);
            ctx.current_module = current_module.to_string();
        }
        "identifier" | "operator" => {
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
                change_pwd(&PathBuf::from(&newsrc.src_path));
                scoped_eval(ctx, &root, &newsrc, env, &mut result, 0);
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
            //print_syntax_error(node, src);
        }
        _ => {
            print_node(&node.parent().unwrap(), src);
            println!("Unimplemented kind {}", node.kind());
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
fn joinpath(base: PathBuf, path: PathBuf) -> PathBuf {
    let mut joined = base;
    joined.push(path);
    joined
}

fn parse_file_args_call(callnode: &Node, src: &Src) -> PathBuf {
    if let Some(name) = callnode.named_child(0) {
        if node_value(&name, src) != "joinpath" {
            println!("Unknown include function")
        }
    }
    if let Some(args) = callnode.named_child(1) {
        let mut tc = args.walk();
        let mut path = PathBuf::from("./");
        for name in args.named_children(&mut tc) {
            let mut filename = node_value(&name, src).trim_matches('"').to_string();
            filename = if filename == "@__DIR__".to_string() {
                "./".to_string()
            } else {
                filename
            };
            path = joinpath(path, PathBuf::from(filename))
        }
        println!("path => {:?}", path);
        return path;
    }
    panic!("Unexpected call node")
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
                            for filenode in fileargs.named_children(&mut tc) {
                                let filepath = if filenode.kind() == "call_expression" {
                                    PathBuf::from(parse_file_args_call(&filenode, src))
                                } else {
                                    PathBuf::from(node_value(&filenode, src).trim_matches('"'))
                                };
                                let fullpath =
                                    if let Ok(absolute_path) = fs::canonicalize(&filepath) {
                                        absolute_path
                                    } else {
                                        panic!(
                                            "Failed to get the absolute path {:?} cwd={:?}",
                                            filepath,
                                            current_pwd()
                                        );
                                    };
                                let path = fullpath.to_string_lossy().into_owned();
                                let newsrc = load_jl_file(&fullpath);
                                let src_obj = Src {
                                    src_str: newsrc,
                                    src_path: path,
                                };
                                let tree = parse_node(&src_obj);
                                return Some(ParseInfo {
                                    tree: tree,
                                    src: src_obj,
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
                if expsym.kind() == "identifier" || expsym.kind() == "macro_identifier" {
                    symbols.exported.push(node_value(&expsym, src));
                } else if expsym.kind() == "line_comment" || expsym.kind() == "operator"{
                } else {
                    print_node(&expsym, src);
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
    let mut parser = tree_sitter::Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let tree = parser.parse(&src.src_str, None).unwrap();
    return tree;
}

fn module_from_file(modname: &str, file: &PathBuf) -> Module {
    let content = load_jl_file(file);
    let src = Src {
        src_str: content,
        src_path: file.to_string_lossy().into_owned(),
    };
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
            return construct_module_tree(main_module, &modulenode, &src);
        } else {
            println!("modulenode.kind() => {:?}", modulenode.kind());
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
fn read_file(file_path: &Path) -> io::Result<String> {
    let mut file = fs::File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn load_package(ctx: &mut Ctx, name: &String, path: &String) {
    let current_dir = env::current_dir().unwrap();
    change_pwd(&PathBuf::from(&path));
    let cachefile = format!("/Users/vdayanand/.lintia/{}.json", name);
    println!("loading package {:?} at {:?}", name, path);
    if let Err(_) = add_deps(ctx, name) {
        let module = module_from_file(name, &PathBuf::from(path));
        let json = serde_json::to_string(&module).unwrap();
        _ = write_file(
            &PathBuf::from(cachefile),
            &json,
            );

        ctx.loaded_modules.insert(name.to_string(), module);
    }
    println!("loaded package {:?} at {:?}", name, path);
    change_pwd_dir(&current_dir);
}

fn load_project(ctx: &mut Ctx, path: &PathBuf) {

    let projectfile = path.join(PathBuf::from("Project.toml"));
    let manifestfile = path.join(PathBuf::from("Manifest.toml"));
    // Read the TOML file content
    let contents = fs::read_to_string(projectfile).expect("Failed to read the file.");
    let mut packages: Vec<String> = vec![];
    // Parse the TOML content into a TOML value
    let toml_value: Value = contents.parse().expect("Failed to parse the TOML content.");
    if let Some(deps) = toml_value.get("deps") {
        if let Some(table) = deps.as_table() {
            for key in table.keys() {
                packages.push(key.to_string())
            }
        }
    }
    let contents = fs::read_to_string(manifestfile).expect("Failed to read the file.");
    let toml_value: Value = contents.parse().expect("Failed to parse the TOML content.");
    if let Some(table) = toml_value.as_table() {
        for (name, element) in table {
            if !packages.contains(&name) {
                continue;
            }
            if let Some(elements) = element.as_array() {
                for elem in elements {
                    if let Some(uuid) = elem.get("uuid") {
                        if let Some(git_tree_sha1) = elem.get("git-tree-sha1") {
                            let uuid = uuid.as_str().unwrap();
                            let gittreesha1 = git_tree_sha1.as_str().unwrap();
                            let slug = version_slug(uuid, gittreesha1);
                            let deps_path = format!(
                                "/Users/vdayanand/.julia/packages/{}/{}/src/{}.jl",
                                name, slug, name
                            );
                            load_package(ctx, &name, &deps_path);
                        } else
                        if let Some(pkgpath) = elem.get("path") {
                            let pkgpath = pkgpath.as_str().unwrap();
                            let cwd = current_pwd();
                            change_pwd_dir(&path);
                            let file = if let Ok(absolutepath) = fs::canonicalize(&pkgpath) {
                                absolutepath
                            } else {
                                panic!(
                                    "Failed to get the absolute path {:?} cwd: {:?}",
                                    pkgpath,
                                    current_pwd()
                                );
                            };
                            change_pwd_dir(&PathBuf::from(cwd));
                            let deps_path = format!(
                                "{}/src/{}.jl",
                                file.to_string_lossy().to_string(), name
                            );
                            load_package(ctx, &name, &deps_path);
                        }
                        else {
                            if let Err(_) = add_deps(ctx, &name) {
                                panic!("failed to load deps")
                            }
                        }
                    }
                }
            }
        }
    }
}

fn add_deps(ctx: &mut Ctx, name: &String) -> io::Result<()> {
    let file_path =
        Path::new("/Users/vdayanand/.lintia").join(PathBuf::from(format!("{}.json", name)));
    match read_file(&file_path) {
        Ok(contents) => {
            if let Ok(module) = serde_json::from_str::<Module>(&contents) {
                ctx.loaded_modules.insert(module.name.clone(), module);
            }
        }
        Err(error) => eprintln!("Failed to read file: {} {:?}", name, error),
    }
    Ok(())
}

fn lint(ctx: &mut Ctx, src: &Src, env: &Vec<String>, project: Option<PathBuf>) -> Vec<UndefVar> {
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
    if let Some(pj) = project {
        load_project(ctx, &pj);
    }
    /*
    let stdlibs = load_packages_dir(&PathBuf::from("/Users/vdayanand/code/julia/stdlib/"));
    for (name, path) in stdlibs {
        if  name == "InteractiveUtils" || name == "LinearAlgebra" {
            load_package(ctx, &name, &joinpath(path, PathBuf::from(format!("src/{}.jl", name))).to_string_lossy().to_string())
        }
    }*/
    //    ctx.loaded_modules.insert("UUIDs".to_string(), uuidmod);
    //    let _ = add_deps(ctx);
    scoped_eval(ctx, &root_node, src, env, &mut result, 0);
    return result;
}

fn load_packages_dir(dir_path: &Path) -> Vec<(String, PathBuf)> {
    let mut subdirectories = Vec::new();
    if let Ok(entries) = fs::read_dir(dir_path) {
        for entry in entries.filter_map(Result::ok) {
            let path = entry.path();
            if path.is_dir() {
                let dir_name = match path.file_name() {
                    Some(name) => name.to_string_lossy().to_string(),
                    None => continue,
                };
                subdirectories.push((dir_name, path));
            }
        }
    }
    subdirectories
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
    env::set_current_dir(newpwd).expect("Failed to change directory");
}
fn change_pwd_dir(path: &PathBuf) {
    env::set_current_dir(path).expect("Failed to change directory");
}

fn current_pwd() -> PathBuf {
    env::current_dir().unwrap()
}

/// A linter for Julia programming language
#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct LintiaArgs {
    /// Path to the source file
    #[arg(value_parser(clap::value_parser!(PathBuf)), value_hint(clap::ValueHint::FilePath))]
    sourcefile: PathBuf,

    /// Path to the project
    #[arg(short, long, value_hint(clap::ValueHint::FilePath))]
    project: Option<String>,
}

fn main() {
    let args = LintiaArgs::parse();
    println!("file => {:?}", args.sourcefile);
    println!("project => {:?}", args.project);
    if let Ok(current_dir) = env::current_dir() {
        println!("Current working directory: {:?}", current_dir);
    } else {
        eprintln!("Failed to get the current working directory");
    }

    let file = if let Ok(absolutepath) = args.sourcefile.canonicalize() {
        absolutepath
    } else {
        panic!(
            "Failed to get the absolute path {:?} cwd: {:?}",
            args.sourcefile,
            current_pwd()
        );
    };

    let projectfullpath = if let Some(project) = &args.project {
        let project = tilde(&project).into_owned();
        if let Ok(absolute_path) = fs::canonicalize(&project) {
            Some(absolute_path)
        } else {
            panic!(
                "Failed to get the absolute path {:?} cwd: {:?}",
                project,
                current_pwd()
            );
        }
    } else {
        None
    };
    let src = load_jl_file(&file);
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
    let linfo = lint(&mut ctx, &src, &localenv, projectfullpath);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };

        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);

        let one = errs.remove(0);
        assert_eq!(errs.len(), 0);
        assert_eq!(one.symbol, "z".to_string());
        assert_eq!(one.row, 2);
        assert_eq!(one.column, 9);
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);

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
         y = 1
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let errs = lint(&mut ctx, &source_code, &env, None);
        println!("errs => {:?}", errs);
        assert_eq!(errs.len(), 0);

        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let errs = lint(&mut ctx, &source_code, &env, None);
        assert_eq!(errs.len(), 0);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
        Base.@kwdef struct Typee <: Animal
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
        let env: Vec<String> = vec!["Base".to_string()];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);

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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
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
    #[test]
    fn test_relative_import() {
        let snip = r#"
        module A
           t(x)=1
           m(x)=1
           export t
           module B
              import ..A
              export x
              x(y)=1
              t
              m
           end
           import .B
           x
           y
        end
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
        println!("errs => {:?}", errs);
        assert_eq!(errs.len(), 2);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "m".to_string());
        assert_eq!(one.row, 11);
        assert_eq!(one.column, 14);
        let two = errs.remove(0);
        assert_eq!(two.symbol, "y".to_string());
        assert_eq!(two.row, 15);
        assert_eq!(two.column, 11);
    }
    #[test]
    fn test_def_with_macrocall() {
        let snip = r#"
        macro t()
        end
        @t struct A
        end
        A()
        @t function def()
        end
        def()
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let errs = lint(&mut ctx, &source_code, &env, None);
        println!("errs => {:?}", errs);
        assert_eq!(errs.len(), 0);
    }
    #[test]
    fn test_mul_declare() {
        let snip = r#"
        a = b = 1
        a
        b
        c
        "#;
        let env: Vec<String> = vec![];
        let mut ctx = Ctx {
            src_module_root: None,
            current_module: "".to_string(),
            loaded_modules: HashMap::new(),
            default_env: vec![],
        };
        let source_code = Src {
            src_str: snip.to_string(),
            src_path: "<repl>".to_string(),
        };
        let mut errs = lint(&mut ctx, &source_code, &env, None);
        println!("errs => {:?}", errs);
        assert_eq!(errs.len(), 1);
        let one = errs.remove(0);
        assert_eq!(one.symbol, "c".to_string());
        assert_eq!(one.row, 5);
        assert_eq!(one.column, 8);
    }
}
