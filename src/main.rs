use tree_sitter::{Language, Parser, Node};

extern "C" {
    fn tree_sitter_julia() -> Language;
}

fn print_node(node: &tree_sitter::Node, src: &String) {
    println!("Kind: {}", node.kind());
    println!("has error: {}", node.has_error());
    println!("Child count: {}", node.named_child_count());
    let mut tc = node.walk();
    for (id, child) in node.named_children(&mut tc).enumerate(){
        println!("Child({}): Kind=> {}", id, child.kind());
        println!("Child({}): value=> {}", id, node_value(&child, src));
    }
}

fn row(node: &Node, src: &String) -> usize{
    let p =  node.start_position();
    return p.row;
}
fn col(node: &Node, src: &String) -> usize{
    let p =  node.start_position();
    return p.column;
}

fn node_value(node: &Node, src: &String) -> String{
    if let Ok(val) = node.utf8_text(src.as_bytes()) {
        return val.to_string()
    }
    return "".to_string()
}
fn analyse(node: &Node, src: &String, env: &Vec<String>) {
   if let Ok(val) = node.utf8_text(src.as_bytes()) {
        let sym = val.to_string();
        if !env.contains(&sym.to_string()) {
            println!("Undefined symbol {} found at {}:{}, ", sym, row(&node, src), col(&node, src));
        }
    }
}
fn eval(node: &Node, src: &String, env: &Vec<String>){
    match node.kind() {
        "source_file" => {
            let mut tc = node.walk();
            for child in node.named_children(&mut tc) {
                eval(&child, src, env);
            }
        },
        "assignment_expression" => {
            if let Some(rnode) = node.named_child(1) {
                eval(&rnode, src, env)
            }
        },
        "subscript_expression" => {
            print_node(node, src);
            if let Some(rnode) = node.named_child(1) {
                eval(&rnode, src, env)
            }
        },
        "parenthesized_expression" => {
            if let Some(rnode) = node.named_child(0) {
                eval(&rnode, src, env)
            }
        },
        "number" => (),
        "binary_expression" => {
            if let Some(firstnode) = node.named_child(0) {
                if firstnode.kind() == "identifier" {
                    analyse(&firstnode, src, env)
                }
                else {
                    eval(&firstnode, src, env)
                }
            }
            if let Some(secondnode) = node.named_child(1) {
                if secondnode.kind() == "identifier" {
                    analyse(&secondnode, src, env)
                }
                else {
                    eval(&secondnode, src, env);
                }
            }
        },
        _ => {
            println!("Unimplemented kind {}", node.kind());
        }
    };
}

fn main() {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_julia() };
    parser.set_language(language).unwrap();
    let source_code = "a[1][2]";
    let tree = parser.parse(source_code, None).unwrap();
    let root_node = tree.root_node();
    let env = Vec::<String>::new();
    eval(&root_node, &String::from(source_code), &env);
}
