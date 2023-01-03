use quercus::Rule;
use tree_sitter as ts;

mod language {
    include!(concat!(env!("OUT_DIR"), "/language.rs"));
}

fn main() {
    let lang = language::language();
    let mut parser = ts::Parser::new();
    parser.set_language(lang).unwrap();
    let tree = parser.parse("hello", None).unwrap();
    let out = lang_grammar::Lang::from_node(&tree.root_node(), "hello");
    println!("{:?}", out);
}
