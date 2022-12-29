use tree_sitter as ts;

extern "C" {
    fn tree_sitter_Lang() -> ts::Language;
}

fn main() {
    let lang = unsafe { tree_sitter_Lang() };
    let mut parser = ts::Parser::new();
    parser.set_language(lang).unwrap();
}
