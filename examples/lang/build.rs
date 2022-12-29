use quercus::Grammar;

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    quercus_generate::generate(&lang_grammar::Lang::grammar_dsl(), &out_dir);
}
