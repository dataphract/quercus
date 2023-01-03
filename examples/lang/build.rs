use quercus::Grammar;

fn main() {
    quercus_generate::generate(&lang_grammar::Lang::grammar_dsl(), "language.rs");
}
