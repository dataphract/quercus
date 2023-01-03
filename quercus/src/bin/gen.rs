use quercus::{Grammar, Rule};

#[derive(Rule, Grammar)]
struct MyLang {
    #[rule(string = "hello")]
    hello: String,
}

fn main() {
    quercus_generate::generate(&MyLang::grammar_dsl(), "language.rs");
    //println!("{:#?}", MyLang::grammar_dsl());
}
