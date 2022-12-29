use quercus::{Grammar, Rule};

#[derive(Rule, Grammar)]
pub struct Lang {
    #[rule(string = "hello")]
    hello: String,
}
