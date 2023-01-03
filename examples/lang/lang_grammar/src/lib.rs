use quercus::{Grammar, Rule};

#[derive(Debug, Rule, Grammar)]
pub struct Lang {
    #[rule(string = "hello")]
    hello: String,
}
