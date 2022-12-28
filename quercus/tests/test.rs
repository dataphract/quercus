use quercus::{dsl, Grammar, GrammarBuilder, Rule};

fn rule_definition<T: Rule>(symbol: &str) -> dsl::Rule {
    let mut builder = GrammarBuilder::new("test");
    T::register_dependencies(&mut builder);
    builder
        .build()
        .rules
        .remove(symbol)
        .expect(&format!("no such rule: {symbol}"))
}

#[derive(Rule)]
#[rule(string = "{")]
struct LBrace;

#[test]
fn test_derive_unit_struct() {
    assert_eq!(LBrace::emit(), dsl::Rule::symbol("LBrace"));
    assert_eq!(rule_definition::<LBrace>("LBrace"), dsl::Rule::string("{"));
}

#[derive(Rule)]
#[rule(string = "}")]
struct RBrace {}

#[test]
fn test_derive_empty_named_struct() {
    assert_eq!(RBrace::emit(), dsl::Rule::symbol("RBrace"));
    assert_eq!(rule_definition::<RBrace>("RBrace"), dsl::Rule::string("}"));
}

#[derive(Rule)]
#[rule(string = "(")]
struct LParen();

#[test]
fn test_derive_empty_tuple_struct() {
    assert_eq!(LParen::emit(), dsl::Rule::symbol("LParen"));
    assert_eq!(rule_definition::<LParen>("LParen"), dsl::Rule::string("("));
}

#[derive(Rule)]
#[rule(string = ")")]
struct RParen;

#[derive(Rule)]
#[rule(string = ":")]
struct Colon;

#[derive(Rule)]
struct EmptyParens {
    _left: LParen,
    _right: RParen,
}

#[test]
fn test_derive_named_struct_symbol_fields() {
    assert_eq!(EmptyParens::emit(), dsl::Rule::symbol("EmptyParens"),);

    assert_eq!(
        rule_definition::<EmptyParens>("EmptyParens"),
        dsl::Rule::seq([
            dsl::Rule::field("_left", dsl::Rule::symbol("LParen")),
            dsl::Rule::field("_right", dsl::Rule::symbol("RParen")),
        ]),
    );
}

#[derive(Rule)]
struct Ident {
    #[rule(pattern = "[a-zA-Z_][a-zA-Z0-9_]*")]
    _text: String,
}

#[test]
fn test_derive_named_struct_leaf_field() {
    assert_eq!(Ident::emit(), dsl::Rule::symbol("Ident"));
    assert_eq!(
        rule_definition::<Ident>("Ident"),
        dsl::Rule::seq([dsl::Rule::field(
            "_text",
            dsl::Rule::pattern("[a-zA-Z_][a-zA-Z0-9_]*")
        )]),
    );
}

#[derive(Rule)]
#[rule(string = "pub")]
struct Pub;

#[derive(Rule)]
struct Vis {
    _vis: Option<Pub>,
}

#[test]
fn test_derive_named_struct_optional_field() {
    assert_eq!(Vis::emit(), dsl::Rule::symbol("Vis"));
    assert_eq!(
        rule_definition::<Vis>("Vis"),
        dsl::Rule::seq([dsl::Rule::field(
            "_vis",
            dsl::Rule::optional(dsl::Rule::symbol("Pub")),
        )]),
    );
}

#[derive(Rule)]
enum Greeting {
    #[rule(pattern = "[hH]i")]
    Hi,
    #[rule(pattern = "[hH]ello")]
    Hello,
}

#[test]
fn test_derive_enum_unit_variants() {
    assert_eq!(Greeting::emit(), dsl::Rule::symbol("Greeting"));
    assert_eq!(
        rule_definition::<Greeting>("Greeting"),
        dsl::Rule::choice([
            dsl::Rule::field("Hi", dsl::Rule::pattern("[hH]i")),
            dsl::Rule::field("Hello", dsl::Rule::pattern("[hH]ello")),
        ]),
    );
}

#[derive(Rule)]
#[rule(string = "\"")]
struct DoubleQuote;

#[derive(Rule)]
struct StrLit {
    lquote: DoubleQuote,
    #[rule(pattern = r#"[^"]*"#)]
    text: String,
    rquote: DoubleQuote,
}

#[derive(Rule)]
struct NumLit {
    #[rule(pattern = "[1-9][0-9]*")]
    digits: String,
}

#[derive(Rule)]
enum Lit {
    Str(StrLit),
    Num(NumLit),
}

#[test]
fn test_derive_enum_tuple_variants_single_field() {
    assert_eq!(Lit::emit(), dsl::Rule::symbol("Lit"));
    assert_eq!(
        rule_definition::<Lit>("Lit"),
        dsl::Rule::choice([
            dsl::Rule::field("Str", dsl::Rule::symbol("StrLit")),
            dsl::Rule::field("Num", dsl::Rule::symbol("NumLit")),
        ]),
    );
}

#[derive(Rule)]
enum Atom {
    Ident(Ident),
    Lit(Lit),
}

#[derive(Rule)]
struct List {
    lparen: LParen,
    #[rule(repeat)]
    items: Vec<Sexp>,
    rparen: RParen,
}

#[derive(Rule)]
enum Sexp {
    Atom(Atom),
    List(List),
}

#[derive(Rule, Grammar)]
struct Lisp {
    #[rule(repeat)]
    exprs: Vec<Sexp>,
}

#[test]
fn test_derive_grammar_lisp() {
    let grammar = Lisp::grammar_dsl();
    panic!("{grammar:#?}");
}
