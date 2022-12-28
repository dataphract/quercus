use quercus::{dsl, GrammarBuilder, Rule};

#[derive(Rule)]
#[rule(string = "{")]
struct LBrace;

#[derive(Rule)]
#[rule(string = "}")]
struct RBrace {}

#[derive(Rule)]
#[rule(string = "(")]
struct LParen();

#[derive(Rule)]
#[rule(string = ")")]
struct RParen;

#[derive(Rule)]
#[rule(string = ":")]
struct Colon;

#[derive(Rule)]
struct EmptyParens {
    left: LParen,
    right: RParen,
}

#[derive(Rule)]
struct IdentComma {
    ident: Ident,
    #[rule(string = ",")]
    comma: (),
}

#[derive(Rule)]
struct NonEmptyTuple {
    l_paren: LParen,
    #[rule(repeat)]
    idents: Vec<IdentComma>,
    last: Option<Ident>,
    r_paren: RParen,
}

enum IdentTuple {
    Empty(EmptyParens),
    NonEmpty(NonEmptyTuple),
}

#[test]
fn test_derive_named_struct_repeat_field() {
    #[derive(Rule)]
    struct Tuple {
        #[rule(string = "(")]
        l_paren: (),
        #[rule(string = ")")]
        r_paren: (),
    }
}

#[derive(Rule)]
struct Func {
    #[rule(string = "fn")]
    _fn: (),
    ident: Ident,
    args: FuncArgs,
    body: Block,
}

#[derive(Rule)]
struct FuncArgs {
    _l_paren: LParen,
    // #[rule(repeat)]
    // args: Vec<FuncArg>,
    _r_paren: RParen,
}

#[derive(Rule)]
struct FuncArg {
    ident: Ident,
    _colon: Colon,
}

#[derive(Rule)]
struct Ident {
    #[rule(pattern = "[a-zA-Z_][a-zA-Z0-9_]*")]
    ident: String,
}

#[derive(Rule)]
struct Type {
    ident: Ident,
}

#[derive(Rule)]
struct Block {
    _l_brace: LBrace,
    _r_brace: RBrace,
}

fn main() {
    let mut builder = GrammarBuilder::new("test");
    let test = Func::emit();

    println!("{}", serde_json::to_string_pretty(&test).unwrap());

    Func::register_dependencies(&mut builder);
    println!(
        "{}",
        serde_json::to_string_pretty(&builder.build()).unwrap()
    );
}
