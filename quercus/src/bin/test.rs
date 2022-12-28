use quercus::{dsl, Grammar, GrammarBuilder, Rule};

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

fn main() {
    let grammar = Lisp::grammar_dsl();
    println!("{grammar:#?}");
}
