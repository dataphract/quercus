extern crate proc_macro;

use proc_macro_error::{emit_error, proc_macro_error};
use syn::{parse_macro_input, Attribute, DeriveInput, Lit, LitStr, Meta, MetaNameValue};

mod grammar;
mod rule;

/// Derives `Rule` for a type.
///
/// This derive macro generates rules automatically based on the structure of the derived type and
/// usage of the `#[rule]` attribute. It mirrors the way rules are defined by Tree-sitter.
///
/// Note that the behavior of certain rules may change depending on settings of the top-level
/// grammar, like `extra` and `word` rules.
///
/// # Leaf rules
///
/// A _leaf rule_ is a rule that directly matches some text in the input, without any sub-rules. For
/// instance, when parsing Rust, leaf rules would be used to parse single characters, keywords,
/// identifiers, and so on.
///
/// Leaf rules can either consist of an exact string or a pattern (specified as regex). Leaf rules
/// can be specified on unit structs:
///
/// ```ignore
/// #[derive(Rule)]
/// #[rule(string = ";")]
/// struct Semicolon;
/// ```
///
/// Or on struct fields:
///
/// ```ignore
/// #[derive(Rule)]
/// struct Ident {
///     #[rule(pattern = "[a-zA-Z_][a-zA-Z0-9_]*")]
///     text: String,
/// }
/// ```
///
/// Or on enum variants:
///
/// (TODO)
///
/// ## String rules
///
/// The simplest type of leaf rule is the `string` rule, which exactly matches some input text.
///
/// ```ignore
/// // Matches the exact text `Hello, world!` in the input.
/// #[rule(string = "Hello, world!")]
/// struct Hello;
/// ```
///
/// ## Pattern rules
///
/// The `pattern` rule uses a regular expression to match some input text.
///
/// ```ignore
/// // Matches an ASCII alphabetic character or underscore, followed by any number of ASCII
/// // alphanumerics or underscores.
/// #[rule(pattern = "[a-zA-Z_][a-zA-Z0-9_]*")]
/// struct Ident;
/// ```
///
/// The pattern text is passed directly to Tree-sitter, meaning that the regex must be specified as
/// JavaScript regex.
///
/// ## Sequence rules
///
/// A sequence rule matches a series of other rules in sequence. To define a sequence rule,
/// `#[derive(Rule)]` on a struct with multiple fields.
///
/// ```ignore
/// // Matches the text `hello` followed by some punctuation (e.g. `hello.` or `hello!`).
/// #[derive(Rule)]
/// struct Hello {
///     #[rule(string = "hello")]
///     hello: (),
///     #[rule(pattern = "[.!?]")]
///     punct: (),
/// }
/// ```
///
#[proc_macro_derive(Rule, attributes(rule))]
#[proc_macro_error]
pub fn derive_rule(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // rule::derive_rule(parse_macro_input!(input as DeriveInput)).into()
    rule::derive_rule(parse_macro_input!(input as DeriveInput)).into()
}

/// Derives `Grammar` for a type.
///
/// The `Grammar` trait provides the necessary context to parse input according to a top-level rule.
/// In particular, this derive macro provides a `#[grammar]` attribute that modifies the behavior of
/// rules in the grammar.
#[proc_macro_derive(Grammar, attributes(grammar))]
#[proc_macro_error]
pub fn derive_grammar(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    grammar::derive_grammar(parse_macro_input!(input as DeriveInput)).into()
}

/// Extracts a `Meta::Path` from `meta`, or emits an error and returns `None`.
fn meta_path_only<T>(meta: &Meta, ident: &str, arg: T) -> Option<T> {
    match meta {
        Meta::Path(_) => Some(arg),
        _ => {
            emit_error!(meta, "expected only a path: `{}`", ident);
            None
        }
    }
}

/// Extracts a `Meta::NameValue` from `meta`, or emits an error and returns `None`.
fn meta_lit_str<T>(meta: &Meta, ident: &str, f: impl Fn(LitStr) -> T) -> Option<T> {
    match meta {
        Meta::NameValue(MetaNameValue {
            lit: Lit::Str(ls), ..
        }) => Some(f(ls.clone())),
        _ => {
            emit_error!(meta, "expected a key-value pair: `{} = \"...\"`", ident);
            None
        }
    }
}

/// Attempts to parse an attribute as a list of nested metas with the specified path.
///
/// If the attribute has the specified path, returns a list of successfully parsed arguments to the
/// attribute, and emits errors for each failed argument.
fn parse_nested_metas<F, T>(path: &str, attr: &Attribute, f: F) -> Option<Vec<(T, Meta)>>
where
    F: Fn(&syn::Meta) -> Option<T>,
{
    if !attr.path.is_ident(path) {
        return None;
    }

    let meta = match attr.parse_meta() {
        Ok(m) => m,

        Err(e) => {
            emit_error!(e.span(), "invalid `{}` attribute: {}", path, e);
            return None;
        }
    };

    let list = match meta {
        syn::Meta::List(l) => l,
        _ => {
            emit_error!(meta, "expected a list-like attribute: `#[{}(...)]`", path);
            return None;
        }
    };

    let mut parsed = Vec::new();

    for arg in list.nested.into_iter() {
        let m = match arg {
            syn::NestedMeta::Meta(m) => m,
            syn::NestedMeta::Lit(_) => {
                emit_error!(
                    arg,
                    "expected a meta item: `item`, `item = \"...\"` or `item(...)`"
                );
                continue;
            }
        };

        if let Some(arg) = f(&m) {
            parsed.push((arg, m));
        }
    }

    Some(parsed)
}
