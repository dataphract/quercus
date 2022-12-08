use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_error::{abort, emit_error, proc_macro_error};
use quote::quote_spanned;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, DeriveInput, FieldsNamed, Lit, LitStr, Meta,
    MetaNameValue, NestedMeta, TypePath, TypeTuple,
};

use quercus_dsl as dsl;

extern crate proc_macro;

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
    let DeriveInput {
        attrs,
        vis: _,
        ident,
        generics,
        data,
    } = parse_macro_input!(input as DeriveInput);

    match data {
        syn::Data::Struct(ds) => match &ds.fields {
            syn::Fields::Unit => generate_rule_impl(&ident, derive_leaf_rule(&ident, attrs)),

            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.is_empty() {
                    generate_rule_impl(&ident, derive_leaf_rule(&ident, attrs))
                } else {
                    abort!(
                        ds.fields,
                        "#[derive(Rule)] is not supported for non-empty tuple structs"
                    )
                }
            }

            syn::Fields::Named(fields) => {
                if fields.named.is_empty() {
                    generate_rule_impl(&ident, derive_leaf_rule(&ident, attrs))
                } else {
                    generate_rule_impl(&ident, derive_seq_rule(&ident, attrs, fields.clone()))
                }
            }
        },

        syn::Data::Enum(de) => todo!("enum"),

        syn::Data::Union(du) => abort!(
            du.union_token,
            "#[derive(Rule)] is not supported for `union`"
        ),
    }
    .into()
}

fn generate_rule_impl(ident: &Ident, emit_impl: TokenStream) -> TokenStream {
    quote_spanned! { ident.span() =>
        impl Rule for #ident {
            fn emit() -> quercus::dsl::Rule {
                #emit_impl
            }
        }
    }
}

/// Given a `Rule`, create the `TokenStream` that evaluates to it.
fn rule_to_token_stream(span: Span, rule: &dsl::Rule) -> TokenStream {
    match rule {
        dsl::Rule::Blank => quote_spanned! { span => quercus::dsl::Rule::Blank },
        dsl::Rule::String(dsl::StringRule { value }) => {
            quote_spanned! { span => quercus::dsl::Rule::string(#value.into()) }
        }
        dsl::Rule::Pattern(dsl::PatternRule { value }) => {
            quote_spanned! { span => quercus::dsl::Rule::pattern(#value.into()) }
        }
        dsl::Rule::Symbol(dsl::SymbolRule { name }) => {
            quote_spanned! { span => quercus::dsl::Rule::symbol(#name.into()) }
        }
        dsl::Rule::Seq(dsl::SeqRule { members }) => {
            let member_tokens = members
                .iter()
                .map(|m| rule_to_token_stream(span, m))
                .collect::<Vec<_>>();
            quote_spanned! { span =>
                quercus::dsl::Rule::seq(
                    core::iter::empty()
                        #(.chain(#member_tokens))*
                )
            }
        }
        dsl::Rule::Choice(dsl::ChoiceRule { members }) => {
            let member_tokens = members
                .iter()
                .map(|m| rule_to_token_stream(span, m))
                .collect::<Vec<_>>();
            quote_spanned! { span =>
                quercus::dsl::Rule::choice(
                    core::iter::empty()
                        #(.chain(#member_tokens))*
                )
            }
        }

        dsl::Rule::Repeat(dsl::RepeatRule { content }) => {
            let content_tokens = rule_to_token_stream(span, &content);
            quote_spanned! { span => quercus::dsl::Rule::repeat(#content_tokens) }
        }

        dsl::Rule::Repeat1(dsl::Repeat1Rule { content }) => {
            let content_tokens = rule_to_token_stream(span, &content);
            quote_spanned! { span => quercus::dsl::Rule::repeat1(#content_tokens) }
        }

        dsl::Rule::Token(dsl::TokenRule { content }) => {
            let content_tokens = rule_to_token_stream(span, &content);
            quote_spanned! { span => quercus::dsl::Rule::token(#content_tokens) }
        }

        dsl::Rule::ImmediateToken(dsl::ImmediateTokenRule { content }) => {
            let content_tokens = rule_to_token_stream(span, &content);
            quote_spanned! { span => quercus::dsl::Rule::immediate_token(#content_tokens) }
        }

        dsl::Rule::Alias(dsl::AliasRule {
            value,
            named,
            content,
        }) => {
            let content_tokens = rule_to_token_stream(span, &content);

            if *named {
                quote_spanned! { span => quercus::dsl::Rule::named_alias(#value, #content_tokens) }
            } else {
                quote_spanned! { span => quercus::dsl::Rule::anonymous_alias(#value, #content_tokens) }
            }
        }

        dsl::Rule::Field(dsl::FieldRule { name, rule }) => {
            let rule_tokens = rule_to_token_stream(span, &rule);
            let name = name.clone();

            quote_spanned! { span => quercus::dsl::Rule::field(#name, #rule_tokens) }
        }
    }
}

enum RuleAttrArg {
    String(LitStr),
    Pattern(LitStr),
    Repeat,
    Repeat1,
}

impl RuleAttrArg {
    fn from_meta(meta: &Meta) -> Option<RuleAttrArg> {
        let ident = match meta.path().get_ident() {
            Some(i) => i.to_string(),
            None => {
                emit_error!(meta.path(), "unrecognized option");
                return None;
            }
        };

        fn path_only(meta: &Meta, ident: &str, arg: RuleAttrArg) -> Option<RuleAttrArg> {
            match meta {
                Meta::Path(_) => Some(arg),
                _ => {
                    emit_error!(meta, "expected only a path: `{}`", ident);
                    None
                }
            }
        }

        fn lit_str(
            meta: &Meta,
            ident: &str,
            f: impl Fn(LitStr) -> RuleAttrArg,
        ) -> Option<RuleAttrArg> {
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

        match ident.as_str() {
            "string" => lit_str(meta, "string", RuleAttrArg::String),
            "pattern" => lit_str(meta, "pattern", RuleAttrArg::Pattern),
            "repeat" => path_only(meta, "repeat", RuleAttrArg::Repeat),
            "repeat1" => path_only(meta, "repeat1", RuleAttrArg::Repeat1),
            i => {
                emit_error!(ident, "unrecognized option {}", i);
                None
            }
        }
    }
}

fn is_unit(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Tuple(tt) => tt.elems.is_empty(),
        _ => false,
    }
}

/// Attempts to parse an attribute as a `#[rule(...)]` attribute.
///
/// If the attribute is a rule attribute, returns a list of successfully parsed arguments to the
/// attribute, and emits errors for each failed argument.
fn parse_rule_attr(attr: &Attribute) -> Option<Vec<(RuleAttrArg, Meta)>> {
    if !attr.path.is_ident("rule") {
        return None;
    }

    let meta = match attr.parse_meta() {
        Ok(m) => m,

        Err(e) => {
            emit_error!(e.span(), "invalid `rule` attribute: {}", e);
            return None;
        }
    };

    let list = match meta {
        syn::Meta::List(l) => l,
        _ => {
            emit_error!(meta, "expected a list-like attribute: `#[rule(...)]`");
            return None;
        }
    };

    let mut parsed = Vec::new();

    for arg in list.nested.into_iter() {
        let m = match arg {
            NestedMeta::Meta(m) => m,
            NestedMeta::Lit(_) => {
                emit_error!(
                    arg,
                    "expected a meta item: `item`, `item = \"...\"` or `item(...)`"
                );
                continue;
            }
        };

        if let Some(arg) = RuleAttrArg::from_meta(&m) {
            parsed.push((arg, m));
        }
    }

    Some(parsed)
}

#[derive(Default)]
struct LeafRuleBuilder {
    rule: Option<dsl::Rule>,
}

impl LeafRuleBuilder {
    fn set_rule_once(&mut self, rule: dsl::Rule, meta: &Meta) {
        if let Some(old) = self.rule.as_ref() {
            emit_error!(meta, "multiple rules specified");
        } else {
            self.rule = Some(rule);
        }
    }
}

fn derive_leaf_rule(ident: &Ident, attrs: Vec<Attribute>) -> TokenStream {
    let mut builder = LeafRuleBuilder::default();
    for args in attrs.iter().filter_map(parse_rule_attr) {
        for (arg, meta) in args {
            match arg {
                RuleAttrArg::String(s) => {
                    builder.set_rule_once(dsl::Rule::string(s.value()), &meta);
                }
                RuleAttrArg::Pattern(s) => {
                    builder.set_rule_once(dsl::Rule::pattern(s.value()), &meta);
                }
                _ => {
                    emit_error!(&meta, "option not supported for leaf rules");
                }
            }
        }
    }

    let rule = match builder.rule {
        Some(r) => r,
        None => abort!(
            ident,
            "expected a leaf rule";
            help = "add an appropriate attribute, e.g.: `#[rule(string = \"...\")]`"
        ),
    };

    let emit_impl = rule_to_token_stream(ident.span(), &rule);
    quote_spanned! { ident.span() => #emit_impl }
}

#[derive(Default)]
struct SeqRuleBuilder {
    // A list of expressions evaluating to `dsl::Rule`.
    members: Vec<TokenStream>,
}

enum FieldRuleProp {
    String(LitStr),
    Pattern(LitStr),
    Repeat,
    Repeat1,
}

struct FieldRuleBuilder {
    name: Ident,
    rule: Option<dsl::Rule>,
    prop: Option<FieldRuleProp>,
}

impl FieldRuleBuilder {
    fn new(name: Ident) -> FieldRuleBuilder {
        FieldRuleBuilder {
            name,
            rule: None,
            prop: None,
        }
    }

    fn set_rule_once(&mut self, rule: dsl::Rule, meta: &Meta) {
        if self.rule.is_some() {
            emit_error!(meta, "multiple rules specified");
        } else {
            self.rule = Some(rule);
        }
    }

    fn set_prop_once(&mut self, modifier: FieldRuleProp, meta: &Meta) {
        if self.prop.is_some() {
            emit_error!(meta, "conflicting rule occurrences specified");
        } else {
            self.prop = Some(modifier);
        }
    }
}

fn parse_vec_inner(ty_path: &TypePath) -> Option<&syn::Type> {
    static TARGETS: &[&[&str]] = &[&["Vec"], &["std", "vec", "Vec"], &["alloc", "vec", "Vec"]];

    let segments = ty_path
        .path
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>();

    let mut is_vec = false;
    for target in TARGETS {
        if segments.len() == target.len() && segments.iter().zip(target.iter()).all(|(l, r)| l == r)
        {
            is_vec = true;
            break;
        }
    }

    if !is_vec {
        return None;
    }

    let option_segment = ty_path.path.segments.last().unwrap();

    let inner = match &option_segment.arguments {
        syn::PathArguments::AngleBracketed(params) => params.args.first().unwrap(),
        _ => return None,
    };

    match inner {
        syn::GenericArgument::Type(ty) => Some(ty),
        _ => None,
    }
}

fn derive_seq_rule(ident: &Ident, attrs: Vec<Attribute>, fields: FieldsNamed) -> TokenStream {
    // Parse any options on the struct itself.
    for args in attrs.iter().filter_map(parse_rule_attr) {
        for (arg, meta) in args {
            match arg {
                _ => emit_error!(&meta, "option not supported on structs with named fields"),
            }
        }
    }

    let mut builder = SeqRuleBuilder::default();

    for field in fields.named {
        let field_span = field.span();
        let field_ident = field.ident.clone().unwrap();
        let field_ident_str = field_ident.to_string();

        let mut field_builder = FieldRuleBuilder::new(field_ident.clone());

        for args in field.attrs.iter().filter_map(parse_rule_attr) {
            for (arg, meta) in args {
                match arg {
                    RuleAttrArg::String(lit) => {
                        field_builder.set_prop_once(FieldRuleProp::String(lit), &meta)
                    }
                    RuleAttrArg::Pattern(lit) => {
                        field_builder.set_prop_once(FieldRuleProp::Pattern(lit), &meta)
                    }
                    RuleAttrArg::Repeat => {
                        field_builder.set_prop_once(FieldRuleProp::Repeat, &meta)
                    }
                    RuleAttrArg::Repeat1 => {
                        field_builder.set_prop_once(FieldRuleProp::Repeat1, &meta)
                    }
                }
            }
        }

        let field_ty = &field.ty;
        let field_rule = match field_builder.prop {
            Some(FieldRuleProp::String(lit)) => {
                let from_str_check = (!is_unit(field_ty)).then(|| {
                    quote_spanned! { field.ty.span() =>
                        fn _from_str<T: core::str::FromStr>(_: T) {}
                        _from_str::<#field_ty>();
                    }
                });

                quote_spanned! { field.ty.span() =>
                    #from_str_check
                    quercus::dsl::Rule::string(#lit.into())
                }
            }

            Some(FieldRuleProp::Pattern(lit)) => {
                let from_str_check = (!is_unit(field_ty)).then(|| {
                    quote_spanned! { field.ty.span() =>
                        fn _from_str<T: core::str::FromStr>() {}
                        _from_str::<#field_ty>();
                    }
                });

                quote_spanned! { field.ty.span() =>
                    {#from_str_check
                    quercus::dsl::Rule::string(#lit.into())}
                }
            }

            Some(FieldRuleProp::Repeat) => {
                quote_spanned! { field.ty.span() =>
                    <#field_ty as quercus::Repeat>::emit_repeat()
                }
            }

            Some(FieldRuleProp::Repeat1) => {
                quote_spanned! { field.ty.span() =>
                    <#field_ty as quercus::Repeat>::emit_repeat1()
                }
            }

            None => quote_spanned! { field.ty.span() => <#field_ty as quercus::Rule>::emit() },
        };

        // TODO: attr to allow changing field name
        builder.members.push(quote_spanned! { field_span =>
            dsl::Rule::field(
                #field_ident_str,
                #field_rule,
            )
        });

        // for args in field.attrs.into_iter().filter_map(parse_rule_attr) {
        //     for (arg, meta) in args {
        //         match arg {}
        //     }
        // }
    }

    let members = builder.members;

    quote_spanned! { ident.span() =>
        quercus::dsl::Rule::seq(
            core::iter::empty()
                #(.chain(core::iter::once(#members)))*
        )
    }
}
