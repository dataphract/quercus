use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_error::{abort, emit_error};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    spanned::Spanned, Attribute, DeriveInput, Field, Fields, FieldsNamed, LitStr, Meta, Type,
    TypePath, TypeTuple, Variant,
};

use crate::{meta_lit_str, meta_path_only, parse_nested_metas};

struct Rule {
    span: Span,
    ast_repr: TokenStream,
    dependencies: TokenStream,
}

impl Rule {
    fn span(&self) -> Span {
        self.span
    }

    fn emit_impl(&self, ident: Ident) -> TokenStream {
        let ast_repr = &self.ast_repr;
        let dependencies = &self.dependencies;

        quote_spanned! { self.span() =>
            impl Rule for #ident {
                fn emit() -> quercus::dsl::Rule {
                    #ast_repr
                }

                fn register_dependencies(builder: &mut quercus::GrammarBuilder) {
                    #dependencies
                }
            }
        }
    }
}

pub fn derive_rule(input: DeriveInput) -> TokenStream {
    let span = input.span();
    let DeriveInput {
        attrs,
        vis: _,
        ident,
        generics: _,
        data,
    } = input;

    match data {
        syn::Data::Struct(ds) => match &ds.fields {
            Fields::Unit => derive_rule_unit_struct(&ident, span, attrs),

            Fields::Unnamed(fields) => {
                if fields.unnamed.is_empty() {
                    derive_rule_unit_struct(&ident, span, attrs)
                } else {
                    abort!(
                        ds.fields,
                        "#[derive(Rule)] is not supported for non-empty tuple structs"
                    )
                }
            }

            Fields::Named(fields) => {
                if fields.named.is_empty() {
                    derive_rule_unit_struct(&ident, span, attrs)
                } else {
                    derive_rule_named_struct(&ident, attrs, fields.clone())
                }
            }
        },

        syn::Data::Enum(de) => derive_rule_enum(&ident, attrs, de.variants),

        syn::Data::Union(du) => abort!(
            du.union_token,
            "#[derive(Rule)] is not supported for `union`"
        ),
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

        match ident.as_str() {
            "string" => meta_lit_str(meta, "string", RuleAttrArg::String),
            "pattern" => meta_lit_str(meta, "pattern", RuleAttrArg::Pattern),
            "repeat" => meta_path_only(meta, "repeat", RuleAttrArg::Repeat),
            "repeat1" => meta_path_only(meta, "repeat1", RuleAttrArg::Repeat1),
            i => {
                emit_error!(ident, "unrecognized option {}", i);
                None
            }
        }
    }

    /// Parses all arguments to all occurrences of `#[rule]` in a list of attributes.
    fn flat_filter<'a, I>(iter: I) -> impl Iterator<Item = (RuleAttrArg, Meta)>
    where
        I: IntoIterator<Item = &'a Attribute>,
    {
        iter.into_iter().filter_map(parse_rule_attr).flatten()
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
    parse_nested_metas("rule", attr, RuleAttrArg::from_meta)
}

struct LeafRuleBuilder {
    name: Ident,
    span: Span,
    rule: Option<TokenStream>,
}

impl LeafRuleBuilder {
    fn new(ident: &Ident, span: Span) -> LeafRuleBuilder {
        LeafRuleBuilder {
            name: ident.clone(),
            span,
            rule: None,
        }
    }

    fn set_rule_once(&mut self, rule: TokenStream, meta: &Meta) {
        if self.rule.is_some() {
            emit_error!(meta, "multiple rules specified");
        } else {
            self.rule = Some(rule);
        }
    }

    fn build(self) -> TokenStream {
        let ident = self.name.clone();
        let name = self.name.to_string();

        let rule = match self.rule {
            Some(r) => r,
            None => abort!(
                ident,
                "expected a leaf rule";
                help = "add an appropriate attribute, e.g.: `#[rule(string = \"...\")]`"
            ),
        };

        quote_spanned! { self.span =>
            impl Rule for #ident {
                fn emit() -> quercus::dsl::Rule {
                    quercus::dsl::Rule::symbol(#name)
                }

                fn register_dependencies(builder: &mut quercus::GrammarBuilder) {
                    builder.add_rule(#name, #rule);
                }
            }
        }
    }
}

fn derive_rule_unit_struct(ident: &Ident, span: Span, attrs: Vec<Attribute>) -> TokenStream {
    let mut builder = LeafRuleBuilder::new(ident, span);

    for (arg, meta) in RuleAttrArg::flat_filter(&attrs) {
        match arg {
            RuleAttrArg::String(s) => {
                builder.set_rule_once(
                    quote_spanned! { meta.span() =>
                        quercus::dsl::Rule::string(#s)
                    },
                    &meta,
                );
            }
            RuleAttrArg::Pattern(s) => {
                builder.set_rule_once(
                    quote_spanned! { meta.span() =>
                        quercus::dsl::Rule::pattern(#s)
                    },
                    &meta,
                );
            }
            _ => {
                emit_error!(&meta, "option not supported for leaf rules");
            }
        }
    }

    builder.build()
}

struct RuleBuilder {
    ident: Ident,

    // `builder.register_dependencies(...)` for each field or variant.
    dependencies: Vec<TokenStream>,

    // In-AST representations of all subrules.
    ast_reprs: Vec<TokenStream>,
}

impl RuleBuilder {
    fn new(ident: Ident) -> RuleBuilder {
        RuleBuilder {
            ident,
            dependencies: Vec::new(),
            ast_reprs: Vec::new(),
        }
    }

    fn add_subrule(&mut self, builder: SubruleBuilder) {
        self.dependencies.push(builder.register_dependencies_impl());
        self.ast_reprs.push(builder.ast_repr());
    }

    fn ast_repr_symbol(&self) -> TokenStream {
        let ident = &self.ident;
        let ident_str = ident.to_string();

        quote_spanned! { self.ident.span() =>
            quercus::dsl::Rule::symbol(#ident_str)
        }
    }

    fn dependencies_struct(&self) -> TokenStream {
        let ident = &self.ident;
        let ident_str = ident.to_string();
        let each_ast_repr = &self.ast_reprs;
        let each_dep = &self.dependencies;

        quote_spanned! { self.ident.span() =>
            #(#each_dep)*
            builder.add_rule(
                #ident_str,
                quercus::dsl::Rule::seq(
                    core::iter::empty()
                        #(.chain(core::iter::once(#each_ast_repr)))*
                ),
            );
        }
    }

    fn dependencies_enum(&self) -> TokenStream {
        let ident = &self.ident;
        let ident_str = ident.to_string();
        let each_ast_repr = &self.ast_reprs;
        let each_dep = &self.dependencies;

        quote_spanned! { self.ident.span() =>
            #(#each_dep)*
            builder.add_rule(
                #ident_str,
                quercus::dsl::Rule::choice(
                    core::iter::empty()
                        #(.chain(core::iter::once(#each_ast_repr)))*
                ),
            );
        }
    }

    /// Builds the rule as a named sequence rule.
    fn build_struct_symbol(self) -> Rule {
        Rule {
            span: self.ident.span(),
            ast_repr: self.ast_repr_symbol(),
            dependencies: self.dependencies_struct(),
        }
    }

    fn build_struct_inline(self) -> Rule {
        let each_ast_repr = &self.ast_reprs;

        Rule {
            span: self.ident.span(),
            ast_repr: quote_spanned! { self.ident.span() =>
                quercus::dsl::Rule::seq(
                    core::iter::empty()
                        #(.chain(core::iter::once(#each_ast_repr)))*
                )
            },
            dependencies: TokenStream::new(),
        }
    }

    /// Builds the rule as a named choice rule.
    fn build_enum_symbol(self) -> Rule {
        Rule {
            span: self.ident.span(),
            ast_repr: self.ast_repr_symbol(),
            dependencies: self.dependencies_enum(),
        }
    }
}

struct SubruleLeaf {
    rule_meta: Meta,
    ty: Type,
    rule_impl: TokenStream,
}

enum SubruleKind {
    /// The subrule references a named rule via a symbol.
    ///
    /// This is the case if the subrule is of a named (non-repeat) type.
    Symbol(TypePath),
    /// The subrule is defined inline.
    ///
    /// This is the case for struct and tuple variants, which correspond to composite rules but have
    /// no associated symbol.
    Inline(Rule),
    /// The subrule is a sequence rule.
    ///
    /// This is the case for subrules of tuple type.
    Seq(TypeTuple),
    /// The subrule is a leaf rule.
    ///
    /// This is the case for subrules of unit type (`()`).
    Leaf(SubruleLeaf),
    /// The subrule is a repeat rule.
    ///
    /// This is the case for subrules tagged #[repeat].
    Repeat(Type),
    /// The subrule is a repeat1 rule.
    ///
    /// This is the case for subrules tagged #[repeat1].
    Repeat1(Type),
}

/// A struct field or enum variant.
enum Contents {
    Field(Field),
    Variant(Variant),
}

impl Contents {
    fn ident(&self) -> Option<&Ident> {
        match self {
            Contents::Field(f) => f.ident.as_ref(),
            Contents::Variant(v) => Some(&v.ident),
        }
    }
}

impl ToTokens for Contents {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Contents::Field(f) => f.to_tokens(tokens),
            Contents::Variant(v) => v.to_tokens(tokens),
        }
    }
}

struct SubruleBuilder {
    contents: Contents,
    subrule: Option<SubruleKind>,
}

impl SubruleBuilder {
    fn field(field: Field) -> SubruleBuilder {
        SubruleBuilder {
            contents: Contents::Field(field),
            subrule: None,
        }
    }

    fn variant(variant: Variant) -> SubruleBuilder {
        SubruleBuilder {
            contents: Contents::Variant(variant),
            subrule: None,
        }
    }

    fn set_subrule_once(&mut self, subrule: SubruleKind) {
        if let Some(old) = self.subrule.as_ref() {
            todo!("emit error");
            // emit_error!(
            //     meta,
            //     "this rule conflicts with an existing rule on this field"
            // );
        } else {
            self.subrule = Some(subrule);
        }
    }

    fn set_subrule_if_unset(&mut self, subrule: SubruleKind) {
        if self.subrule.is_none() {
            self.subrule = Some(subrule);
        }
    }

    fn ast_repr(&self) -> TokenStream {
        let inner = match self.subrule.as_ref() {
            Some(SubruleKind::Symbol(ty_path)) => {
                quote_spanned! { ty_path.span() =>
                    <#ty_path as quercus::Rule>::emit()
                }
            }

            Some(SubruleKind::Inline(rule)) => {
                let ast_repr = &rule.ast_repr;
                quote_spanned! { rule.span() =>
                    #ast_repr
                }
            }

            Some(SubruleKind::Seq(tup)) => {
                quote_spanned! { tup.span() =>
                    <#tup as quercus::Rule>::emit()
                }
            }

            Some(SubruleKind::Leaf(leaf)) => {
                let rule_impl = &leaf.rule_impl;
                quote_spanned! { leaf.rule_meta.span() =>
                    #rule_impl
                }
            }

            Some(SubruleKind::Repeat(ty)) => {
                quote_spanned! { ty.span() =>
                    <#ty as quercus::Repeat>::emit_repeat()
                }
            }

            Some(SubruleKind::Repeat1(ty)) => {
                quote_spanned! { ty.span() =>
                    <#ty as quercus::Repeat>::emit_repeat1()
                }
            }

            None => {
                emit_error!(self.contents, "no rule specified for this field");
                quote_spanned! { self.contents.span() => unimplemented!() }
            }
        };

        match self.contents.ident() {
            Some(ident) => {
                let ident_str = ident.to_string();
                quote_spanned! { self.contents.span() =>
                    quercus::dsl::Rule::field(#ident_str, #inner)
                }
            }
            None => inner,
        }
    }

    fn register_dependencies_impl(&self) -> TokenStream {
        match &self.subrule {
            Some(SubruleKind::Symbol(ty_path)) => {
                quote_spanned! { ty_path.span() =>
                    <#ty_path as quercus::Rule>::register_dependencies(builder);
                }
            }

            Some(SubruleKind::Inline(rule)) => {
                let deps = &rule.dependencies;
                quote_spanned! { rule.span() =>
                    #deps
                }
            }

            Some(SubruleKind::Seq(tup)) => {
                let each_elem = tup.elems.iter();
                quote_spanned! { self.contents.span() =>
                    #(<#each_elem as quercus::Rule>::register_dependencies(builder);)*
                }
            }

            Some(SubruleKind::Leaf(leaf)) => {
                let field_ty = &leaf.ty;

                let from_str_check = (!is_unit(&leaf.ty)).then(|| {
                    quote_spanned! { self.contents.span() =>
                        fn _from_str<T: core::str::FromStr>() {}
                        _from_str::<#field_ty>();
                    }
                });

                quote_spanned! { self.contents.span() =>
                    #from_str_check
                }
            }

            Some(SubruleKind::Repeat(ty)) => {
                quote_spanned! { ty.span() =>
                    <<#ty as quercus::Repeat>::Item>::register_dependencies(builder);
                }
            }

            Some(SubruleKind::Repeat1(ty)) => {
                quote_spanned! { ty.span() =>
                    <<#ty as quercus::Repeat>::Item>::register_dependencies(builder);
                }
            }

            None => {
                emit_error!(self.contents, "no rule specified for this field");
                quote_spanned! { self.contents.span() => unimplemented!() }
            }
        }
    }
}

fn derive_rule_field(field: Field) -> SubruleBuilder {
    let mut field_builder = SubruleBuilder::field(field.clone());

    for (arg, meta) in RuleAttrArg::flat_filter(&field.attrs) {
        match arg {
            RuleAttrArg::String(lit) => {
                field_builder.set_subrule_once(SubruleKind::Leaf(SubruleLeaf {
                    rule_meta: meta.clone(),
                    ty: field.ty.clone(),
                    rule_impl: quote_spanned! { meta.span() =>
                        quercus::dsl::Rule::string(#lit)
                    },
                }));
            }

            RuleAttrArg::Pattern(lit) => {
                field_builder.set_subrule_once(SubruleKind::Leaf(SubruleLeaf {
                    rule_meta: meta.clone(),
                    ty: field.ty.clone(),
                    rule_impl: quote_spanned! { meta.span() =>
                        quercus::dsl::Rule::pattern(#lit)
                    },
                }));
            }

            RuleAttrArg::Repeat => {
                field_builder.set_subrule_once(SubruleKind::Repeat(field.ty.clone()));
            }

            RuleAttrArg::Repeat1 => {
                field_builder.set_subrule_once(SubruleKind::Repeat1(field.ty.clone()));
            }
        }
    }

    // Strip off parens
    let mut actual_ty = &field.ty;
    while let Type::Paren(inner) = actual_ty {
        actual_ty = &inner.elem;
    }

    match actual_ty {
        Type::Array(_) => {
            emit_error!(actual_ty, "array fields are not implemented yet!");
        }

        Type::Path(ty_path) => {
            field_builder.set_subrule_if_unset(SubruleKind::Symbol(ty_path.clone()));
        }

        Type::Tuple(tup) => {
            if !tup.elems.is_empty() {
                field_builder.set_subrule_once(SubruleKind::Seq(tup.clone()));
            }
        }

        _ => {
            emit_error!(
                actual_ty,
                "fields of this type are not supported by `#[derive(Rule)]`"
            );
        }
    }

    field_builder
}

fn derive_rule_named_struct(
    ident: &Ident,
    attrs: Vec<Attribute>,
    fields: FieldsNamed,
) -> TokenStream {
    // Parse any options on the struct itself.
    for (_arg, meta) in RuleAttrArg::flat_filter(&attrs) {
        emit_error!(&meta, "option not supported on structs with named fields");
    }

    let mut builder = RuleBuilder::new(ident.clone());

    for field in fields.named {
        builder.add_subrule(derive_rule_field(field));
    }

    builder.build_struct_symbol().emit_impl(ident.clone())
}

fn derive_rule_enum(
    ident: &Ident,
    attrs: Vec<Attribute>,
    variants: impl IntoIterator<Item = Variant>,
) -> TokenStream {
    // Parse any options on the enum itself.
    for (_arg, meta) in RuleAttrArg::flat_filter(&attrs) {
        emit_error!(&meta, "option not supported on enums");
    }

    let mut builder = RuleBuilder::new(ident.clone());

    for variant in variants {
        let vb = match &variant.fields {
            Fields::Named(fs) => derive_rule_seq_variant(variant),
            Fields::Unnamed(fs) => {
                if fs.unnamed.len() == 1 {
                    derive_rule_single_variant(variant)
                } else {
                    derive_rule_seq_variant(variant)
                }
            }
            Fields::Unit => derive_rule_unit_variant(variant),
        };

        builder.add_subrule(vb);
    }

    builder.build_enum_symbol().emit_impl(ident.clone())
}

fn derive_rule_unit_variant(variant: Variant) -> SubruleBuilder {
    assert!(matches!(variant.fields, Fields::Unit));

    let mut builder = SubruleBuilder::variant(variant.clone());

    for (arg, meta) in RuleAttrArg::flat_filter(&variant.attrs) {
        match arg {
            RuleAttrArg::String(lit) => builder.set_subrule_once(SubruleKind::Leaf(SubruleLeaf {
                rule_meta: meta.clone(),
                ty: syn::parse2(quote! { () }).unwrap(),
                rule_impl: quote_spanned! { meta.span() =>
                    quercus::dsl::Rule::string(#lit)
                },
            })),

            RuleAttrArg::Pattern(lit) => builder.set_subrule_once(SubruleKind::Leaf(SubruleLeaf {
                rule_meta: meta.clone(),
                ty: syn::parse2(quote! { () }).unwrap(),
                rule_impl: quote_spanned! { meta.span() =>
                    quercus::dsl::Rule::pattern(#lit)
                },
            })),

            _ => {
                emit_error!(&meta, "option not supported on unit variants");
            }
        }
    }

    builder
}

/// Special case for tuple variants with only one field.
fn derive_rule_single_variant(variant: Variant) -> SubruleBuilder {
    let Fields::Unnamed(fields) = variant.fields else {
        unreachable!("called derive_rule_single_variant on a non-tuple variant");
    };

    for (arg, meta) in RuleAttrArg::flat_filter(&variant.attrs) {
        emit_error!(&meta, "option not supported on struct or tuple variants");
    }

    assert!(fields.unnamed.len() == 1);

    derive_rule_field(Field {
        ident: Some(variant.ident.clone()),
        ..fields.unnamed[0].clone()
    })
}

fn derive_rule_seq_variant(variant: Variant) -> SubruleBuilder {
    let fields = match &variant.fields {
        Fields::Named(fs) => &fs.named,
        Fields::Unnamed(fs) => &fs.unnamed,
        Fields::Unit => unreachable!("called derive_rule_seq_variant on a unit variant"),
    };

    for (arg, meta) in RuleAttrArg::flat_filter(&variant.attrs) {
        emit_error!(&meta, "option not supported on struct or tuple variants");
    }

    let mut builder = RuleBuilder::new(variant.ident.clone());

    for field in fields {
        builder.add_subrule(derive_rule_field(field.clone()));
    }

    let subrule = builder.build_struct_inline();

    let mut builder = SubruleBuilder::variant(variant.clone());
    builder.set_subrule_once(SubruleKind::Inline(subrule));

    builder
}
