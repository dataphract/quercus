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
    /// Rust expression evaluating to the in-AST representation of the rule.
    ast_repr: TokenStream,
    /// Rust statement(s) registering any symbols required by the rule, including its own.
    dependencies: TokenStream,
    /// Rust expression creating the associated type from a Tree-sitter node.
    from_node: TokenStream,
}

impl Rule {
    fn span(&self) -> Span {
        self.span
    }

    fn emit_impl(&self, ident: Ident) -> TokenStream {
        let ast_repr = &self.ast_repr;
        let dependencies = &self.dependencies;
        let from_node = &self.from_node;

        quote_spanned! { self.span() =>
            impl quercus::Rule for #ident {
                fn emit() -> quercus::dsl::Rule {
                    #ast_repr
                }

                fn register_dependencies(builder: &mut quercus::GrammarBuilder) {
                    #dependencies
                }

                fn from_node(node: &tree_sitter::Node, src: &str) -> Self {
                    #from_node
                }
            }
        }
    }
}

/// `#[derive(Rule)]` entry point.
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
            Fields::Unit => derive_rule_unit_struct(&ident, UnitStructKind::Unit, span, attrs),

            Fields::Unnamed(fields) => {
                if fields.unnamed.is_empty() {
                    derive_rule_unit_struct(&ident, UnitStructKind::Tuple, span, attrs)
                } else {
                    abort!(
                        ds.fields,
                        "#[derive(Rule)] is not supported for non-empty tuple structs"
                    )
                }
            }

            Fields::Named(fields) => {
                if fields.named.is_empty() {
                    derive_rule_unit_struct(&ident, UnitStructKind::Named, span, attrs)
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

enum UnitStructKind {
    Named,
    Tuple,
    Unit,
}

struct LeafRuleBuilder {
    name: Ident,
    kind: UnitStructKind,
    span: Span,
    rule: Option<TokenStream>,
}

impl LeafRuleBuilder {
    fn new(ident: &Ident, kind: UnitStructKind, span: Span) -> LeafRuleBuilder {
        LeafRuleBuilder {
            name: ident.clone(),
            kind,
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

        let expr = match self.kind {
            UnitStructKind::Named => quote! { #ident {} },
            UnitStructKind::Tuple => quote! { #ident() },
            UnitStructKind::Unit => quote! { #ident },
        };

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

                fn from_node(node: &tree_sitter::Node, src: &str) -> Self {
                    #expr
                }
            }
        }
    }
}

fn derive_rule_unit_struct(
    ident: &Ident,
    kind: UnitStructKind,
    span: Span,
    attrs: Vec<Attribute>,
) -> TokenStream {
    let mut builder = LeafRuleBuilder::new(ident, kind, span);

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

struct StructRuleBuilder {
    ident: Ident,

    named: FieldsNamedBuilder,
}

impl StructRuleBuilder {
    fn new(ident: Ident) -> StructRuleBuilder {
        StructRuleBuilder {
            ident: ident.clone(),
            named: FieldsNamedBuilder::new(ident.span(), ident.to_token_stream()),
        }
    }

    fn add_field(&mut self, field: &Field, subrule: SubruleBuilder) {
        self.named.add_field(field, subrule);
    }

    fn ast_repr(&self) -> TokenStream {
        self.named.ast_repr()
    }

    fn dependencies(&self) -> TokenStream {
        let ident = &self.ident;
        let ident_str = ident.to_string();
        let each_ast_repr = &self.named.ast_reprs;
        let each_dep = &self.named.dependencies;

        quote_spanned! { self.ident.span() =>
            // Only register dependencies if this rule hasn't been seen before.
            if builder.add_rule(
                #ident_str,
                quercus::dsl::Rule::seq(
                    core::iter::empty()
                        #(.chain(core::iter::once(#each_ast_repr)))*
                ),
            ) {
                #(#each_dep)*
            }
        }
    }

    fn from_node_impl(&self) -> TokenStream {
        self.named.from_node_impl()
    }

    /// Builds the rule as a named sequence rule.
    fn build(self) -> Rule {
        Rule {
            span: self.ident.span(),
            ast_repr: self.ast_repr(),
            dependencies: self.dependencies(),
            from_node: self.from_node_impl(),
        }
    }
}

struct EnumRuleBuilder {
    ident: Ident,

    // `builder.register_dependencies(...)` for each field or variant.
    dependencies: Vec<TokenStream>,

    // In-AST representations of all subrules.
    ast_reprs: Vec<TokenStream>,

    // Match branches to construct each variant.
    from_node: Vec<TokenStream>,
}

impl EnumRuleBuilder {
    fn new(ident: Ident) -> EnumRuleBuilder {
        EnumRuleBuilder {
            ident,
            dependencies: Vec::new(),
            ast_reprs: Vec::new(),
            from_node: Vec::new(),
        }
    }

    fn add_variant_named(&mut self, ident: Ident, named: FieldsNamedBuilder) {
        self.dependencies.extend(named.dependencies.iter().cloned());

        let ident_str = ident.to_string();
        let ast_repr = named.ast_repr();
        self.ast_reprs.push(quote_spanned! { ident.span() =>
            quercus::dsl::Rule::field(#ident_str, #ast_repr)
        });

        let from_node = named.from_node_impl();
        self.from_node.push(quote_spanned! { ident.span() =>
            #ident_str => { #from_node }
        });
    }

    fn add_variant_unnamed(&mut self, ident: Ident, unnamed: FieldsUnnamedBuilder) {
        self.dependencies
            .extend(unnamed.dependencies.iter().cloned());

        let ident_str = ident.to_string();
        let ast_repr = unnamed.ast_repr();
        self.ast_reprs.push(quote_spanned! { ident.span() =>
            quercus::dsl::Rule::field(#ident_str, #ast_repr)
        });

        let from_node = unnamed.from_node_impl();
        self.from_node.push(quote_spanned! { ident.span() =>
            #ident_str => { #from_node }
        });
    }

    fn add_variant_single(&mut self, ident: Ident, subrule: SubruleBuilder) {
        self.dependencies.push(subrule.register_dependencies_impl());

        let ident_str = ident.to_string();
        let ast_repr = subrule.ast_repr();
        self.ast_reprs.push(quote_spanned! { ident.span() =>
            quercus::dsl::Rule::field(#ident_str, #ast_repr)
        });

        let from_node = subrule.from_node_impl();
        self.from_node.push(quote_spanned! { ident.span() =>
            #ident_str => { #from_node }
        });
    }

    fn add_variant(&mut self, builder: SubruleBuilder) {
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

    fn dependencies(&self) -> TokenStream {
        let ident = &self.ident;
        let ident_str = ident.to_string();
        let each_ast_repr = &self.ast_reprs;
        let each_dep = &self.dependencies;

        quote_spanned! { self.ident.span() =>
            // Only register dependencies if this rule hasn't been seen before.
            if builder.add_rule(
                #ident_str,
                quercus::dsl::Rule::choice(
                    core::iter::empty()
                        #(.chain(core::iter::once(#each_ast_repr)))*
                ),
            ) {
                #(#each_dep)*
            }
        }
    }

    fn from_node_impl(&self) -> TokenStream {
        let each_branch = &self.from_node;

        quote! {
            match node.kind() {
                #(#each_branch),*
                other => panic!("`{other}` is not a subrule"),
            }
        }
    }

    /// Builds the rule as a named choice rule.
    fn build_symbol(self) -> Rule {
        Rule {
            span: self.ident.span(),
            ast_repr: self.ast_repr_symbol(),
            dependencies: self.dependencies(),
            from_node: self.from_node_impl(),
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

struct SubruleBuilder {
    field: Field,
    subrule: Option<SubruleKind>,
}

impl SubruleBuilder {
    fn field(field: Field) -> SubruleBuilder {
        SubruleBuilder {
            field,
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
                emit_error!(self.field, "no rule specified for this field");
                quote_spanned! { self.field.span() => unimplemented!() }
            }
        };

        match &self.field.ident {
            Some(ident) => {
                let ident_str = ident.to_string();
                quote_spanned! { self.field.span() =>
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

            Some(SubruleKind::Seq(tup)) => {
                let each_elem = tup.elems.iter();
                quote_spanned! { self.field.span() =>
                    #(<#each_elem as quercus::Rule>::register_dependencies(builder);)*
                }
            }

            Some(SubruleKind::Leaf(leaf)) => TokenStream::new(),

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
                emit_error!(self.field, "no rule specified for this field");
                quote_spanned! { self.field.span() => unimplemented!() }
            }
        }
    }

    fn from_node_impl(&self) -> TokenStream {
        match &self.subrule {
            Some(SubruleKind::Symbol(ty_path)) => {
                quote_spanned! { ty_path.span() =>
                    <ty_path as quercus::Rule>::from_node(node, src)
                }
            }

            Some(SubruleKind::Seq(tup)) => {
                quote_spanned! { tup.span() =>
                    <#tup as quercus::Rule>::from_node(node, src)
                }
            }

            Some(SubruleKind::Leaf(leaf)) => {
                let field_ty = &leaf.ty;

                let from_str = (!is_unit(&leaf.ty)).then(|| {
                    quote_spanned! { self.field.span() =>
                        let range = node.byte_range();
                        let snippet = &src[range];
                        <#field_ty as core::str::FromStr>::from_str(snippet).unwrap()
                    }
                });

                quote_spanned! { self.field.span() =>
                    #from_str
                }
            }

            Some(SubruleKind::Repeat(ty)) => {
                quote_spanned! { ty.span() =>
                    todo!("from_node is not implemented for Repeat")
                }
            }

            Some(SubruleKind::Repeat1(ty)) => {
                quote_spanned! { ty.span() =>
                    todo!()
                }
            }

            None => {
                emit_error!(self.field, "no rule specified for this field");
                quote_spanned! { self.field.span() => unimplemented!() }
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

    let mut builder = StructRuleBuilder::new(ident.clone());

    for field in fields.named {
        let rule = derive_rule_field(field.clone());
        builder.add_field(&field, rule);
    }

    builder.build().emit_impl(ident.clone())
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

    let mut builder = EnumRuleBuilder::new(ident.clone());

    for variant in variants {
        match &variant.fields {
            Fields::Named(named) => {
                let var_ident = &variant.ident;
                let mut vb =
                    FieldsNamedBuilder::new(variant.span(), quote! { #ident :: #var_ident });

                for field in &named.named {
                    vb.add_field(field, derive_rule_field(field.clone()));
                }

                builder.add_variant_named(variant.ident.clone(), vb);
            }

            Fields::Unnamed(unnamed) => {
                if unnamed.unnamed.len() == 1 {
                    builder.add_variant_single(
                        variant.ident.clone(),
                        derive_rule_field(unnamed.unnamed[0].clone()),
                    );
                } else {
                    let var_ident = &variant.ident;
                    let mut vb =
                        FieldsUnnamedBuilder::new(variant.span(), quote! { #ident :: #var_ident });

                    for (idx, field) in unnamed.unnamed.iter().enumerate() {
                        vb.add_field(idx, field, derive_rule_field(field.clone()));
                    }

                    builder.add_variant_unnamed(variant.ident.clone(), vb);
                }
            }
            Fields::Unit => todo!(),
        }
    }

    builder.build_symbol().emit_impl(ident.clone())
}

fn from_node_impl_variant(ident: &Ident, variant: &Variant) -> TokenStream {
    let variant_ident = &variant.ident;
    let path_in_expr = quote_spanned! { variant.span() =>
        #ident :: #variant_ident
    };

    match &variant.fields {
        Fields::Named(named) => {
            let mut builder = FieldsNamedBuilder::new(variant.span(), path_in_expr);

            for field in &named.named {
                let subrule = derive_rule_field(field.clone());
                builder.add_field(&field, subrule);
            }

            todo!()
        }

        Fields::Unnamed(unnamed) => {
            let mut builder = FieldsUnnamedBuilder::new(variant.span(), path_in_expr);

            for (idx, field) in unnamed.unnamed.iter().enumerate() {
                let subrule = derive_rule_field(field.clone());
                builder.add_field(idx, &field, subrule);
            }

            todo!()
        }

        Fields::Unit => todo!(),
    }
}

/// Special case for tuple variants with only one field.
fn derive_rule_single_variant(variant: Variant) -> SubruleBuilder {
    let Fields::Unnamed(fields) = variant.fields else {
        unreachable!("called derive_rule_single_variant on a non-tuple variant");
    };

    for (_arg, meta) in RuleAttrArg::flat_filter(&variant.attrs) {
        emit_error!(&meta, "option not supported on struct or tuple variants");
    }

    assert!(fields.unnamed.len() == 1);

    derive_rule_field(Field {
        ident: Some(variant.ident.clone()),
        ..fields.unnamed[0].clone()
    })
}

struct FieldsNamedBuilder {
    span: Span,

    // `builder.register_dependencies(...)` for each field or variant.
    dependencies: Vec<TokenStream>,

    // In-AST representations of all fields.
    ast_reprs: Vec<TokenStream>,

    path_in_expr: TokenStream,
    each_field_from_node: Vec<TokenStream>,
}

impl FieldsNamedBuilder {
    fn new(span: Span, path_in_expr: TokenStream) -> FieldsNamedBuilder {
        FieldsNamedBuilder {
            span,
            dependencies: Vec::new(),
            ast_reprs: Vec::new(),
            path_in_expr,
            each_field_from_node: Vec::new(),
        }
    }

    fn add_field(&mut self, field: &Field, subrule: SubruleBuilder) {
        self.dependencies.push(subrule.register_dependencies_impl());
        self.ast_reprs.push(subrule.ast_repr());

        let ident = field.ident.clone().unwrap();
        let ident_str = ident.to_string();

        let from_node_impl = subrule.from_node_impl();

        self.each_field_from_node
            .push(quote_spanned! { field.span() =>
                #ident: {
                    let node = node.child_by_field_name(#ident_str).unwrap();
                    #from_node_impl
                }
            });
    }

    fn ast_repr(&self) -> TokenStream {
        let each_ast_repr = &self.ast_reprs;

        quote_spanned! { self.span =>
            quercus::dsl::Rule::seq([
                #(
                    #each_ast_repr
                ),*
            ])
        }
    }

    fn from_node_impl(&self) -> TokenStream {
        let path_in_expr = &self.path_in_expr;
        let each_field_from_node = &self.each_field_from_node;

        quote_spanned! { self.span =>
            #path_in_expr {
                #(#each_field_from_node),*
            }
        }
    }
}

struct FieldsUnnamedBuilder {
    span: Span,

    // `builder.register_dependencies(...)` for each field or variant.
    dependencies: Vec<TokenStream>,

    // In-AST representations of all fields.
    ast_reprs: Vec<TokenStream>,

    // Path used when instantiating a value of this type.
    //
    // For structs, this is the name of the struct. For enum variants, this is the path of the
    // variant.
    path_in_expr: TokenStream,
    each_field_from_node: Vec<TokenStream>,
}

impl FieldsUnnamedBuilder {
    fn new(span: Span, path_in_expr: TokenStream) -> FieldsUnnamedBuilder {
        FieldsUnnamedBuilder {
            span,
            dependencies: Vec::new(),
            ast_reprs: Vec::new(),
            path_in_expr,
            each_field_from_node: Vec::new(),
        }
    }

    fn add_field(&mut self, idx: usize, field: &Field, subrule: SubruleBuilder) {
        self.dependencies.push(subrule.register_dependencies_impl());
        self.ast_reprs.push(subrule.ast_repr());

        let ident_str = idx.to_string();
        let from_node_impl = subrule.from_node_impl();

        self.each_field_from_node
            .push(quote_spanned! { field.span() =>
                {
                    let node = node.child_by_field_name(#ident_str).unwrap();
                    #from_node_impl
                }
            });
    }

    fn ast_repr(&self) -> TokenStream {
        let each_ast_repr = &self.ast_reprs;

        quote_spanned! { self.span =>
            quercus::dsl::Rule::seq([
                #(#each_ast_repr),*
            ])
        }
    }

    fn from_node_impl(&self) -> TokenStream {
        let path_in_expr = &self.path_in_expr;
        let each_field_from_node = &self.each_field_from_node;

        quote_spanned! { self.span =>
            #path_in_expr (
                #(#each_field_from_node),*
            )
        }
    }
}
