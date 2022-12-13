use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_error::{abort, emit_error};
use quote::quote_spanned;
use syn::{
    spanned::Spanned, Attribute, DeriveInput, Field, FieldsNamed, LitStr, Meta, Type, TypePath,
    TypeTuple,
};

use crate::{meta_lit_str, meta_path_only, parse_nested_metas};

pub fn derive_rule(input: DeriveInput) -> TokenStream {
    let span = input.span();
    let DeriveInput {
        attrs,
        vis: _,
        ident,
        generics,
        data,
    } = input;

    match data {
        syn::Data::Struct(ds) => match &ds.fields {
            syn::Fields::Unit => derive_rule_unit_struct(&ident, span, attrs),

            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.is_empty() {
                    derive_rule_unit_struct(&ident, span, attrs)
                } else {
                    abort!(
                        ds.fields,
                        "#[derive(Rule)] is not supported for non-empty tuple structs"
                    )
                }
            }

            syn::Fields::Named(fields) => {
                if fields.named.is_empty() {
                    derive_rule_unit_struct(&ident, span, attrs)
                } else {
                    derive_rule_named_struct(&ident, attrs, fields.clone())
                }
            }
        },

        syn::Data::Enum(de) => todo!("enum"),

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

    for args in attrs.iter().filter_map(parse_rule_attr) {
        for (arg, meta) in args {
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
    }

    builder.build()
}

struct StructRuleBuilder {
    ident: Ident,

    // `builder.register_dependencies(...)` for each field.
    dependencies: Vec<TokenStream>,

    // In-AST rule representations.
    ast_reprs: Vec<TokenStream>,
}

impl StructRuleBuilder {
    fn new(ident: Ident) -> StructRuleBuilder {
        StructRuleBuilder {
            ident,
            dependencies: Vec::new(),
            ast_reprs: Vec::new(),
        }
    }

    fn add_field(&mut self, builder: SubruleBuilder) {
        self.dependencies.push(builder.register_dependencies_impl());
        self.ast_reprs.push(builder.ast_repr());
    }

    fn build(self) -> TokenStream {
        let ident = &self.ident;
        let ident_str = ident.to_string();
        let each_ast_repr = &self.ast_reprs;
        let each_dep = &self.dependencies;

        quote_spanned! { self.ident.span() =>
            impl Rule for #ident {
                fn emit() -> quercus::dsl::Rule {
                    quercus::dsl::Rule::symbol(#ident_str)
                }

                fn register_dependencies(builder: &mut quercus::GrammarBuilder) {
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
    fn new(field: Field) -> SubruleBuilder {
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
        match self.subrule.as_ref() {
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

            Some(SubruleKind::Leaf(leaf)) => {
                let field_ty = &leaf.ty;
                let from_str_check = (!is_unit(&leaf.ty)).then(|| {
                    quote_spanned! { self.field.ty.span() =>
                        fn _from_str<T: core::str::FromStr>() {}
                        _from_str::<#field_ty>();
                    }
                });

                quote_spanned! { self.field.span() =>
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
                emit_error!(self.field, "no rule specified for this field");
                quote_spanned! { self.field.span() => unimplemented!() }
            }
        }
    }
}

fn derive_rule_named_struct(
    ident: &Ident,
    attrs: Vec<Attribute>,
    fields: FieldsNamed,
) -> TokenStream {
    // Parse any options on the struct itself.
    for args in attrs.iter().filter_map(parse_rule_attr) {
        for (_arg, meta) in args {
            emit_error!(&meta, "option not supported on structs with named fields");
        }
    }

    let mut builder = StructRuleBuilder::new(ident.clone());

    for field in fields.named {
        let mut field_builder = SubruleBuilder::new(field.clone());

        for args in field.attrs.iter().filter_map(parse_rule_attr) {
            for (arg, meta) in args {
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
        }

        // Strip off parens
        let mut actual_ty = &field.ty;
        while let Type::Paren(inner) = actual_ty {
            actual_ty = &inner.elem;
        }

        match actual_ty {
            Type::Array(_) => {
                emit_error!(actual_ty, "array fields are not implemented yet!");
                continue;
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
                continue;
            }
        }

        builder.add_field(field_builder);
    }

    builder.build()
}
