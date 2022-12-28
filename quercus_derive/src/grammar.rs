use proc_macro2::TokenStream;
use proc_macro_error::emit_error;
use quote::{quote, quote_spanned, ToTokens};
use syn::{spanned::Spanned, DeriveInput, Ident, LitStr, Meta};

use crate::{meta_lit_str, parse_nested_metas};

enum GrammarAttrArg {
    Extra(LitStr),
    Word(LitStr),
}

impl GrammarAttrArg {
    fn from_meta(meta: &Meta) -> Option<GrammarAttrArg> {
        let ident = match meta.path().get_ident() {
            Some(i) => i.to_string(),
            None => {
                emit_error!(meta.path(), "unrecognized option");
                return None;
            }
        };

        match ident.as_str() {
            "extra" => meta_lit_str(meta, "string", GrammarAttrArg::Extra),
            "word" => meta_lit_str(meta, "word", GrammarAttrArg::Word),
            i => {
                emit_error!(ident, "unrecognized option {}", i);
                None
            }
        }
    }
}

struct GrammarBuilder {
    ident: Ident,
    extras: Vec<TokenStream>,
    word: Option<TokenStream>,
}

impl GrammarBuilder {
    fn new(ident: Ident) -> GrammarBuilder {
        GrammarBuilder {
            ident,
            extras: Vec::new(),
            word: None,
        }
    }

    fn add_extra(&mut self, tokens: TokenStream) {
        self.extras.push(tokens);
    }

    fn set_word_once(&mut self, word: TokenStream, meta: &Meta) {
        if self.word.is_some() {
            emit_error!(meta, "multiple `word` properties specified")
        } else {
            self.word = Some(word);
        }
    }

    fn build(self) -> TokenStream {
        let ident = &self.ident;
        let ident_str = ident.to_string();
        let each_extra = &self.extras;
        let set_word_rule = self.word.clone().map(|w| {
            quote_spanned! { w.span() => builder.set_word_rule(#w); }
        });

        quote_spanned! { ident.span() =>
            impl quercus::Grammar for #ident {
                fn grammar_dsl() -> quercus::dsl::Grammar {
                    let mut builder = quercus::GrammarBuilder::new(#ident_str);
                    <Self as quercus::Rule>::register_dependencies(&mut builder);
                    builder.add_rule("source_file", quercus::dsl::Rule::symbol(#ident_str));
                    #(
                        builder.add_extra(#each_extra);
                    )*
                    #set_word_rule
                    builder.build()
                }
            }
        }
    }
}

pub fn derive_grammar(input: DeriveInput) -> TokenStream {
    let DeriveInput {
        attrs,
        vis: _,
        ident,
        generics,
        data,
    } = input;

    let mut builder = GrammarBuilder::new(ident.clone());

    for args in attrs
        .iter()
        .filter_map(|attr| parse_nested_metas("grammar", attr, GrammarAttrArg::from_meta))
    {
        for (arg, meta) in args {
            match arg {
                GrammarAttrArg::Extra(lit) => {
                    let Ok(ty) = lit.parse::<syn::Type>() else {
                        emit_error!(lit, "`{}` is not a type", lit.value());
                        continue;
                    };

                    builder.add_extra(quote_spanned! { meta.span() =>
                        <#ty as quercus::Rule>::emit()
                    })
                }

                GrammarAttrArg::Word(lit) => builder.set_word_once(lit.to_token_stream(), &meta),
            }
        }
    }

    builder.build()
}
