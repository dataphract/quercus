use proc_macro2::TokenStream;
use proc_macro_error::emit_error;
use quote::quote_spanned;
use syn::{spanned::Spanned, DeriveInput, LitStr, Meta};

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

#[derive(Default)]
struct GrammarBuilder {
    extras: Vec<TokenStream>,
    word: Option<TokenStream>,
}

impl GrammarBuilder {
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
}

pub fn derive_grammar(input: DeriveInput) -> TokenStream {
    let input_span = input.span();
    let DeriveInput {
        attrs,
        vis,
        ident,
        generics,
        data,
    } = input;

    let mut builder = GrammarBuilder::default();

    for args in attrs
        .iter()
        .filter_map(|attr| parse_nested_metas("grammar", attr, GrammarAttrArg::from_meta))
    {
        for (arg, meta) in args {
            match arg {
                GrammarAttrArg::Extra(lit) => builder.add_extra(quote_spanned! { meta.span() =>
                    <#lit as quercus::Rule>::emit()
                }),
                GrammarAttrArg::Word(_) => todo!(),
            }
        }
    }

    quote_spanned! { input_span =>
        quercus::dsl::Grammar {

        }
    }
}
