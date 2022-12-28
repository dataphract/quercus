use std::collections::{btree_map::Entry, BTreeMap};

use tree_sitter as ts;

#[macro_use]
extern crate quercus_derive;

pub use quercus_derive::*;

pub mod dsl {
    pub use quercus_dsl::*;
}

pub struct Span {
    start: u32,
    len: u32,
}

pub trait Grammar {
    fn grammar_dsl() -> dsl::Grammar;
}

#[derive(Debug)]
pub struct GrammarBuilder {
    name: String,
    rules: BTreeMap<String, dsl::Rule>,
    extras: Vec<dsl::Rule>,
    word: Option<String>,
}

impl GrammarBuilder {
    pub fn new<S: AsRef<str>>(name: S) -> GrammarBuilder {
        GrammarBuilder {
            name: name.as_ref().into(),
            rules: BTreeMap::new(),
            extras: Vec::new(),
            word: None,
        }
    }

    /// Add a named rule to the grammar.
    ///
    /// Returns `true` if the rule is newly added, `false` otherwise.
    ///
    /// # Panics
    ///
    /// Panics if there is an existing rule named `name` that is not equal to `rule`.
    pub fn add_rule(&mut self, name: &str, rule: dsl::Rule) -> bool {
        match self.rules.entry(name.to_string()) {
            Entry::Vacant(v) => {
                v.insert(rule);
                true
            }

            Entry::Occupied(o) => {
                if o.get() != &rule {
                    panic!("multiple rules named `{name}` specified");
                }

                false
            }
        }
    }

    pub fn add_extra(&mut self, rule: dsl::Rule) {
        self.extras.push(rule);
    }

    pub fn set_word_rule(&mut self, word_rule: String) {
        self.word = Some(word_rule);
    }

    pub fn build(self) -> dsl::Grammar {
        dsl::Grammar {
            name: self.name,
            rules: self.rules,
            extras: self.extras,
            externals: vec![],
            inline: vec![],
            conflicts: vec![],
            word: self.word,
        }
    }
}

pub trait Rule {
    /// Emits the DSL representation of this rule.
    ///
    /// If the rule should appear as a symbol in the complete grammar, this method should be
    /// implemented using `Rule::symbol()`, and the actual rule definition should be provided via
    /// `Self::register_dependencies()`.
    fn emit() -> dsl::Rule;

    /// Registers any named rules referenced by this rule.
    ///
    /// If any of this rule's subrules are of type `Rule::Symbol`, then this method must be
    /// implemented to register the associated name with `builder`.
    fn register_dependencies(builder: &mut GrammarBuilder) {}
}

impl<R: Rule> Rule for Option<R> {
    fn emit() -> dsl::Rule {
        dsl::Rule::optional(R::emit())
    }

    fn register_dependencies(builder: &mut GrammarBuilder) {
        R::register_dependencies(builder)
    }
}

macro_rules! impl_rule_for_tuple {
    ($($param:ident)*) => {
        impl<$($param),*> Rule for ($($param,)*) where $($param: Rule),* {
            fn emit() -> dsl::Rule {
                dsl::Rule::seq(
                    core::iter::empty()
                        $(.chain(core::iter::once(<$param as Rule>::emit())))*
                )
            }

            fn register_dependencies(builder: &mut GrammarBuilder) {
                $(<$param as Rule>::register_dependencies(builder);)*
            }
        }
    };
}

impl_rule_for_tuple! { A }
impl_rule_for_tuple! { A B }
impl_rule_for_tuple! { A B C }
impl_rule_for_tuple! { A B C D }
impl_rule_for_tuple! { A B C D E }
impl_rule_for_tuple! { A B C D E F }
impl_rule_for_tuple! { A B C D E F G }
impl_rule_for_tuple! { A B C D E F G H }
impl_rule_for_tuple! { A B C D E F G H I }
impl_rule_for_tuple! { A B C D E F G H I J }

pub trait Repeat: FromIterator<Self::Item> {
    type Item: Rule;

    fn emit_repeat() -> dsl::Rule {
        dsl::Rule::repeat(Self::Item::emit())
    }

    fn emit_repeat1() -> dsl::Rule {
        dsl::Rule::repeat1(Self::Item::emit())
    }
}

impl<R: Rule> Repeat for Vec<R> {
    type Item = R;
}
