//! Rust interface to the Tree-sitter grammar DSL.
//!
//! An entire Tree-sitter grammar is described by a [`Grammar`], which is composed of [`Rule`]s.

use std::collections::BTreeMap;

use serde::Serialize;

/// A Tree-sitter grammar as described by the Tree-sitter DSL.
///
/// This serializes to a JSON object that complies with the [grammar schema][0].
///
/// [0]: https://github.com/tree-sitter/tree-sitter/blob/master/cli/src/generate/grammar-schema.json
#[derive(Clone, Debug, Serialize)]
pub struct Grammar {
    pub name: String,

    pub rules: BTreeMap<String, Rule>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub extras: Vec<Rule>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub externals: Vec<Rule>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub inline: Vec<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub conflicts: Vec<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub word: Option<String>,
}

impl Grammar {
    pub fn new(name: String) -> Grammar {
        Grammar {
            name,
            rules: BTreeMap::new(),
            extras: Vec::new(),
            externals: Vec::new(),
            inline: Vec::new(),
            conflicts: Vec::new(),
            word: None,
        }
    }
}

/// A rule in a Tree-sitter grammar.
///
/// Each variant has an associated convenience constructor.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(tag = "type", rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Rule {
    Blank,
    String(StringRule),
    Pattern(PatternRule),
    Symbol(SymbolRule),
    Seq(SeqRule),
    Choice(ChoiceRule),
    Repeat(RepeatRule),
    #[serde(rename = "REPEAT1")]
    Repeat1(Repeat1Rule),
    Token(TokenRule),
    ImmediateToken(ImmediateTokenRule),
    Alias(AliasRule),
    Field(FieldRule),
}

impl Rule {
    /// Creates a blank rule.
    pub fn blank() -> Rule {
        Rule::Blank
    }

    pub fn string<S: AsRef<str>>(value: S) -> Rule {
        Rule::String(StringRule {
            value: value.as_ref().into(),
        })
    }

    pub fn pattern<S: AsRef<str>>(value: S) -> Rule {
        Rule::Pattern(PatternRule {
            value: value.as_ref().into(),
        })
    }

    pub fn symbol<S: AsRef<str>>(name: S) -> Rule {
        Rule::Symbol(SymbolRule {
            name: name.as_ref().into(),
        })
    }

    pub fn seq<I>(members: I) -> Rule
    where
        I: IntoIterator<Item = Rule>,
    {
        Rule::Seq(SeqRule {
            members: members.into_iter().collect(),
        })
    }

    pub fn choice<I>(members: I) -> Rule
    where
        I: IntoIterator<Item = Rule>,
    {
        Rule::Choice(ChoiceRule {
            members: members.into_iter().collect(),
        })
    }

    pub fn repeat(content: Rule) -> Rule {
        Rule::Repeat(RepeatRule {
            content: Box::new(content),
        })
    }

    pub fn repeat1(content: Rule) -> Rule {
        Rule::Repeat1(Repeat1Rule {
            content: Box::new(content),
        })
    }

    pub fn optional(rule: Rule) -> Rule {
        Rule::Choice(ChoiceRule {
            members: vec![Rule::blank(), rule],
        })
    }

    pub fn token(content: Rule) -> Rule {
        Rule::Token(TokenRule {
            content: Box::new(content),
        })
    }

    pub fn immediate_token(content: Rule) -> Rule {
        Rule::ImmediateToken(ImmediateTokenRule {
            content: Box::new(content),
        })
    }

    pub fn named_alias<S: AsRef<str>>(name: S, content: Rule) -> Rule {
        Rule::Alias(AliasRule {
            value: name.as_ref().into(),
            named: true,
            content: Box::new(content),
        })
    }

    pub fn anonymous_alias<S: AsRef<str>>(value: S, content: Rule) -> Rule {
        Rule::Alias(AliasRule {
            value: value.as_ref().into(),
            named: false,
            content: Box::new(content),
        })
    }

    pub fn field<S: AsRef<str>>(name: S, rule: Rule) -> Rule {
        Rule::Field(FieldRule {
            name: name.as_ref().into(),
            rule: Box::new(rule),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct StringRule {
    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct PatternRule {
    pub value: String,
}

/// A rule which references a named rule in a grammar.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct SymbolRule {
    /// The name of the referenced rule.
    pub name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct SeqRule {
    pub members: Vec<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ChoiceRule {
    pub members: Vec<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct RepeatRule {
    pub content: Box<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Repeat1Rule {
    pub content: Box<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct TokenRule {
    pub content: Box<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ImmediateTokenRule {
    pub content: Box<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct AliasRule {
    pub value: String,
    pub named: bool,
    pub content: Box<Rule>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct FieldRule {
    pub name: String,
    pub rule: Box<Rule>,
}
