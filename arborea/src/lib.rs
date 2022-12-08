use tree_sitter as ts;

#[macro_use]
extern crate arborea_derive;

pub use arborea_derive::*;

pub mod dsl {
    pub use arborea_dsl::*;
}

pub struct Span {
    start: u32,
    len: u32,
}

pub trait Rule {
    fn emit() -> dsl::Rule;
}

impl<R: Rule> Rule for Option<R> {
    fn emit() -> dsl::Rule {
        dsl::Rule::optional(R::emit())
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
