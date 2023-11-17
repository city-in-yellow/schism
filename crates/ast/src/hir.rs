use errors::InterpretingError;
use rpds::{HashTrieMapSync, ListSync};
use rug::Float;
use smol_str::SmolStr;
use std::{cmp::Ordering, collections::BTreeMap, hash::Hash, mem};

use crate::{
    ast::{tagged_pretty_string, TExpr},
    trivia::{Range, Trivia, WithTrivia},
};

#[derive(Debug, Eq, PartialEq)]
pub struct Context<'w> {
    pub builtins: HashTrieMapSync<SmolStr, TVal>,
    pub environment: HashTrieMapSync<&'w SmolStr, &'w TVal>,
}

impl<'w> Context<'w> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            builtins: HashTrieMapSync::new_sync(),
            environment: HashTrieMapSync::new_sync(),
        }
    }

    pub fn with_builtin(&self, key: SmolStr, val: TVal) -> Self {
        Self {
            builtins: self.builtins.insert(key, val),
            environment: self.environment.clone(),
        }
    }

    pub fn with<'c: 'w>(&'c self, key: &'w SmolStr, val: &'w TVal) -> Self {
        Self {
            builtins: self.builtins.clone(),
            environment: self.environment.insert(key, val),
        }
    }

    pub fn find(&'w self, key: &'w SmolStr) -> Option<&'w TVal> {
        self.builtins
            .get(key)
            .or_else(|| self.environment.get(key).copied())
    }

    pub fn merge<'a: 'w>(&'a self, other: &'a HashTrieMapSync<SmolStr, TVal>) -> Self {
        let mut next_environment = self.environment.clone();
        for (key, value) in other.iter() {
            if next_environment.contains_key(&key) {
                continue;
            }

            next_environment = next_environment.insert(key, value);
        }

        Self {
            builtins: self.builtins.clone(),
            environment: next_environment,
        }
    }
}

impl<'w> Clone for Context<'w> {
    fn clone(&self) -> Self {
        Self {
            builtins: self.builtins.clone(),
            environment: self.environment.clone(),
        }
    }
}

pub type TVal = Trivia<Val>;
pub type RFunc = Result<TVal, InterpretingError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeFunction {
    pub name: SmolStr,
    pub params: ListSync<TVal>,
    pub apply: fn(Context, Range, ListSync<TVal>, TVal) -> RFunc,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Unit,
    Bool(bool),
    String(SmolStr),
    Number(isize),
    Decimal(Float),
    List(Vec<TVal>),
    Struct(BTreeMap<Trivia<SmolStr>, TVal>),
    Function(HashTrieMapSync<SmolStr, TVal>, Trivia<SmolStr>, TExpr),
    NativeFunction(NativeFunction),
}

impl Eq for Val {}
impl Hash for Val {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Val::Unit => mem::discriminant(self).hash(state),
            Val::Bool(b) => b.hash(state),
            Val::String(s) => s.hash(state),
            Val::Number(n) => n.hash(state),
            Val::Decimal(d) => d.to_string().hash(state),
            Val::List(l) => l.hash(state),
            Val::Struct(s) => s.hash(state),
            Val::Function(_, i, _) => i.hash(state),
            Val::NativeFunction(NativeFunction { name: i, .. }) => i.hash(state),
        };
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let l_d = mem::discriminant(self);
        let r_d = mem::discriminant(other);

        if l_d != r_d {
            return None;
        }

        match (self, other) {
            (Val::List(_) | Val::Function(..) | Val::NativeFunction(_), _) => None,
            (Val::Unit, _) => Some(Ordering::Equal),
            (Val::Bool(l), Val::Bool(r)) => l.partial_cmp(r),
            (Val::String(l), Val::String(r)) => l.partial_cmp(r),
            (Val::Number(l), Val::Number(r)) => l.partial_cmp(r),
            (Val::Decimal(l), Val::Decimal(r)) => l.partial_cmp(r),
            (Val::Struct(l), Val::Struct(r)) => l.partial_cmp(r),
            _ => unreachable!(),
        }
    }
}

impl Val {
    pub fn to_readable_type(&self) -> SmolStr {
        match self {
            Val::Unit => "Unit",
            Val::Bool(_) => "Bool",
            Val::String(_) => "String",
            Val::Number(_) => "Number",
            Val::Decimal(_) => "Decimal",
            Val::List(_) => "List",
            Val::Struct(_) => "Struct",
            Val::Function(_, _, _) => "Function",
            Val::NativeFunction(_) => "Builtin",
        }
        .into()
    }

    pub fn as_num(&self) -> isize {
        match self {
            Val::Number(i) => *i,
            x => panic!("not a number! {:?}", x),
        }
    }
}

impl WithTrivia for Trivia<Val> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        match &self.inner {
            Val::Unit => format!("{buffer}() {}..{}", self.span.start, self.span.end),
            Val::Bool(bool) => format!("{buffer}{bool} {}..{}", self.span.start, self.span.end),
            Val::String(str) => {
                format!("{buffer}\"{str}\" {}..{}", self.span.start, self.span.end)
            }
            Val::Number(number) => {
                format!("{buffer}{number} {}..{}", self.span.start, self.span.end)
            }
            Val::Decimal(number) => {
                format!(
                    "{buffer}{} {}..{}",
                    number.to_f32(),
                    self.span.start,
                    self.span.end
                )
            }
            Val::List(list) => {
                let line = format!("{buffer}List {}..{}", self.span.start, self.span.end);
                let mut lines: Vec<String> =
                    list.iter().map(|x| x.pretty_string(indent + 2)).collect();
                lines.insert(0, line);

                lines.join("\n")
            }
            Val::Struct(map) => {
                let line = format!("{buffer}Struct {}..{}", self.span.start, self.span.end);
                let mut lines: Vec<String> = map
                    .iter()
                    .flat_map(|(k, v)| {
                        [
                            format!("{buffer}  Entry {}..{}", k.span.start, v.span.end),
                            tagged_pretty_string(
                                &k.inner,
                                "Key",
                                k.span.start,
                                k.span.end,
                                indent + 4,
                            ),
                            v.pretty_string(indent + 4),
                        ]
                    })
                    .collect();
                lines.insert(0, line);

                lines.join("\n")
            }
            Val::Function(_, ident, rest) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let ident = tagged_pretty_string(
                    &ident.inner,
                    "Param",
                    ident.span.start,
                    ident.span.end,
                    indent + 2,
                );
                let rest = rest.pretty_string(indent + 2);
                let line = format!("{buffer}Func {}..{}", self.span.start, self.span.end);

                vec![line, ident, rest].join("\n")
            }
            Val::NativeFunction(NativeFunction { name, .. }) => {
                format!(
                    "{buffer}Builtin({name}) {}..{}",
                    self.span.start, self.span.end
                )
            }
        }
    }
}
