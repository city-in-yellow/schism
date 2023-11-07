use archery::ArcK;
use errors::InterpretingError;
use fixed::{types::extra::U24, FixedI32};
use rpds::{HashTrieMap, List};
use smol_str::SmolStr;

use crate::{
    ast::{tagged_pretty_string, TExpr},
    trivia::{Range, Trivia, WithTrivia},
};

#[derive(Debug, Eq, PartialEq)]
pub struct Context {
    pub builtins: HashTrieMap<SmolStr, TVal, ArcK>,
    pub environment: HashTrieMap<SmolStr, TVal, ArcK>,
}

impl Context {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            builtins: HashTrieMap::new_sync(),
            environment: HashTrieMap::new_sync(),
        }
    }

    pub fn with(&self, key: SmolStr, val: TVal) -> Self {
        Self {
            builtins: self.builtins.clone(),
            environment: self.environment.insert(key, val),
        }
    }

    pub fn find(&self, key: &SmolStr) -> Option<&TVal> {
        self.builtins.get(key).or_else(|| self.environment.get(key))
    }

    pub fn merge(&self, other: &Self) -> Self {
        let mut next_environment: HashTrieMap<SmolStr, Trivia<Val>, ArcK> =
            other.environment.clone();
        for key in self.environment.keys() {
            if next_environment.contains_key(key) {
                continue;
            }

            next_environment =
                next_environment.insert(key.clone(), self.environment.get(key).unwrap().clone());
        }

        Self {
            builtins: self.builtins.clone(),
            environment: next_environment,
        }
    }
}

impl Clone for Context {
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
    pub params: List<TVal, ArcK>,
    pub apply: fn(Context, Range, List<TVal, ArcK>, TVal) -> RFunc,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Val {
    Unit,
    Bool(bool),
    String(SmolStr),
    Number(isize),
    Decimal(FixedI32<U24>),
    List(Vec<TVal>),
    Function(Context, Trivia<SmolStr>, TExpr),
    NativeFunction(NativeFunction),
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
            Val::Function(_, _, _) => "Function",
            Val::NativeFunction(_) => "Builtin",
        }
        .into()
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
                format!("{buffer}{number} {}..{}", self.span.start, self.span.end)
            }
            Val::List(list) => {
                let line = format!("{buffer}List {}..{}", self.span.start, self.span.end);
                let mut lines: Vec<String> =
                    list.iter().map(|x| x.pretty_string(indent + 2)).collect();
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
