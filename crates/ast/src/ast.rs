use std::collections::BTreeMap;

use smol_str::SmolStr;

use crate::trivia::{Trivia, WithTrivia};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Atom {
    Unit,
    Bool(bool),
    Ident(SmolStr),
    String(SmolStr),
    Number(isize),
    Die(usize, usize),
    List(Vec<TExpr>),
    Struct(BTreeMap<Trivia<SmolStr>, TExpr>),
}

impl WithTrivia for Trivia<Atom> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        match &self.inner {
            Atom::Unit => format!("{buffer}() {}..{}", self.span.start, self.span.end),
            Atom::Bool(bool) => format!("{buffer}{bool} {}..{}", self.span.start, self.span.end),
            Atom::Ident(ident) => format!("{buffer}{ident} {}..{}", self.span.start, self.span.end),
            Atom::String(str) => {
                format!("{buffer}\"{str}\" {}..{}", self.span.start, self.span.end)
            }
            Atom::Number(number) => {
                format!("{buffer}{number} {}..{}", self.span.start, self.span.end)
            }
            Atom::Die(quantity, base) => format!(
                "{buffer}{quantity}d{base} {}..{}",
                self.span.start, self.span.end
            ),
            Atom::List(list) => {
                let line = format!("{buffer}List {}..{}", self.span.start, self.span.end);
                let mut lines: Vec<String> =
                    list.iter().map(|x| x.pretty_string(indent + 2)).collect();
                lines.insert(0, line);

                lines.join("\n")
            }
            Atom::Struct(map) => {
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
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Equal,
    And,
    Or,
    Gt,
    Gte,
    Lt,
    Lte,
    RangeEx,
    RangeInc,
}

impl WithTrivia for Trivia<Infix> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        match &self.inner {
            Infix::Plus => format!("{buffer}`+` {}..{}", self.span.start, self.span.end),
            Infix::Minus => format!("{buffer}`-` {}..{}", self.span.start, self.span.end),
            Infix::Star => format!("{buffer}`*` {}..{}", self.span.start, self.span.end),
            Infix::Slash => format!("{buffer}`/` {}..{}", self.span.start, self.span.end),
            Infix::Caret => format!("{buffer}`^` {}..{}", self.span.start, self.span.end),
            Infix::Equal => format!("{buffer}`=` {}..{}", self.span.start, self.span.end),
            Infix::And => format!("{buffer}`and` {}..{}", self.span.start, self.span.end),
            Infix::Or => format!("{buffer}`or` {}..{}", self.span.start, self.span.end),
            Infix::Gt => format!("{buffer}`>` {}..{}", self.span.start, self.span.end),
            Infix::Gte => format!("{buffer}`>=` {}..{}", self.span.start, self.span.end),
            Infix::Lt => format!("{buffer}`<` {}..{}", self.span.start, self.span.end),
            Infix::Lte => format!("{buffer}`<=` {}..{}", self.span.start, self.span.end),
            Infix::RangeEx => format!("{buffer}`...` {}..{}", self.span.start, self.span.end),
            Infix::RangeInc => format!("{buffer}`..=` {}..{}", self.span.start, self.span.end),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Prefix {
    Minus,
    Bang,
}

impl WithTrivia for Trivia<Prefix> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        match &self.inner {
            Prefix::Minus => format!("{buffer}`-` {}..{}", self.span.start, self.span.end),
            Prefix::Bang => format!("{buffer}`!` {}..{}", self.span.start, self.span.end),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Postfix {
    Question,
}

impl WithTrivia for Trivia<Postfix> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        match &self.inner {
            Postfix::Question => format!("{buffer}`?` {}..{}", self.span.start, self.span.end),
        }
    }
}

pub type ParameterList = Vec<Trivia<SmolStr>>;

impl WithTrivia for Trivia<ParameterList> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        let line = format!("{buffer}ParamList {}..{}", self.span.start, self.span.end);
        let mut lines: Vec<String> = self
            .inner
            .iter()
            .map(|param| {
                tagged_pretty_string(
                    &param.inner,
                    "Param",
                    param.span.start,
                    param.span.end,
                    indent + 2,
                )
            })
            .collect();

        lines.insert(0, line);

        lines.join("\n")
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Case {
    pub predicate: TExpr,
    pub expr: TExpr,
}

impl WithTrivia for Trivia<Case> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        let case = format!("{buffer}Case {}..{}", self.span.start, self.span.end);
        let predicate = self.inner.predicate.pretty_string(indent + 2);
        let expr = self.inner.expr.pretty_string(indent + 2);

        vec![case, predicate, expr].join("\n")
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
    FunctionBody(Trivia<SmolStr>, TExpr),
    FunctionCall(TExpr, TExpr),
    Assignment(TExpr, Trivia<SmolStr>, TExpr),
    Match(Vec<Trivia<Case>>),
    For(TExpr, Trivia<SmolStr>, TExpr),
    Atom(Trivia<Atom>),
}

pub type TExpr = Trivia<Box<Expr>>;

impl WithTrivia for TExpr {
    fn pretty_string(&self, indent: usize) -> String {
        match &self.inner {
            box Expr::Atom(atom) => atom.pretty_string(indent),
            box Expr::FunctionBody(ident, rest) => {
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
            box Expr::FunctionCall(function, argument) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let line = format!("{buffer}FuncCall {}..{}", self.span.start, self.span.end);
                let function = function.pretty_string(indent + 2);
                let argument = argument.pretty_string(indent + 2);

                vec![line, function, argument].join("\n")
            }
            box Expr::Assignment(expr, ident, next) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let expr = expr.pretty_string(indent + 2);
                let ident = tagged_pretty_string(
                    &ident.inner,
                    "Name",
                    ident.span.start,
                    ident.span.end,
                    indent + 2,
                );
                let next = next.pretty_string(indent + 2);

                let line = format!("{buffer}Assign {}..{}", self.span.start, self.span.end);

                vec![line, expr, ident, next].join("\n")
            }
            box Expr::Match(cases) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let mut cases = cases
                    .iter()
                    .map(|x| x.pretty_string(indent + 2))
                    .collect::<Vec<String>>();
                let line = format!("{buffer}Match {}..{}", self.span.start, self.span.end);
                cases.insert(0, line);

                cases.join("\n")
            }
            box Expr::For(range, ident, expr) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let range = range.pretty_string(indent + 2);
                let ident = tagged_pretty_string(
                    &ident.inner,
                    "Name",
                    ident.span.start,
                    ident.span.end,
                    indent + 2,
                );
                let expr = expr.pretty_string(indent + 2);
                let line = format!("{buffer}For {}..{}", self.span.start, self.span.end);

                vec![line, range, ident, expr].join("\n")
            }
        }
    }
}

pub fn tagged_pretty_string(
    s: &SmolStr,
    tag: &str,
    start: usize,
    end: usize,
    indent: usize,
) -> String {
    let buffer = String::from_utf8(vec![b' '; indent]).unwrap();
    format!("{buffer}{tag}({s}) {}..{}", start, end)
}
