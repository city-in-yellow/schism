use smol_str::SmolStr;

use crate::trivia::{Trivia, WithTrivia};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Atom {
    Unit,
    Ident(SmolStr),
    Number(usize),
    Die(usize, usize),
    List(Vec<Trivia<Box<Expr>>>),
}

impl WithTrivia for Trivia<Atom> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        match &self.inner {
            Atom::Unit => format!("{buffer}() {}..{}", self.span.start, self.span.end),
            Atom::Ident(ident) => format!("{buffer}{ident} {}..{}", self.span.start, self.span.end),
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
pub enum Expr {
    InfixExpr(Trivia<Box<Expr>>, Trivia<Infix>, Trivia<Box<Expr>>),
    PrefixExpr(Trivia<Prefix>, Trivia<Box<Expr>>),
    PostfixExpr(Trivia<Box<Expr>>, Trivia<Postfix>),
    Assignment(Trivia<SmolStr>, Trivia<Box<Expr>>, Trivia<Box<Expr>>),
    FunctionAssignment(
        Trivia<SmolStr>,
        Trivia<ParameterList>,
        Trivia<Box<Expr>>,
        Trivia<Box<Expr>>,
    ),
    FunctionCall(Trivia<SmolStr>, Vec<Trivia<Box<Expr>>>),
    Atom(Trivia<Atom>),
}

impl WithTrivia for Trivia<Box<Expr>> {
    fn pretty_string(&self, indent: usize) -> String {
        match &self.inner {
            box Expr::Atom(atom) => atom.pretty_string(indent),
            box Expr::InfixExpr(lhs, op, rhs) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();
                let lhs = lhs.pretty_string(indent + 2);
                let op = op.pretty_string(indent + 2);
                let rhs = rhs.pretty_string(indent + 2);
                let line = format!("{buffer}Expr {}..{}", self.span.start, self.span.end);

                vec![line, lhs, op, rhs].join("\n")
            }
            box Expr::PrefixExpr(op, rhs) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();
                let op = op.pretty_string(indent + 2);
                let rhs = rhs.pretty_string(indent + 2);
                let line = format!("{buffer}Expr {}..{}", self.span.start, self.span.end);

                vec![line, op, rhs].join("\n")
            }
            box Expr::PostfixExpr(lhs, op) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();
                let lhs = lhs.pretty_string(indent + 2);
                let op = op.pretty_string(indent + 2);
                let line = format!("{buffer}Expr {}..{}", self.span.start, self.span.end);

                vec![line, lhs, op].join("\n")
            }
            box Expr::Assignment(ident, expr, rest) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let ident = tagged_pretty_string(
                    &ident.inner,
                    "Name",
                    ident.span.start,
                    ident.span.end,
                    indent + 2,
                );
                let expr = expr.pretty_string(indent + 2);
                let rest = rest.pretty_string(indent + 2);
                let line = format!("{buffer}Assign {}..{}", self.span.start, self.span.end);

                vec![line, ident, expr, rest].join("\n")
            }
            box Expr::FunctionAssignment(ident, parameters, expr, rest) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let ident = tagged_pretty_string(
                    &ident.inner,
                    "Name",
                    ident.span.start,
                    ident.span.end,
                    indent + 2,
                );
                let params = parameters.pretty_string(indent + 2);
                let expr = expr.pretty_string(indent + 2);
                let rest = rest.pretty_string(indent + 2);
                let line = format!("{buffer}FuncAssign {}..{}", self.span.start, self.span.end);

                vec![line, ident, params, expr, rest].join("\n")
            }
            box Expr::FunctionCall(ident, arguments) => {
                let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

                let ident = tagged_pretty_string(
                    &ident.inner,
                    "Name",
                    ident.span.start,
                    ident.span.end,
                    indent + 2,
                );

                let line = format!("{buffer}FuncCall {}..{}", self.span.start, self.span.end);
                let mut lines: Vec<String> = arguments
                    .iter()
                    .map(|x| x.pretty_string(indent + 2))
                    .collect();
                lines.insert(0, ident);
                lines.insert(0, line);

                lines.join("\n")
            }
        }
    }
}

fn tagged_pretty_string(s: &SmolStr, tag: &str, start: usize, end: usize, indent: usize) -> String {
    let buffer = String::from_utf8(vec![b' '; indent]).unwrap();
    format!("{buffer}{tag}({s}) {}..{}", start, end)
}
