use smol_str::SmolStr;

use crate::trivia::{Trivia, WithTrivia};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Atom {
    Ident(SmolStr),
    Number(usize),
    Die(usize, usize),
    List(Vec<Trivia<Box<Expr>>>),
}

impl WithTrivia for Trivia<Atom> {
    fn pretty_string(&self, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();

        match &self.inner {
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
    InfixExpr(Trivia<Box<Expr>>, Trivia<Infix>, Trivia<Box<Expr>>),
    PrefixExpr(Trivia<Prefix>, Trivia<Box<Expr>>),
    PostfixExpr(Trivia<Box<Expr>>, Trivia<Postfix>),
    Assignment(Trivia<SmolStr>, Trivia<Box<Expr>>, Trivia<Box<Expr>>),
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

                let child_buffer = String::from_utf8(vec![b' '; indent + 2]).unwrap();
                let ident = format!(
                    "{child_buffer}LValue({}) {}..{}",
                    ident.inner, ident.span.start, ident.span.end
                );

                let expr = expr.pretty_string(indent + 2);
                let rest = rest.pretty_string(indent + 2);
                let line = format!("{buffer}Assign {}..{}", self.span.start, self.span.end);

                vec![line, ident, expr, rest].join("\n")
            }
        }
    }
}
