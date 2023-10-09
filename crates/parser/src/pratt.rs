use ast::{
    ast::{Atom, Expr, Infix, Postfix, Prefix},
    trivia::{new, Trivia},
};
use pest::iterators::Pair;
use pratt::{Affix, Associativity, PrattParser, Precedence, Result};

use crate::Rule;

pub struct ExprParser;

impl<'i, I> PrattParser<I> for ExprParser
where
    I: Iterator<Item = Pair<'i, Rule>>,
{
    type Error = pratt::NoError;
    type Input = Pair<'i, Rule>;
    type Output = Trivia<Box<Expr>>;

    fn query(&mut self, tree: &Self::Input) -> Result<Affix> {
        let affix = match (tree.as_rule(), tree.as_str()) {
            (Rule::infix, "=") => Affix::Infix(Precedence(2), Associativity::Neither),
            (Rule::infix, "+" | "-") => Affix::Infix(Precedence(3), Associativity::Left),
            (Rule::infix, "*" | "/") => Affix::Infix(Precedence(4), Associativity::Left),
            (Rule::postfix, "?") => Affix::Postfix(Precedence(5)),
            (Rule::prefix, "-" | "!") => Affix::Prefix(Precedence(6)),
            (Rule::infix, "^") => Affix::Infix(Precedence(7), Associativity::Right),
            (
                Rule::group
                | Rule::assignment
                | Rule::list
                | Rule::die
                | Rule::number
                | Rule::ident,
                _,
            ) => Affix::Nilfix,
            _ => unreachable!(),
        };
        Ok(affix)
    }

    fn primary(&mut self, tree: Self::Input) -> Result<Trivia<Box<Expr>>> {
        let span = std::ops::Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let expr = match tree.as_rule() {
            Rule::ident => new(
                Box::new(Expr::Atom(new(
                    Atom::Ident(tree.as_str().into()),
                    span.clone(),
                ))),
                span,
            ),
            Rule::number => new(
                Box::new(Expr::Atom(new(
                    Atom::Number(tree.as_str().parse().unwrap()),
                    span.clone(),
                ))),
                span,
            ),
            Rule::die => {
                let mut pairs = tree.into_inner();
                let inner = match pairs.len() {
                    1 => {
                        let base = pairs.next().unwrap().as_str().parse().unwrap();
                        Expr::Atom(new(Atom::Die(1, base), span.clone()))
                    }
                    2 => {
                        let quantity = pairs.next().unwrap().as_str().parse().unwrap();
                        let base = pairs.next().unwrap().as_str().parse().unwrap();
                        Expr::Atom(new(Atom::Die(quantity, base), span.clone()))
                    }
                    _ => unreachable!(),
                };

                new(Box::new(inner), span)
            }
            Rule::list => new(
                Box::new(Expr::Atom(new(
                    Atom::List(
                        tree.into_inner()
                            .map(|x| self.parse(x.into_inner()).unwrap())
                            .collect(),
                    ),
                    span.clone(),
                ))),
                span,
            ),
            Rule::group => self.parse(&mut tree.into_inner()).unwrap(),
            Rule::assignment => {
                let mut pairs = tree.into_inner();

                let ident = {
                    let pair = pairs.next().unwrap();
                    let span = std::ops::Range {
                        start: pair.as_span().start(),
                        end: pair.as_span().end(),
                    };
                    new(pair.as_str().into(), span)
                };
                let expr = { self.parse(&mut pairs.next().unwrap().into_inner()).unwrap() };
                let next = { self.parse(&mut pairs.next().unwrap().into_inner()).unwrap() };
                new(Box::new(Expr::Assignment(ident, expr, next)), span)
            }
            _ => unreachable!(),
        };
        Ok(expr)
    }

    fn infix(
        &mut self,
        lhs: Trivia<Box<Expr>>,
        tree: Self::Input,
        rhs: Trivia<Box<Expr>>,
    ) -> Result<Trivia<Box<Expr>>> {
        let span = std::ops::Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let op = match tree.as_str() {
            "+" => Infix::Plus,
            "-" => Infix::Minus,
            "*" => Infix::Star,
            "/" => Infix::Slash,
            "^" => Infix::Caret,
            "=" => Infix::Equal,
            _ => unreachable!(),
        };

        Ok(new(
            Box::new(Expr::InfixExpr(lhs.clone(), new(op, span), rhs.clone())),
            std::ops::Range {
                start: lhs.span.start,
                end: rhs.span.end,
            },
        ))
    }

    fn prefix(&mut self, tree: Self::Input, rhs: Trivia<Box<Expr>>) -> Result<Trivia<Box<Expr>>> {
        let span = std::ops::Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let op = match tree.as_str() {
            "!" => Prefix::Bang,
            "-" => Prefix::Minus,
            _ => unreachable!(),
        };

        Ok(new(
            Box::new(Expr::PrefixExpr(new(op, span.clone()), rhs.clone())),
            std::ops::Range {
                start: span.start,
                end: rhs.span.end,
            },
        ))
    }

    fn postfix(&mut self, lhs: Trivia<Box<Expr>>, tree: Self::Input) -> Result<Trivia<Box<Expr>>> {
        let span = std::ops::Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let op = match tree.as_str() {
            "?" => Postfix::Question,
            _ => unreachable!(),
        };

        Ok(new(
            Box::new(Expr::PostfixExpr(lhs.clone(), new(op, span.clone()))),
            std::ops::Range {
                start: lhs.span.start,
                end: span.end,
            },
        ))
    }
}
