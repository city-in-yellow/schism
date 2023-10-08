use ast::{Atom, Expr, Infix, Postfix, Prefix};
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
    type Output = Expr;

    fn query(&mut self, tree: &Self::Input) -> Result<Affix> {
        let affix = match (tree.as_rule(), tree.as_str()) {
            (Rule::infix, "=") => Affix::Infix(Precedence(2), Associativity::Neither),
            (Rule::infix, "+" | "-") => Affix::Infix(Precedence(3), Associativity::Left),
            (Rule::infix, "*" | "/") => Affix::Infix(Precedence(4), Associativity::Left),
            (Rule::postfix, "?") => Affix::Postfix(Precedence(5)),
            (Rule::prefix, "-" | "!") => Affix::Prefix(Precedence(6)),
            (Rule::infix, "^") => Affix::Infix(Precedence(7), Associativity::Right),
            (Rule::group | Rule::assignment | Rule::die | Rule::number | Rule::ident, _) => {
                Affix::Nilfix
            }
            _ => unreachable!(),
        };
        Ok(affix)
    }

    fn primary(&mut self, tree: Self::Input) -> Result<Expr> {
        let expr = match tree.as_rule() {
            Rule::ident => Expr::Atom(Atom::Ident(tree.as_str().into())),
            Rule::number => Expr::Atom(Atom::Number(tree.as_str().parse().unwrap())),
            Rule::die => {
                let mut pairs = tree.into_inner();
                match pairs.len() {
                    1 => {
                        let base = pairs.next().unwrap().as_str().parse().unwrap();
                        Expr::Atom(Atom::Die(1, base))
                    }
                    2 => {
                        let quantity = pairs.next().unwrap().as_str().parse().unwrap();
                        let base = pairs.next().unwrap().as_str().parse().unwrap();
                        Expr::Atom(Atom::Die(quantity, base))
                    }
                    _ => unreachable!(),
                }
            }
            Rule::group => self.parse(&mut tree.into_inner()).unwrap(),
            Rule::assignment => {
                let mut pairs = tree.into_inner();

                let ident = pairs.next().unwrap().as_str();
                let expr = self.parse(&mut pairs.next().unwrap().into_inner()).unwrap();
                let next = self.parse(&mut pairs.next().unwrap().into_inner()).unwrap();
                Expr::Assignment(ident.into(), Box::new(expr), Box::new(next))
            }
            _ => unreachable!(),
        };
        Ok(expr)
    }

    fn infix(&mut self, lhs: Expr, tree: Self::Input, rhs: Expr) -> Result<Expr> {
        let op = match tree.as_str() {
            "+" => Infix::Plus,
            "-" => Infix::Minus,
            "*" => Infix::Star,
            "/" => Infix::Slash,
            "^" => Infix::Caret,
            "=" => Infix::Equal,
            _ => unreachable!(),
        };
        Ok(Expr::InfixExpr(Box::new(lhs), op, Box::new(rhs)))
    }

    fn prefix(&mut self, tree: Self::Input, rhs: Expr) -> Result<Expr> {
        let op = match tree.as_str() {
            "!" => Prefix::Bang,
            "-" => Prefix::Minus,
            _ => unreachable!(),
        };
        Ok(Expr::PrefixExpr(op, Box::new(rhs)))
    }

    fn postfix(&mut self, lhs: Expr, tree: Self::Input) -> Result<Expr> {
        let op = match tree.as_str() {
            "?" => Postfix::Question,
            _ => unreachable!(),
        };
        Ok(Expr::PostfixExpr(Box::new(lhs), op))
    }
}
