use ast::{
    ast::{Atom, Expr, Infix, ParameterList, Postfix, Prefix},
    trivia::{new, Range, Trivia},
};
use pest::iterators::{Pair, Pairs};
use pratt::{Affix, Associativity, PrattParser, Precedence, Result};
use smol_str::SmolStr;

use crate::Rule;

pub struct ExprParser;

impl ExprParser {
    fn span(&mut self, pair: &Pair<'_, Rule>) -> Range {
        Range {
            start: pair.as_span().start(),
            end: pair.as_span().end(),
        }
    }

    fn smol_str(&mut self, pair: Pair<'_, Rule>) -> Trivia<SmolStr> {
        let span = self.span(&pair);

        new(pair.as_str().into(), span)
    }

    fn unit(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let atom = Expr::Atom(new(Atom::Unit, span));

        new(Box::new(atom), span)
    }

    fn ident(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let atom = Expr::Atom(new(Atom::Ident(pair.as_str().into()), span));

        new(Box::new(atom), span)
    }

    fn number(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let atom = Expr::Atom(new(Atom::Number(pair.as_str().parse().unwrap()), span));

        new(Box::new(atom), span)
    }

    fn die(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();
        let inner = match pairs.len() {
            1 => {
                let base = pairs.next().unwrap().as_str().parse().unwrap();
                Expr::Atom(new(Atom::Die(1, base), span))
            }
            2 => {
                let quantity = pairs.next().unwrap().as_str().parse().unwrap();
                let base = pairs.next().unwrap().as_str().parse().unwrap();
                Expr::Atom(new(Atom::Die(quantity, base), span))
            }
            _ => unreachable!(),
        };

        new(Box::new(inner), span)
    }

    fn list(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let atom = Atom::List(pair.into_inner().map(|x| self.expr(x)).collect());
        let list = Expr::Atom(new(atom, span));

        new(Box::new(list), span)
    }

    fn expr(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        self.parse(&mut pair.into_inner()).unwrap()
    }

    fn assignment(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();

        let ident = self.smol_str(pairs.next().unwrap());
        let expr = self.expr(pairs.next().unwrap());
        let next = self.expr(pairs.next().unwrap());

        new(Box::new(Expr::Assignment(ident, expr, next)), span)
    }

    fn param_list(&mut self, pair: Pair<'_, Rule>) -> Trivia<ParameterList> {
        let span = self.span(&pair);
        let params = pair.into_inner().map(|x| self.smol_str(x)).collect();

        new(params, span)
    }

    fn func_assignment(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();

        let ident = self.smol_str(pairs.next().unwrap());
        let params = self.param_list(pairs.next().unwrap());
        let expr = self.expr(pairs.next().unwrap());
        let next = self.expr(pairs.next().unwrap());

        new(
            Box::new(Expr::FunctionAssignment(ident, params, expr, next)),
            span,
        )
    }

    fn func_call_list(&mut self, pair: Pair<'_, Rule>) -> Vec<Trivia<Box<Expr>>> {
        pair.into_inner().map(|x| self.expr(x)).collect()
    }

    fn func_call(&mut self, pair: Pair<'_, Rule>) -> Trivia<Box<Expr>> {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();

        let ident = self.smol_str(pairs.next().unwrap());
        let exprs = self.func_call_list(pairs.next().unwrap());

        new(Box::new(Expr::FunctionCall(ident, exprs)), span)
    }
}

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
                | Rule::function_assignment
                | Rule::function_call
                | Rule::function_call_list
                | Rule::param_list
                | Rule::assignment
                | Rule::list
                | Rule::empty_list
                | Rule::poplated_list
                | Rule::die
                | Rule::number
                | Rule::unit
                | Rule::ident
                | Rule::entry
                | Rule::EOI,
                _,
            ) => Affix::Nilfix,
            (r, s) => panic!("should be unreachable => {:?} {}", r, s),
        };
        Ok(affix)
    }

    fn primary(&mut self, tree: Self::Input) -> Result<Trivia<Box<Expr>>> {
        let expr = match tree.as_rule() {
            Rule::unit => self.unit(tree),
            Rule::ident => self.ident(tree),
            Rule::number => self.number(tree),
            Rule::die => self.die(tree),
            Rule::list => self.list(tree),
            Rule::group => self.expr(tree),
            Rule::assignment => self.assignment(tree),
            Rule::function_assignment => self.func_assignment(tree),
            Rule::function_call => self.func_call(tree),
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
        let span = Range {
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
            Range {
                start: lhs.span.start,
                end: rhs.span.end,
            },
        ))
    }

    fn prefix(&mut self, tree: Self::Input, rhs: Trivia<Box<Expr>>) -> Result<Trivia<Box<Expr>>> {
        let span = Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let op = match tree.as_str() {
            "!" => Prefix::Bang,
            "-" => Prefix::Minus,
            _ => unreachable!(),
        };

        Ok(new(
            Box::new(Expr::PrefixExpr(new(op, span), rhs.clone())),
            Range {
                start: span.start,
                end: rhs.span.end,
            },
        ))
    }

    fn postfix(&mut self, lhs: Trivia<Box<Expr>>, tree: Self::Input) -> Result<Trivia<Box<Expr>>> {
        let span = Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let op = match tree.as_str() {
            "?" => Postfix::Question,
            _ => unreachable!(),
        };

        Ok(new(
            Box::new(Expr::PostfixExpr(lhs.clone(), new(op, span))),
            Range {
                start: lhs.span.start,
                end: span.end,
            },
        ))
    }
}

pub fn parse_pratt(source: Pairs<'_, Rule>) -> Trivia<Box<Expr>> {
    ExprParser.parse(source).unwrap()
}
