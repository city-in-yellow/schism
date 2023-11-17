use std::collections::LinkedList;

use ast::{
    ast::{Atom, Case, Expr, ParameterList, TExpr},
    trivia::{new, Range, Trivia},
};
use pest::iterators::{Pair, Pairs};
use pratt::{Affix, Associativity, PrattParser, Precedence, Result};
use smol_str::SmolStr;

use crate::Rule;

fn newb<T>(t: T, span: Range) -> Trivia<Box<T>> {
    new(Box::new(t), span)
}

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

    fn unit(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let atom = Expr::Atom(new(Atom::Unit, span));

        newb(atom, span)
    }

    fn ident(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let atom = Expr::Atom(new(Atom::Ident(pair.as_str().into()), span));

        newb(atom, span)
    }

    fn string(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let pair = pair.into_inner().next().unwrap();
        let atom = Expr::Atom(new(Atom::String(pair.as_str().into()), span));

        newb(atom, span)
    }

    fn number(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let atom = Expr::Atom(new(Atom::Number(pair.as_str().parse().unwrap()), span));

        newb(atom, span)
    }

    fn die(&mut self, pair: Pair<'_, Rule>) -> TExpr {
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

        newb(inner, span)
    }

    fn list(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let atom = Atom::List(pair.into_inner().map(|x| self.expr(x)).collect());
        let list = Expr::Atom(new(atom, span));

        newb(list, span)
    }

    fn struct_entry(&mut self, pair: Pair<'_, Rule>) -> (Trivia<SmolStr>, TExpr) {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();

        let ident = self.smol_str(pairs.next().unwrap());
        let val = self.expr(pairs.next().unwrap());

        (ident, val)
    }

    fn struct_body(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let atom = Atom::Struct(pair.into_inner().map(|x| self.struct_entry(x)).collect());
        let map = Expr::Atom(new(atom, span));

        newb(map, span)
    }

    fn expr(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        self.parse(&mut pair.into_inner()).unwrap()
    }

    fn assignment(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();

        let ident = self.smol_str(pairs.next().unwrap());
        let expr = self.expr(pairs.next().unwrap());
        let next = self.expr(pairs.next().unwrap());

        newb(Expr::Assignment(expr, ident, next), span)
    }

    fn case(&mut self, pair: Pair<'_, Rule>) -> Trivia<Case> {
        let span = self.span(&pair);

        let (predicate, expr) = match pair.as_rule() {
            Rule::case | Rule::if_head => {
                let mut pairs = pair.into_inner();

                let predicate = self.expr(pairs.next().unwrap());
                let expr = self.expr(pairs.next().unwrap());
                (predicate, expr)
            }
            Rule::always_case => {
                let mut pairs = pair.into_inner();

                let predicate = newb(Expr::Atom(new(Atom::Bool(true), span)), span);
                let expr = self.expr(pairs.next().unwrap());
                (predicate, expr)
            }
            _ => unreachable!(),
        };

        new(Case { predicate, expr }, span)
    }

    fn match_expr(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let cases = pair.into_inner().map(|pair| self.case(pair)).collect();

        newb(Expr::Match(cases), span)
    }

    fn param_list(&mut self, pair: Pair<'_, Rule>) -> Trivia<ParameterList> {
        let span = self.span(&pair);
        let params = pair.into_inner().map(|x| self.smol_str(x)).collect();

        new(params, span)
    }

    fn structure_func_assignment(mut idents: LinkedList<Trivia<SmolStr>>, next: TExpr) -> TExpr {
        match idents.len() {
            0 => unreachable!(),
            1 => {
                let ident = idents.pop_front().unwrap();
                let span = Range {
                    start: ident.span.start,
                    end: next.span.end,
                };

                newb(Expr::FunctionBody(ident, next), span)
            }
            _ => {
                let hd = idents.pop_front().unwrap();
                let span = Range {
                    start: hd.span.start,
                    end: next.span.end,
                };

                let func_body =
                    Expr::FunctionBody(hd, ExprParser::structure_func_assignment(idents, next));
                newb(func_body, span)
            }
        }
    }

    fn func_assignment(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();

        let ident = self.smol_str(pairs.next().unwrap());
        let params = self.param_list(pairs.next().unwrap());
        let expr = self.expr(pairs.next().unwrap());
        let next = self.expr(pairs.next().unwrap());

        let params = params.inner.into_iter().collect();
        let inner = ExprParser::structure_func_assignment(params, expr);
        newb(Expr::Assignment(inner, ident, next), span)
    }

    fn func_call_list(&mut self, pair: Pair<'_, Rule>) -> Vec<TExpr> {
        let mut pairs = pair.into_inner();
        match pairs.len() {
            1 => vec![self.expr(pairs.next().unwrap())],
            2 => {
                let lhs = self.expr(pairs.next().unwrap());
                let mut vals = self.func_call_list(pairs.next().unwrap());
                vals.insert(0, lhs);

                vals
            }
            _ => unreachable!(),
        }
    }

    fn structure_func_call(mut exprs: LinkedList<TExpr>) -> TExpr {
        match exprs.len() {
            0 => unreachable!(),
            1 => exprs.pop_front().unwrap(),
            _ => {
                let hd = exprs.pop_front().unwrap();
                let span = Range {
                    start: hd.span.start,
                    end: exprs.back().unwrap().span.end,
                };

                let func_call = Expr::FunctionCall(hd, ExprParser::structure_func_call(exprs));
                newb(func_call, span)
            }
        }
    }

    fn order_func_call(expr: TExpr) -> TExpr {
        match expr.inner {
            box Expr::FunctionCall(
                lhs,
                Trivia {
                    inner: box Expr::FunctionCall(i_lhs, i_rhs),
                    ..
                },
            ) => {
                let i_span = Range {
                    start: expr.span.start,
                    end: i_lhs.span.end,
                };

                let i_lhs = ExprParser::order_func_call(i_lhs);
                let i_rhs = ExprParser::order_func_call(i_rhs);

                let i_func = newb(Expr::FunctionCall(lhs, i_lhs), i_span);
                let i_func = ExprParser::order_func_call(i_func);
                ExprParser::order_func_call(new(
                    Box::new(Expr::FunctionCall(i_func, i_rhs)),
                    expr.span,
                ))
            }
            _ => expr,
        }
    }

    fn func_call(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let mut pairs = pair.into_inner();

        let ident = {
            let ident = self.smol_str(pairs.next().unwrap());
            new(
                Box::new(Expr::Atom(new(Atom::Ident(ident.inner), ident.span))),
                ident.span,
            )
        };
        let mut exprs = self.func_call_list(pairs.next().unwrap());
        exprs.insert(0, ident);
        let exprs = exprs.into_iter().collect::<LinkedList<_>>();

        ExprParser::order_func_call(ExprParser::structure_func_call(exprs))
    }

    fn for_expr(&mut self, pair: Pair<'_, Rule>) -> TExpr {
        let span = self.span(&pair);
        let mut pairs = pair.into_inner();

        let range = self.expr(pairs.next().unwrap());
        let ident = self.smol_str(pairs.next().unwrap());
        let expr = self.expr(pairs.next().unwrap());

        newb(Expr::For(range, ident, expr), span)
    }
}

impl<'i, I> PrattParser<I> for ExprParser
where
    I: Iterator<Item = Pair<'i, Rule>>,
{
    type Error = pratt::NoError;
    type Input = Pair<'i, Rule>;
    type Output = TExpr;

    fn query(&mut self, tree: &Self::Input) -> Result<Affix> {
        let affix = match (tree.as_rule(), tree.as_str()) {
            (Rule::infix, "=" | "and" | "or" | ">" | ">=" | "<" | "<=") => {
                Affix::Infix(Precedence(2), Associativity::Neither)
            }
            (Rule::infix, "+" | "-") => Affix::Infix(Precedence(3), Associativity::Left),
            (Rule::infix, "*" | "/") => Affix::Infix(Precedence(4), Associativity::Left),
            (Rule::postfix, "?") => Affix::Postfix(Precedence(5)),
            (Rule::prefix, "!") => Affix::Prefix(Precedence(6)),
            (Rule::infix, "^") => Affix::Infix(Precedence(7), Associativity::Right),
            (Rule::infix, "..." | "..=") => Affix::Infix(Precedence(8), Associativity::Neither),
            (
                Rule::group
                | Rule::function_assignment
                | Rule::function_call
                | Rule::param_list
                | Rule::assignment
                | Rule::if_expr
                | Rule::for_expr
                | Rule::list
                | Rule::empty_list
                | Rule::poplated_list
                | Rule::struct_body
                | Rule::die
                | Rule::number
                | Rule::string
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

    fn primary(&mut self, tree: Self::Input) -> Result<TExpr> {
        let expr = match tree.as_rule() {
            Rule::unit => self.unit(tree),
            Rule::ident => self.ident(tree),
            Rule::string => self.string(tree),
            Rule::number => self.number(tree),
            Rule::die => self.die(tree),
            Rule::list => self.list(tree),
            Rule::struct_body => self.struct_body(tree),
            Rule::group => self.expr(tree),
            Rule::assignment => self.assignment(tree),
            Rule::if_expr => self.match_expr(tree),
            Rule::function_assignment => self.func_assignment(tree),
            Rule::function_call => self.func_call(tree),
            Rule::for_expr => self.for_expr(tree),
            _ => unreachable!(),
        };
        Ok(expr)
    }

    fn infix(&mut self, lhs: TExpr, tree: Self::Input, rhs: TExpr) -> Result<TExpr> {
        let span = Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let ident = new(Atom::Ident(tree.as_str().into()), span);
        let op = newb(Expr::Atom(ident), span);

        let inner_span = Range {
            start: lhs.span.start,
            end: op.span.end,
        };

        let outer_span = Range {
            start: lhs.span.start,
            end: rhs.span.end,
        };

        let inner_func = newb(Expr::FunctionCall(op, lhs), inner_span);

        Ok(new(
            Box::new(Expr::FunctionCall(inner_func, rhs)),
            outer_span,
        ))
    }

    fn prefix(&mut self, tree: Self::Input, rhs: TExpr) -> Result<TExpr> {
        let span = Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let ident = new(Atom::Ident(tree.as_str().into()), span);
        let lhs = newb(Expr::Atom(ident), span);

        let outer_span = Range {
            start: span.start,
            end: rhs.span.end,
        };

        Ok(newb(Expr::FunctionCall(lhs, rhs), outer_span))
    }

    fn postfix(&mut self, lhs: TExpr, tree: Self::Input) -> Result<TExpr> {
        let span = Range {
            start: tree.as_span().start(),
            end: tree.as_span().end(),
        };

        let ident = new(Atom::Ident(tree.as_str().into()), span);
        let rhs = newb(Expr::Atom(ident), span);

        let outer_span = Range {
            start: lhs.span.start,
            end: span.end,
        };

        Ok(newb(Expr::FunctionCall(rhs, lhs), outer_span))
    }
}

pub fn parse_pratt(source: Pairs<'_, Rule>) -> TExpr {
    ExprParser.parse(source).unwrap()
}
