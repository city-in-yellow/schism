use ast::{
    ast::{Atom, Case, Expr, TExpr},
    hir::{Context, NativeFunction, TVal, Val},
    trivia::{new, Range, Trivia},
};
use errors::{ForError, FuncError, InterpretingError, LookupError, MatchError};
use rayon::prelude::*;
use rpds::HashTrieMap;
use smol_str::SmolStr;

type Res = Result<TVal, InterpretingError>;

pub fn eval(ctx: Context, expr: TExpr) -> Res {
    let outer_span = expr.span;
    match expr.inner {
        box Expr::Atom(atom) => eval_atom(ctx, atom),
        box Expr::FunctionBody(ident, expr) => eval_function_body(ctx, ident, expr, outer_span),
        box Expr::Assignment(expr, ident, next) => eval_assignment(ctx, expr, ident, next),
        box Expr::Match(cases) => eval_match(ctx, cases, outer_span),
        box Expr::For(range, ident, for_expr) => {
            eval_for_range(ctx, range, ident, for_expr, outer_span)
        }
        box Expr::FunctionCall(func, arg) => eval_function_call(ctx, func, arg),
    }
}

fn eval_atom(ctx: Context, atom: Trivia<Atom>) -> Res {
    let res = match atom.inner {
        Atom::Unit => new(Val::Unit, atom.span),
        Atom::Bool(bool) => new(Val::Bool(bool), atom.span),
        Atom::String(string) => new(Val::String(string), atom.span),
        Atom::Number(number) => new(Val::Number(number), atom.span),
        Atom::Die(_, _) => todo!(),
        Atom::List(list) => {
            let list = list
                .into_par_iter()
                .map(|item| eval(ctx.clone(), item))
                .collect::<Result<_, InterpretingError>>()?;
            new(Val::List(list), atom.span)
        }
        Atom::Struct(map) => {
            let map = map
                .into_par_iter()
                .map(|(k, v)| Ok((k, eval(ctx.clone(), v)?)))
                .collect::<Result<_, InterpretingError>>()?;

            new(Val::Struct(map), atom.span)
        }
        Atom::Ident(ident) => match ctx.find(&ident) {
            Some(val) => match val {
                Trivia {
                    inner: inner @ Val::NativeFunction(NativeFunction { params, .. }),
                    ..
                } => {
                    if params.is_empty() {
                        new(inner.clone(), atom.span)
                    } else {
                        val.clone()
                    }
                }
                _ => val.clone(),
            },
            None => {
                return Err(InterpretingError::LookupError(LookupError {
                    start: atom.span.start,
                    end: atom.span.end,
                }))
            }
        },
    };

    Ok(res)
}

fn eval_function_body(ctx: Context, ident: Trivia<SmolStr>, expr: TExpr, span: Range) -> Res {
    let closure = {
        let mut c = HashTrieMap::new_sync();

        for (key, value) in ctx.environment.iter() {
            c = c.insert((*key).clone(), (*value).clone());
        }

        c
    };

    Ok(new(Val::Function(closure, ident, expr), span))
}

fn eval_assignment(ctx: Context, expr: TExpr, ident: Trivia<SmolStr>, next: TExpr) -> Res {
    let arg = eval(ctx.clone(), expr)?;
    let func_ctx = ctx.with(&ident.inner, &arg);
    eval(func_ctx, next)
}

fn eval_match(ctx: Context, cases: Vec<Trivia<Case>>, outer_match: Range) -> Res {
    for case in cases.into_iter() {
        let span = case.span;
        let case = case.inner;

        let predicate = eval(ctx.clone(), case.predicate)?;

        match predicate.inner {
            Val::Bool(false) => {}
            Val::Bool(true) => return eval(ctx, case.expr),
            v => {
                return Err(InterpretingError::MatchError(MatchError {
                    match_start: outer_match.start,
                    match_end: outer_match.end,
                    predicate_start: span.start,
                    predicate_end: span.end,
                    predicate_type: v.to_readable_type(),
                }))
            }
        }
    }

    Ok(new(Val::Unit, outer_match))
}

fn eval_for_range(
    ctx: Context,
    range: TExpr,
    ident: Trivia<SmolStr>,
    expr: TExpr,
    outer_span: Range,
) -> Res {
    let range = eval(ctx.clone(), range)?;
    let items = match range.inner {
        Val::List(l) => l,
        v => {
            return Err(InterpretingError::ForError(ForError {
                for_start: outer_span.start,
                for_end: outer_span.end,
                range_start: range.span.start,
                range_end: range.span.end,
                range_type: v.to_readable_type(),
            }))
        }
    };

    let mapped_items = items
        .into_par_iter()
        .map(|item| eval(ctx.with(&ident.inner, &item), expr.clone()))
        .collect::<Result<_, InterpretingError>>()?;

    Ok(new(Val::List(mapped_items), outer_span))
}

fn eval_function_call(ctx: Context, func: TExpr, arg: TExpr) -> Res {
    let outer_span = func.span;
    match eval(ctx.clone(), func)? {
        Trivia {
            inner: Val::Function(func_ctx, ident, body),
            ..
        } => {
            let func_ctx = ctx.merge(&func_ctx);
            let arg = eval(ctx.clone(), arg)?;
            let func_ctx = func_ctx.with(&ident.inner, &arg);
            eval(func_ctx, body)
        }
        Trivia {
            inner:
                Val::NativeFunction(NativeFunction {
                    name,
                    apply,
                    params,
                    ..
                }),
            span,
        } => {
            // comparison short circuit
            if let (
                "or",
                Some(Trivia {
                    inner: Val::Bool(true),
                    ..
                }),
            ) = (name.as_str(), params.first())
            {
                return Ok(new(Val::Bool(true), span));
            }

            if let (
                "and",
                Some(Trivia {
                    inner: Val::Bool(false),
                    ..
                }),
            ) = (name.as_str(), params.first())
            {
                return Ok(new(Val::Bool(false), span));
            }

            let arg = eval(ctx.clone(), arg)?;
            apply(ctx, span, params, arg)
        }
        v => Err(InterpretingError::FuncError(FuncError {
            outer_start: outer_span.start,
            outer_end: outer_span.end,
            func_start: v.span.start,
            func_end: v.span.end,
            func_type: v.inner.to_readable_type(),
        })),
    }
}
