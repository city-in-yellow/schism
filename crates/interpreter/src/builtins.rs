use archery::ArcK;
use ast::{
    hir::{
        Context, NativeFunction, RFunc, TVal,
        Val::{self, *},
    },
    trivia::{new, Range},
};
use errors::{InterpretingError, TypeError};
use native_derive::native;
use rayon::prelude::*;
use rpds::List;

use rug::{ops::CompleteRound, Float};
use smol_str::SmolStr;

const PRECISION: u32 = 32;

fn type_error<S: Into<SmolStr>, V: AsRef<str>, I: IntoIterator<Item = V>>(
    a_span: Range,
    f_span: Range,
    c_type: S,
    v_types: I,
) -> Result<Val, InterpretingError> {
    Err(InterpretingError::TypeError(TypeError {
        arg_start: a_span.start,
        arg_end: a_span.end,
        func_start: f_span.start,
        func_end: f_span.end,
        current_type: c_type.into(),
        valid_types: v_types.into_iter().map(|x| x.as_ref().into()).collect(),
    }))
}

fn map_list(
    f: fn(ctx: Context, span: Range, params: List<TVal, ArcK>, next: TVal) -> RFunc,
    c: Context,
    l: &TVal,
    r: &TVal,
) -> Result<Val, InterpretingError> {
    match (&l.inner, &r.inner) {
        (List(lhs), _) => Ok(List(
            lhs.par_iter()
                .map(|v| {
                    let v = v.clone();
                    let v_span = v.span;
                    f(c.clone(), v_span, List::new_sync().push_front(v), r.clone())
                        .map(|x| new(x.inner, v_span))
                })
                .collect::<Result<Vec<TVal>, _>>()?,
        )),

        (_, List(rhs)) => Ok(List(
            rhs.par_iter()
                .map(|v| {
                    let v = v.clone();
                    let v_span = v.span;
                    f(c.clone(), v_span, List::new_sync().push_front(l.clone()), v)
                        .map(|x| new(x.inner, v_span))
                })
                .collect::<Result<Vec<TVal>, _>>()?,
        )),

        _ => unreachable!("non-list arguments passed into map_list"),
    }
}

#[native(+)]
fn add(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Number(lhs + rhs)),
            Decimal(rhs) => Ok(Decimal((*lhs + rhs).complete(PRECISION))),
            String(rhs) => Ok(String(format!("{}{}", lhs, rhs).into())),
            List(_) => map_list(add, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "String", "List"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Decimal((lhs + *rhs).complete(PRECISION))),
            Decimal(rhs) => Ok(Decimal((lhs + rhs).complete(PRECISION))),
            String(rhs) => Ok(String(format!("{}{}", lhs, rhs).into())),
            List(_) => map_list(add, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "String", "List"],
            ),
        },

        String(lhs) => match &r.inner {
            Number(rhs) => Ok(String(format!("{}{}", lhs, rhs).into())),
            Decimal(rhs) => Ok(String(format!("{}{}", lhs, rhs).into())),
            String(rhs) => Ok(String([lhs.clone(), rhs.clone()].concat().into())),
            List(_) => map_list(add, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "String", "List"],
            ),
        },

        List(lhs) => match &r.inner {
            Number(_) | Decimal(_) | String(_) => map_list(add, ctx, l, r),
            List(rhs) => Ok(List(lhs.iter().chain(rhs.iter()).cloned().collect())),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "String", "List"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal", "String", "List"],
        ),
    }
}

#[native(-)]
fn sub(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Number(lhs - rhs)),
            Decimal(rhs) => Ok(Decimal((*lhs - rhs).complete(PRECISION))),
            List(_) => map_list(sub, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "List"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Decimal((lhs - *rhs).complete(PRECISION))),
            Decimal(rhs) => Ok(Decimal((lhs - rhs).complete(PRECISION))),
            List(_) => map_list(sub, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "List"],
            ),
        },

        List(_) => match &r.inner {
            Number(_) | Decimal(_) => map_list(sub, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal", "List"],
        ),
    }
}

#[native(*)]
fn mul(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Number(lhs * rhs)),
            Decimal(rhs) => Ok(Decimal((*lhs * rhs).complete(PRECISION))),
            String(rhs) => Ok(String(if *lhs < 0 {
                rhs.chars()
                    .rev()
                    .collect::<std::string::String>()
                    .repeat((*lhs).abs().try_into().unwrap())
                    .into()
            } else {
                rhs.repeat((*lhs).try_into().unwrap()).into()
            })),
            List(_) => map_list(mul, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "String", "List"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Decimal((lhs * *rhs).complete(PRECISION))),
            Decimal(rhs) => Ok(Decimal((lhs * rhs).complete(PRECISION))),
            List(_) => map_list(mul, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "List"],
            ),
        },

        String(lhs) => match &r.inner {
            Number(rhs) => Ok(String(if *rhs < 0 {
                lhs.chars()
                    .rev()
                    .collect::<std::string::String>()
                    .repeat((*rhs).abs().try_into().unwrap())
                    .into()
            } else {
                lhs.repeat((*rhs).try_into().unwrap()).into()
            })),
            List(_) => map_list(mul, ctx, l, r),

            _ => type_error(r.span, span, r.inner.to_readable_type(), ["Number", "List"]),
        },

        List(_) => match &r.inner {
            Number(_) | Decimal(_) | String(_) => map_list(mul, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "String"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal", "String", "List"],
        ),
    }
}

#[native(/)]
fn div(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Decimal(
                Float::with_val(PRECISION, *lhs) / Float::with_val(PRECISION, *rhs),
            )),
            Decimal(rhs) => Ok(Decimal((*lhs / rhs).complete(PRECISION))),
            List(_) => map_list(div, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "List"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Decimal((lhs / *rhs).complete(PRECISION))),
            Decimal(rhs) => Ok(Decimal((lhs / rhs).complete(PRECISION))),
            List(_) => map_list(div, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "List"],
            ),
        },

        List(_) => match &r.inner {
            Number(_) | Decimal(_) => map_list(div, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal", "List"],
        ),
    }
}

#[native(=)]
fn eq(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Unit => match &r.inner {
            Unit => Ok(Bool(true)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                [l.inner.to_readable_type()],
            ),
        },

        Bool(lhs) => match &r.inner {
            Bool(rhs) => Ok(Bool(lhs == rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                [l.inner.to_readable_type()],
            ),
        },

        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(*lhs == *rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                [l.inner.to_readable_type()],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Decimal(rhs) => Ok(Bool(lhs == rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                [l.inner.to_readable_type()],
            ),
        },

        String(lhs) => match &r.inner {
            String(rhs) => Ok(Bool(lhs == rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                [l.inner.to_readable_type()],
            ),
        },

        List(lhs) => match &r.inner {
            List(rhs) => Ok(Bool(lhs.iter().zip(rhs.iter()).all(|(l, r)| l == r))),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                [l.inner.to_readable_type()],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Unit", "Bool", "Number", "Decimal", "String", "List"],
        ),
    }
}

#[native(>)]
fn gt(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs > rhs)),
            Decimal(rhs) => Ok(Bool(lhs > rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs > rhs)),
            Decimal(rhs) => Ok(Bool(lhs > rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal"],
        ),
    }
}

#[native(>=)]
fn gte(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs >= rhs)),
            Decimal(rhs) => Ok(Bool(lhs >= rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs >= rhs)),
            Decimal(rhs) => Ok(Bool(lhs >= rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal"],
        ),
    }
}

#[native(<)]
fn lt(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs < rhs)),
            Decimal(rhs) => Ok(Bool(lhs < rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs < rhs)),
            Decimal(rhs) => Ok(Bool(lhs < rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal"],
        ),
    }
}

#[native(<=)]
fn lte(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs <= rhs)),
            Decimal(rhs) => Ok(Bool(lhs <= rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Bool(lhs <= rhs)),
            Decimal(rhs) => Ok(Bool(lhs <= rhs)),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal"],
            ),
        },

        _ => type_error(
            l.span,
            span,
            l.inner.to_readable_type(),
            ["Number", "Decimal"],
        ),
    }
}

#[native]
fn and(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Bool(lhs) => match &r.inner {
            Bool(rhs) => Ok(Bool(*lhs && *rhs)),

            _ => type_error(r.span, span, r.inner.to_readable_type(), ["Bool"]),
        },

        _ => type_error(l.span, span, l.inner.to_readable_type(), ["Bool"]),
    }
}

#[native]
fn or(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Bool(lhs) => match &r.inner {
            Bool(rhs) => Ok(Bool(*lhs || *rhs)),

            _ => type_error(r.span, span, r.inner.to_readable_type(), ["Bool"]),
        },

        _ => type_error(l.span, span, l.inner.to_readable_type(), ["Bool"]),
    }
}

#[native(...)]
fn ex_range(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(List(
                ((*lhs)..(*rhs)).map(|idx| new(Number(idx), span)).collect(),
            )),

            _ => type_error(r.span, span, r.inner.to_readable_type(), ["Number"]),
        },

        _ => type_error(l.span, span, l.inner.to_readable_type(), ["Number"]),
    }
}

#[native(..=)]
fn inc_range(ctx: Context, span: Range, l: &TVal, r: &TVal) -> RFunc {
    match &l.inner {
        Number(lhs) => match &r.inner {
            Number(rhs) => Ok(List(
                ((*lhs)..=(*rhs))
                    .map(|idx| new(Number(idx), span))
                    .collect(),
            )),

            _ => type_error(r.span, span, r.inner.to_readable_type(), ["Number"]),
        },

        _ => type_error(l.span, span, l.inner.to_readable_type(), ["Number"]),
    }
}

pub fn background_ctx() -> Context {
    let mut ctx = Context::new();

    for func in [
        add, sub, mul, div, // arithmetic
        eq, gt, gte, lt, lte, and, or, // comparisons
        ex_range, inc_range, // ranges
    ] {
        let (name, f) = func();
        ctx = ctx.with(name, f)
    }

    ctx
}
