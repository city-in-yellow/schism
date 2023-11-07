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

use fixed::{types::extra::U24, FixedI32};

use smol_str::SmolStr;

fn itof(i: isize) -> FixedI32<U24> {
    FixedI32::from_num(i)
}

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
                    f(
                        c.clone(),
                        v.clone().span,
                        List::new_sync().push_front(v.clone()),
                        r.clone(),
                    )
                    .map(|x| new(x.inner, v.clone().span))
                })
                .collect::<Result<Vec<TVal>, _>>()?,
        )),

        (_, List(rhs)) => Ok(List(
            rhs.par_iter()
                .map(|v| {
                    f(
                        c.clone(),
                        v.clone().span,
                        List::new_sync().push_front(l.clone()),
                        v.clone(),
                    )
                    .map(|x| new(x.inner, v.clone().span))
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
            Decimal(rhs) => Ok(Decimal(itof(*lhs) + rhs)),
            List(_) => map_list(add, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "List"],
            ),
        },

        Decimal(lhs) => match &r.inner {
            Number(rhs) => Ok(Decimal(lhs + itof(*rhs))),
            Decimal(rhs) => Ok(Decimal(lhs + rhs)),
            List(_) => map_list(add, ctx, l, r),

            _ => type_error(
                r.span,
                span,
                r.inner.to_readable_type(),
                ["Number", "Decimal", "List"],
            ),
        },

        String(lhs) => match &r.inner {
            String(rhs) => Ok(String([lhs.clone(), rhs.clone()].concat().into())),

            _ => type_error(r.span, span, r.inner.to_readable_type(), ["String"]),
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

pub fn background_ctx() -> Context {
    let mut ctx = Context::new();

    for func in [add] {
        let (name, f) = func();
        ctx = ctx.with(name, f)
    }

    ctx
}
