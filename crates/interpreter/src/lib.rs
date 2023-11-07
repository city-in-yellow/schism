#![feature(return_position_impl_trait_in_trait)]
#![feature(box_patterns)]

use ast::hir::TVal;
use errors::ScmError;
use eval::eval;
use parser::parse;

mod builtins;
mod eval;

#[cfg(test)]
mod test;

pub fn run_on_src<S>(src: S) -> Result<TVal, ScmError>
where
    S: AsRef<str>,
{
    let expr = parse(src)?;

    let ctx = builtins::background_ctx();
    let exec = eval(ctx, expr).map_err(ScmError::InterpretingError)?;

    Ok(exec)
}
