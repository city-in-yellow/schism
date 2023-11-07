use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse2, ItemFn};

pub fn native<S>(name: String, s: S) -> TokenStream
where
    TokenStream: From<S>,
{
    let tokens = TokenStream::from(s);
    let func: ItemFn = parse2(tokens).unwrap();

    let sig = func.sig;
    let ident = sig.ident;

    let name = if name.is_empty() {
        format!("{}", ident)
    } else {
        format!("{}", name)
    };

    let inputs = sig.inputs.into_iter().skip(2).collect::<Vec<_>>();
    let n = inputs.len();

    let block = func.block;

    let rev_var = inputs.iter().rev();

    quote! {
        fn #ident() -> (SmolStr, TVal) {
            fn #ident(ctx: Context, span: Range, params: List<TVal, ArcK>, next: TVal) -> RFunc {
                let span = {
                    let start = span.start.min(params.last().map(|x| x.span.start).unwrap_or(0));
                    let end = span.end.max(params.first().map(|x| x.span.end).unwrap_or(0));
                    Range { start, end }
                };

                let next_span = next.span;
                let params = params.push_front(next);

                if params.len() == #n {
                    #(
                        let #rev_var = params.first().unwrap();
                        let params = params.drop_first().unwrap();
                    )*

                    let res = #block;

                    let span = Range {
                        start: span.start.min(next_span.start),
                        end: span.end.max(next_span.end),
                    };

                    res.map(|x| new(x, span))
                } else {
                    let nfunc = NativeFunction {
                        name: #name.into(),
                        params,
                        apply: #ident,
                    };

                    let span = Range {
                        start: span.start.min(next_span.start),
                        end: span.end.max(next_span.end),
                    };

                    Ok(new(Val::NativeFunction(nfunc), span))
                }
            }

            let nfunc = NativeFunction {
                name: #name.into(),
                params: List::new_sync(),
                apply: #ident
            };

            (#name.into(), new(Val::NativeFunction(nfunc), Range { start: 0, end: 0 }))
        }
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use indoc::indoc;
    use insta::assert_snapshot;
    use proc_macro2::TokenStream;
    use syn::{parse_str, File};

    use super::native;

    fn e(t: &str) -> String {
        let tt = native("+".into(), TokenStream::from_str(t).unwrap()).to_string();
        let file: Result<File, _> = parse_str(&tt);
        match file {
            Ok(file) => prettyplease::unparse(&file),
            Err(err) => {
                format!("err: {}", &err)
            }
        }
    }

    #[test]
    fn test() {
        assert_snapshot!(e(indoc! {r#"
            fn add(ctx: Context, span: Range, lhs: &TVal, rhs: &TVal) -> RFunc {
                lhs.clone();
                rhs.clone();

                Ok(new(Val::Unit, span))
            }
        "#}))
    }
}
