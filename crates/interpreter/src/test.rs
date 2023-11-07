use ast::trivia::WithTrivia;
use indoc::indoc;
use insta::assert_snapshot;

use crate::run_on_src;

fn e(s: &str) -> String {
    match run_on_src(s) {
        Ok(o) => {
            format!("{}\n", o.pretty_string(0))
        }
        Err(e) => e.to_report(s),
    }
}

#[test]
fn test() {
    assert_snapshot!(e("1"));
    assert_snapshot!(e("()"));
    assert_snapshot!(e(r#""hi there""#));
    assert_snapshot!(e(r#"foo is 1. foo"#));
    assert_snapshot!(e(r#"[1, 2, 3]"#));
    assert_snapshot!(e(r#"foo bar is bar. foo 1"#));
    assert_snapshot!(e(r#"1 + 2"#));
    assert_snapshot!(e(indoc! {r#"
        foo bar baz is
            barbaz is bar + baz.
            bazbar is bar * baz.
            bazbar - barbaz.
        
        foo 3 5
    "#}));
    assert_snapshot!(e(r#"[1, 2, 3] + 12"#));
    assert_snapshot!(e(r#"1 + "2""#));
    assert_snapshot!(e(indoc! {r#"
        foo bar baz bil is
            barbaz is bar + baz.
            barbaz + bil.
        
        foo 3 5 "a"
    "#}));
}
