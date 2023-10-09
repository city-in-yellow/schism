#![allow(clippy::result_unit_err)]

use pest::Parser;
use pest_derive::Parser;

mod pratt;

#[derive(Parser)]
#[grammar = "scm.pest"]
pub struct SCMParser;

#[cfg(test)]
mod test {
    use ast::trivia::WithTrivia;
    use expect_test::{expect, Expect};
    use pest::{
        iterators::{Pair, Pairs},
        Parser,
    };
    use pratt::PrattParser;

    use crate::{pratt::ExprParser, Rule, SCMParser};

    fn print_pair(pair: Pair<'_, Rule>, indent: usize) -> String {
        let buffer = String::from_utf8(vec![b' '; indent]).unwrap();
        let line = format!(
            "{buffer}{} {}..{} [{:?}]",
            pair.as_str(),
            pair.as_span().start(),
            pair.as_span().end(),
            pair.as_rule(),
        );
        let mut lines = print_pairs(pair.into_inner(), indent + 2);
        lines.insert(0, line);

        lines.join("\n")
    }

    fn print_pairs(pairs: Pairs<'_, Rule>, indent: usize) -> Vec<String> {
        pairs.into_iter().map(|x| print_pair(x, indent)).collect()
    }

    fn e(rule: Rule, s: &str, expect: Expect) {
        match SCMParser::parse(rule, s) {
            Ok(o) => {
                let x = print_pairs(o, 0);
                expect.assert_eq(&format!("{}\n", x.join("\n")));
            }
            Err(e) => {
                expect.assert_eq(&format!("{:#?}", e));
            }
        }
    }

    #[test]
    fn test_ident() {
        e(
            Rule::ident,
            "asd15wd5sa_a",
            expect![[r#"
                asd15wd5sa_a 0..12 [ident]
            "#]],
        );
        e(
            Rule::ident,
            "A",
            expect![[r#"
                Error {
                    variant: ParsingError {
                        positives: [
                            ident,
                        ],
                        negatives: [],
                    },
                    location: Pos(
                        0,
                    ),
                    line_col: Pos(
                        (
                            1,
                            1,
                        ),
                    ),
                    path: None,
                    line: "A",
                    continued_line: None,
                }"#]],
        );
    }

    #[test]
    fn test_die() {
        e(
            Rule::die,
            "1d10",
            expect![[r#"
                1d10 0..4 [die]
                  1 0..1 [number]
                  10 2..4 [number]
            "#]],
        );

        e(
            Rule::die,
            "2d20",
            expect![[r#"
                2d20 0..4 [die]
                  2 0..1 [number]
                  20 2..4 [number]
            "#]],
        );

        e(
            Rule::die,
            "d0",
            expect![[r#"
                d0 0..2 [die]
                  0 1..2 [number]
            "#]],
        );

        e(
            Rule::die,
            "d6",
            expect![[r#"
                d6 0..2 [die]
                  6 1..2 [number]
            "#]],
        );

        e(
            Rule::die,
            "2d",
            expect![[r#"
                Error {
                    variant: ParsingError {
                        positives: [
                            number,
                        ],
                        negatives: [],
                    },
                    location: Pos(
                        2,
                    ),
                    line_col: Pos(
                        (
                            1,
                            3,
                        ),
                    ),
                    path: None,
                    line: "2d",
                    continued_line: None,
                }"#]],
        );
    }

    #[test]
    fn test_list() {
        e(
            Rule::list,
            "[         ]",
            expect![[r#"
                [         ] 0..11 [list]
            "#]],
        );
        e(
            Rule::list,
            "[ 4 + 6, 8, 33, 45, b, d20, keep_highest, 1d6, x is 4. x * 2 ,x ]",
            expect![[r#"
                [ 4 + 6, 8, 33, 45, b, d20, keep_highest, 1d6, x is 4. x * 2 ,x ] 0..65 [list]
                  4 + 6 2..7 [group]
                    4 2..3 [number]
                    + 4..5 [infix]
                    6 6..7 [number]
                  8 9..10 [group]
                    8 9..10 [number]
                  33 12..14 [group]
                    33 12..14 [number]
                  45 16..18 [group]
                    45 16..18 [number]
                  b 20..21 [group]
                    b 20..21 [ident]
                  d20 23..26 [group]
                    d20 23..26 [die]
                      20 24..26 [number]
                  keep_highest 28..40 [group]
                    keep_highest 28..40 [ident]
                  1d6 42..45 [group]
                    1d6 42..45 [die]
                      1 42..43 [number]
                      6 44..45 [number]
                  x is 4. x * 2  47..61 [group]
                    x is 4. x * 2  47..61 [assignment]
                      x 47..48 [ident]
                      4 52..53 [group]
                        4 52..53 [number]
                      x * 2  55..61 [group]
                        x 55..56 [ident]
                        * 57..58 [infix]
                        2 59..60 [number]
                  x  62..64 [group]
                    x 62..63 [ident]
            "#]],
        );
    }

    fn pe<S>(source: S, e: Expect)
    where
        S: AsRef<str>,
    {
        let x = SCMParser::parse(Rule::group, source.as_ref()).unwrap();
        let expr = ExprParser.parse(x.into_iter()).unwrap();
        e.assert_eq(&format!("{}\n", expr.pretty_string(0)));
    }

    #[test]
    fn test_expr() {
        pe(
            "1",
            expect![[r#"
                1 0..1
            "#]],
        );
        pe(
            "a",
            expect![[r#"
                a 0..1
            "#]],
        );
        pe(
            "4d20",
            expect![[r#"
                4d20 0..4
            "#]],
        );

        pe(
            "1 + 2 * 3",
            expect![[r#"
                Expr 0..9
                  1 0..1
                  `+` 2..3
                  Expr 4..9
                    2 4..5
                    `*` 6..7
                    3 8..9
            "#]],
        );
        pe(
            "(1 + 2) * 3",
            expect![[r#"
                Expr 1..11
                  Expr 1..6
                    1 1..2
                    `+` 3..4
                    2 5..6
                  `*` 8..9
                  3 10..11
            "#]],
        );
        pe(
            "a / (7D30 + 6)",
            expect![[r#"
                Expr 0..13
                  a 0..1
                  `/` 2..3
                  Expr 5..13
                    7d30 5..9
                    `+` 10..11
                    6 12..13
            "#]],
        );
        pe(
            "foo is 12. foo",
            expect![[r#"
                Assign 0..14
                  LValue(foo) 0..3
                  12 7..9
                  foo 11..14
            "#]],
        );
        pe(
            "foo is D20 - 44. foo",
            expect![[r#"
                Assign 0..20
                  LValue(foo) 0..3
                  Expr 7..15
                    1d20 7..10
                    `-` 11..12
                    44 13..15
                  foo 17..20
            "#]],
        );
        pe(
            r#"
                [ 4 + 6, 8, 33, 45, b, d20, keep_highest, 1d6, x is 4. x * 2 ,x ]
            "#,
            expect![[r#"
                List 17..82
                  Expr 19..24
                    4 19..20
                    `+` 21..22
                    6 23..24
                  8 26..27
                  33 29..31
                  45 33..35
                  b 37..38
                  1d20 40..43
                  keep_highest 45..57
                  1d6 59..62
                  Assign 64..78
                    LValue(x) 64..65
                    4 69..70
                    Expr 72..77
                      x 72..73
                      `*` 74..75
                      2 76..77
                  x 79..80
            "#]],
        );
        pe(
            r#"
                foo is
                    red is 42.
                    blue is 37.
                    purple is red + blue.
                    purple * 2.
                
                foo
            "#,
            expect![[r#"
                Assign 17..210
                  LValue(foo) 17..20
                  Assign 44..159
                    LValue(red) 44..47
                    42 51..53
                    Assign 75..159
                      LValue(blue) 75..79
                      37 83..85
                      Assign 107..159
                        LValue(purple) 107..113
                        Expr 117..127
                          red 117..120
                          `+` 121..122
                          blue 123..127
                        Expr 149..159
                          purple 149..155
                          `*` 156..157
                          2 158..159
                  foo 194..197
            "#]],
        );
    }
}
