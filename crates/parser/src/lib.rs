#![feature(box_patterns)]

use ast::{ast::Expr, trivia::Trivia};
use errors::{ParsingError, ScmError};
use pest::{error::Error, Parser};
use pest_derive::Parser;

mod pratt;

#[derive(Parser)]
#[grammar = "scm.pest"]
pub struct SCMParser;

#[allow(clippy::type_complexity)]
fn parse_rule(
    source: &str,
    rule: Rule,
) -> Result<Trivia<Box<Expr>>, Box<(errors::ScmError, Error<Rule>)>> {
    match SCMParser::parse(rule, source) {
        Err(err) => {
            let (start, end) = match &err.location {
                pest::error::InputLocation::Pos(offset) => (*offset, *offset),
                pest::error::InputLocation::Span((start, end)) => (*start, *end),
            };

            let e = ScmError::ParsingError(ParsingError { start, end });

            Err(Box::new((e, err)))
        }
        Ok(ok) => Ok(pratt::parse_pratt(ok)),
    }
}

pub fn parse<S>(source: S) -> Result<Trivia<Box<Expr>>, errors::ScmError>
where
    S: AsRef<str>,
{
    parse_rule(source.as_ref(), Rule::entry).map_err(|box (e, _)| e)
}

#[cfg(test)]
mod test {
    use ast::trivia::WithTrivia;
    use expect_test::{expect, Expect};
    use indoc::indoc;

    use crate::{parse_rule, Rule};

    fn e(s: &str, expect: Expect) {
        match parse_rule(s, Rule::entry) {
            Ok(o) => {
                expect.assert_eq(&format!("{}\n", o.pretty_string(0)));
            }
            Err(box (e, err)) => {
                expect.assert_eq(&format!("{}\n\n{:#?}", e.to_report(s), err));
            }
        }
    }

    #[test]
    fn test_ident() {
        e(
            "asd15wd5sa_a",
            expect![[r#"
                asd15wd5sa_a 0..12
            "#]],
        );
        e(
            "A",
            expect![[r#"
                ꡃꡨꡀꡃꡕ: kaathe.encyc.sim.tape_err
                   ╭─[<unknown>:1:1]
                   │
                 1 │ A
                   │ │ 
                   │ ╰─ 「unexpected input」
                ───╯


                Error {
                    variant: ParsingError {
                        positives: [
                            group,
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
            "1d10",
            expect![[r#"
                1d10 0..4
            "#]],
        );

        e(
            "2d20",
            expect![[r#"
                2d20 0..4
            "#]],
        );

        e(
            "d0",
            expect![[r#"
                1d0 0..2
            "#]],
        );

        e(
            "d6",
            expect![[r#"
                1d6 0..2
            "#]],
        );

        e(
            "2d",
            expect![[r#"
                ꡃꡨꡀꡃꡕ: kaathe.encyc.sim.tape_err
                   ╭─[<unknown>:1:3]
                   │
                 1 │ 2d
                   │   │ 
                   │   ╰─ 「unexpected input」
                ───╯


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
            "[         ]",
            expect![[r#"
                List 0..11
            "#]],
        );
        e(
            "[ 4 + 6, 8, 33, 45, b, d20, keep_highest 1d6, x is 4. x * 2 ,x ]",
            expect![[r#"
                List 0..64
                  Expr 2..7
                    4 2..3
                    `+` 4..5
                    6 6..7
                  8 9..10
                  33 12..14
                  45 16..18
                  b 20..21
                  1d20 23..26
                  FuncCall 28..44
                    Name(keep_highest) 28..40
                    1d6 41..44
                  Assign 46..60
                    Name(x) 46..47
                    4 51..52
                    Expr 54..59
                      x 54..55
                      `*` 56..57
                      2 58..59
                  x 61..62
            "#]],
        );
    }

    #[test]
    fn test_expr() {
        e(
            "1",
            expect![[r#"
                1 0..1
            "#]],
        );
        e(
            "a",
            expect![[r#"
                a 0..1
            "#]],
        );
        e(
            "4d20",
            expect![[r#"
                4d20 0..4
            "#]],
        );

        e(
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
        e(
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
        e(
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
        e(
            "foo is 12. foo",
            expect![[r#"
                Assign 0..14
                  Name(foo) 0..3
                  12 7..9
                  foo 11..14
            "#]],
        );
        e(
            "foo is D20 - 44. foo",
            expect![[r#"
                Assign 0..20
                  Name(foo) 0..3
                  Expr 7..15
                    1d20 7..10
                    `-` 11..12
                    44 13..15
                  foo 17..20
            "#]],
        );
        e(
            r#"[ 4 + 6, 8, 33, 45, b, d20, keep_highest 1d6, x is 4. x * 2 ,x ]"#,
            expect![[r#"
                List 0..64
                  Expr 2..7
                    4 2..3
                    `+` 4..5
                    6 6..7
                  8 9..10
                  33 12..14
                  45 16..18
                  b 20..21
                  1d20 23..26
                  FuncCall 28..44
                    Name(keep_highest) 28..40
                    1d6 41..44
                  Assign 46..60
                    Name(x) 46..47
                    4 51..52
                    Expr 54..59
                      x 54..55
                      `*` 56..57
                      2 58..59
                  x 61..62
            "#]],
        );
        e(
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
                  Name(foo) 17..20
                  Assign 44..159
                    Name(red) 44..47
                    42 51..53
                    Assign 75..159
                      Name(blue) 75..79
                      37 83..85
                      Assign 107..159
                        Name(purple) 107..113
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

    #[test]
    fn test_func() {
        e(
            indoc! {r#"
                foo bar is
                    bar * 2.
                
                foo
            "#},
            expect![[r#"
                FuncAssign 0..29
                  Name(foo) 0..3
                  ParamList 4..8
                    Param(bar) 4..7
                  Expr 15..22
                    bar 15..18
                    `*` 19..20
                    2 21..22
                  foo 25..28
            "#]],
        );
        e(
            indoc! {r#"
                take_highest die is
                    roll die is
                        die ().
                    first is roll die.
                    second is roll die.
                    max first second.
                
                take_highest d20
            "#},
            expect![[r#"
                FuncAssign 0..139
                  Name(take_highest) 0..12
                  ParamList 13..17
                    Param(die) 13..16
                  FuncAssign 24..119
                    Name(roll) 24..28
                    ParamList 29..33
                      Param(die) 29..32
                    FuncCall 44..50
                      Name(die) 44..47
                      () 48..50
                    Assign 56..119
                      Name(first) 56..61
                      FuncCall 65..73
                        Name(roll) 65..69
                        die 70..73
                      Assign 79..119
                        Name(second) 79..85
                        FuncCall 89..97
                          Name(roll) 89..93
                          die 94..97
                        FuncCall 103..119
                          Name(max) 103..106
                          FuncCall 107..119
                            Name(first) 107..112
                            second 113..119
                  FuncCall 122..139
                    Name(take_highest) 122..134
                    1d20 135..138
            "#]],
        );
    }
}
