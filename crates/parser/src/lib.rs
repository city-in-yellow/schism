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
                  FuncCall 2..7
                    FuncCall 2..5
                      + 4..5
                      4 2..3
                    6 6..7
                  8 9..10
                  33 12..14
                  45 16..18
                  b 20..21
                  1d20 23..26
                  FuncCall 28..44
                    keep_highest 28..40
                    1d6 41..44
                  Assign 46..60
                    4 51..52
                    Name(x) 46..47
                    FuncCall 54..59
                      FuncCall 54..57
                        * 56..57
                        x 54..55
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
                FuncCall 0..9
                  FuncCall 0..3
                    + 2..3
                    1 0..1
                  FuncCall 4..9
                    FuncCall 4..7
                      * 6..7
                      2 4..5
                    3 8..9
            "#]],
        );
        e(
            "(1 + 2) * 3",
            expect![[r#"
                FuncCall 1..11
                  FuncCall 1..9
                    * 8..9
                    FuncCall 1..6
                      FuncCall 1..4
                        + 3..4
                        1 1..2
                      2 5..6
                  3 10..11
            "#]],
        );
        e(
            "a / (7D30 + 6)",
            expect![[r#"
                FuncCall 0..13
                  FuncCall 0..3
                    / 2..3
                    a 0..1
                  FuncCall 5..13
                    FuncCall 5..11
                      + 10..11
                      7d30 5..9
                    6 12..13
            "#]],
        );
        e(
            "foo is 12. foo",
            expect![[r#"
                Assign 0..14
                  12 7..9
                  Name(foo) 0..3
                  foo 11..14
            "#]],
        );
        e(
            "foo is D20 - 44. foo",
            expect![[r#"
                Assign 0..20
                  FuncCall 7..15
                    FuncCall 7..12
                      - 11..12
                      1d20 7..10
                    44 13..15
                  Name(foo) 0..3
                  foo 17..20
            "#]],
        );
        e(
            r#"[ 4 + 6, 8, 33, 45, b, d20, keep_highest 1d6, x is 4. x * 2 ,x ]"#,
            expect![[r#"
                List 0..64
                  FuncCall 2..7
                    FuncCall 2..5
                      + 4..5
                      4 2..3
                    6 6..7
                  8 9..10
                  33 12..14
                  45 16..18
                  b 20..21
                  1d20 23..26
                  FuncCall 28..44
                    keep_highest 28..40
                    1d6 41..44
                  Assign 46..60
                    4 51..52
                    Name(x) 46..47
                    FuncCall 54..59
                      FuncCall 54..57
                        * 56..57
                        x 54..55
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
                  Assign 44..159
                    42 51..53
                    Name(red) 44..47
                    Assign 75..159
                      37 83..85
                      Name(blue) 75..79
                      Assign 107..159
                        FuncCall 117..127
                          FuncCall 117..122
                            + 121..122
                            red 117..120
                          blue 123..127
                        Name(purple) 107..113
                        FuncCall 149..159
                          FuncCall 149..157
                            * 156..157
                            purple 149..155
                          2 158..159
                  Name(foo) 17..20
                  foo 194..197
            "#]],
        );
    }

    #[test]
    fn test_ordering() {
        e(
            indoc! {r#"
                fn foo bar baz:
                    let barbaz = bar + baz;
                    let bazbar = bar * baz;
                    bazbar - barbaz
                ;
                
                foo 3 5
            "#},
            expect![[r#"
                Assign 0..103
                  Func 7..92
                    Param(bar) 7..10
                    Func 11..92
                      Param(baz) 11..14
                      Assign 20..92
                        FuncCall 33..42
                          FuncCall 33..38
                            + 37..38
                            bar 33..36
                          baz 39..42
                        Name(barbaz) 24..30
                        Assign 48..92
                          FuncCall 61..70
                            FuncCall 61..66
                              * 65..66
                              bar 61..64
                            baz 67..70
                          Name(bazbar) 52..58
                          FuncCall 76..91
                            FuncCall 76..84
                              - 83..84
                              bazbar 76..82
                            barbaz 85..91
                  Name(foo) 3..6
                  FuncCall 95..102
                    FuncCall 95..100
                      foo 95..98
                      3 99..100
                    5 101..102
            "#]],
        );
    }

    #[test]
    fn test_func() {
        e(
            indoc! {r#"
                foo bar is bar. foo 1
            "#},
            expect![[r#"
                Assign 0..22
                  Func 4..14
                    Param(bar) 4..7
                    bar 11..14
                  Name(foo) 0..3
                  FuncCall 16..21
                    foo 16..19
                    1 20..21
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
                Assign 0..139
                  Func 13..119
                    Param(die) 13..16
                    Assign 24..119
                      Func 29..50
                        Param(die) 29..32
                        FuncCall 44..50
                          die 44..47
                          () 48..50
                      Name(roll) 24..28
                      Assign 56..119
                        FuncCall 65..73
                          roll 65..69
                          die 70..73
                        Name(first) 56..61
                        Assign 79..119
                          FuncCall 89..97
                            roll 89..93
                            die 94..97
                          Name(second) 79..85
                          FuncCall 103..119
                            FuncCall 103..112
                              max 103..106
                              first 107..112
                            second 113..119
                  Name(take_highest) 0..12
                  FuncCall 122..138
                    take_highest 122..134
                    1d20 135..138
            "#]],
        );
    }

    #[test]
    fn test_match() {
        e(
            indoc! {r#"
                if first = 6 and second = 6 then "crit"
                else first = 6 then "great"
                else first >= 4 then "okay"
                else "bad"
            "#},
            expect![[r#"
                Match 0..107
                  Case 0..40
                    FuncCall 3..12
                      FuncCall 3..10
                        = 9..10
                        first 3..8
                      6 11..12
                    "crit" 33..39
                  Case 40..68
                    FuncCall 45..54
                      FuncCall 45..52
                        = 51..52
                        first 45..50
                      6 53..54
                    "great" 60..67
                  Case 68..96
                    FuncCall 73..83
                      FuncCall 73..81
                        >= 79..81
                        first 73..78
                      4 82..83
                    "okay" 89..95
                  Case 96..107
                    true 96..107
                    "bad" 101..106
            "#]],
        );
        e(
            indoc! {r#"if first = 6 and second = 6 then "crit""#},
            expect![[r#"
                Match 0..39
                  Case 0..39
                    FuncCall 3..12
                      FuncCall 3..10
                        = 9..10
                        first 3..8
                      6 11..12
                    "crit" 33..39
            "#]],
        );
    }

    #[test]
    fn test_blades() {
        e(
            include_str!("../../../examples/blades.scm"),
            expect![[r#"
                Assign 178..620
                  Func 187..448
                    Param(die) 187..190
                    Assign 243..448
                      FuncCall 255..266
                        highest 255..262
                        die 263..266
                      Name(first) 247..252
                      Assign 272..448
                        FuncCall 285..294
                          FuncCall 285..292
                            nth 285..288
                            die 289..292
                          2 293..294
                        Name(second) 276..282
                        Match 330..448
                          Case 330..374
                            FuncCall 333..342
                              FuncCall 333..340
                                = 339..340
                                first 333..338
                              6 341..342
                            "crit" 363..369
                          Case 374..406
                            FuncCall 379..388
                              FuncCall 379..386
                                = 385..386
                                first 379..384
                              6 387..388
                            "great" 394..401
                          Case 406..438
                            FuncCall 411..421
                              FuncCall 411..419
                                >= 417..419
                                first 411..416
                              4 420..421
                            "okay" 427..433
                          Case 438..448
                            true 438..448
                            "bad" 443..448
                  Name(blade) 181..186
                  Assign 496..620
                    FuncCall 504..516
                      transpose 504..513
                      () 514..516
                    Name(p) 500..501
                    For 546..620
                      FuncCall 550..555
                        FuncCall 550..554
                          ..= 551..554
                          1 550..551
                        7 554..555
                      Name(idx) 559..562
                      Assign 568..620
                        FuncCall 578..592
                          FuncCall 578..590
                            FuncCall 578..586
                              blade 578..583
                              d 585..586
                            idx 587..590
                          6 591..592
                        Name(res) 572..575
                        FuncCall 599..619
                          FuncCall 599..609
                            FuncCall 599..605
                              plot 599..603
                              p 604..605
                            res 606..609
                          "{idx}d6" 610..619
            "#]],
        );
    }
}
