#![allow(clippy::result_unit_err)]

use pest::Parser;
use pest_derive::Parser;

mod pratt;

#[derive(Parser)]
#[grammar = "scm.pest"]
pub struct SCMParser;

#[cfg(test)]
mod test {
    use expect_test::{expect, Expect};
    use pest::Parser;
    use pratt::PrattParser;

    use crate::{pratt::ExprParser, Rule, SCMParser};

    fn e(rule: Rule, s: &str, expect: Expect) {
        let x = SCMParser::parse(rule, s);
        expect.assert_debug_eq(&x);
    }

    #[test]
    fn test_ident() {
        e(
            Rule::ident,
            "asd15wd5sa_a",
            expect![[r#"
                Ok(
                    [
                        Pair {
                            rule: ident,
                            span: Span {
                                str: "asd15wd5sa_a",
                                start: 0,
                                end: 12,
                            },
                            inner: [],
                        },
                    ],
                )
            "#]],
        );
        e(
            Rule::ident,
            "A",
            expect![[r#"
                Err(
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
                    },
                )
            "#]],
        );
    }

    #[test]
    fn test_die() {
        e(
            Rule::die,
            "1d10",
            expect![[r#"
            Ok(
                [
                    Pair {
                        rule: die,
                        span: Span {
                            str: "1d10",
                            start: 0,
                            end: 4,
                        },
                        inner: [
                            Pair {
                                rule: number,
                                span: Span {
                                    str: "1",
                                    start: 0,
                                    end: 1,
                                },
                                inner: [],
                            },
                            Pair {
                                rule: number,
                                span: Span {
                                    str: "10",
                                    start: 2,
                                    end: 4,
                                },
                                inner: [],
                            },
                        ],
                    },
                ],
            )
        "#]],
        );

        e(
            Rule::die,
            "2d20",
            expect![[r#"
            Ok(
                [
                    Pair {
                        rule: die,
                        span: Span {
                            str: "2d20",
                            start: 0,
                            end: 4,
                        },
                        inner: [
                            Pair {
                                rule: number,
                                span: Span {
                                    str: "2",
                                    start: 0,
                                    end: 1,
                                },
                                inner: [],
                            },
                            Pair {
                                rule: number,
                                span: Span {
                                    str: "20",
                                    start: 2,
                                    end: 4,
                                },
                                inner: [],
                            },
                        ],
                    },
                ],
            )
        "#]],
        );

        e(
            Rule::die,
            "d0",
            expect![[r#"
            Ok(
                [
                    Pair {
                        rule: die,
                        span: Span {
                            str: "d0",
                            start: 0,
                            end: 2,
                        },
                        inner: [
                            Pair {
                                rule: number,
                                span: Span {
                                    str: "0",
                                    start: 1,
                                    end: 2,
                                },
                                inner: [],
                            },
                        ],
                    },
                ],
            )
        "#]],
        );

        e(
            Rule::die,
            "d6",
            expect![[r#"
            Ok(
                [
                    Pair {
                        rule: die,
                        span: Span {
                            str: "d6",
                            start: 0,
                            end: 2,
                        },
                        inner: [
                            Pair {
                                rule: number,
                                span: Span {
                                    str: "6",
                                    start: 1,
                                    end: 2,
                                },
                                inner: [],
                            },
                        ],
                    },
                ],
            )
        "#]],
        );

        e(
            Rule::die,
            "2d",
            expect![[r#"
            Err(
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
                },
            )
        "#]],
        );
    }

    fn pe<S>(source: S, e: Expect)
    where
        S: AsRef<str>,
    {
        let x = SCMParser::parse(Rule::group, source.as_ref()).unwrap();
        let expr = ExprParser.parse(x.into_iter()).unwrap();
        e.assert_debug_eq(&expr);
    }

    #[test]
    fn test_expr() {
        pe(
            "1",
            expect![[r#"
            Atom(
                Number(
                    1,
                ),
            )
        "#]],
        );
        pe(
            "a",
            expect![[r#"
            Atom(
                Ident(
                    "a",
                ),
            )
        "#]],
        );
        pe(
            "4d20",
            expect![[r#"
            Atom(
                Die(
                    4,
                    20,
                ),
            )
        "#]],
        );

        pe(
            "1 + 2 * 3",
            expect![[r#"
            InfixExpr(
                Atom(
                    Number(
                        1,
                    ),
                ),
                Plus,
                InfixExpr(
                    Atom(
                        Number(
                            2,
                        ),
                    ),
                    Star,
                    Atom(
                        Number(
                            3,
                        ),
                    ),
                ),
            )
        "#]],
        );
        pe(
            "(1 + 2) * 3",
            expect![[r#"
            InfixExpr(
                InfixExpr(
                    Atom(
                        Number(
                            1,
                        ),
                    ),
                    Plus,
                    Atom(
                        Number(
                            2,
                        ),
                    ),
                ),
                Star,
                Atom(
                    Number(
                        3,
                    ),
                ),
            )
        "#]],
        );
        pe(
            "a / (7D30 + 6)",
            expect![[r#"
            InfixExpr(
                Atom(
                    Ident(
                        "a",
                    ),
                ),
                Slash,
                InfixExpr(
                    Atom(
                        Die(
                            7,
                            30,
                        ),
                    ),
                    Plus,
                    Atom(
                        Number(
                            6,
                        ),
                    ),
                ),
            )
        "#]],
        );
        pe(
            "foo is 12. foo",
            expect![[r#"
                Assignment(
                    "foo",
                    Atom(
                        Number(
                            12,
                        ),
                    ),
                    Atom(
                        Ident(
                            "foo",
                        ),
                    ),
                )
            "#]],
        );
        pe(
            "foo is D20 - 44. foo",
            expect![[r#"
                Assignment(
                    "foo",
                    InfixExpr(
                        Atom(
                            Die(
                                1,
                                20,
                            ),
                        ),
                        Minus,
                        Atom(
                            Number(
                                44,
                            ),
                        ),
                    ),
                    Atom(
                        Ident(
                            "foo",
                        ),
                    ),
                )
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
                Assignment(
                    "foo",
                    Assignment(
                        "red",
                        Atom(
                            Number(
                                42,
                            ),
                        ),
                        Assignment(
                            "blue",
                            Atom(
                                Number(
                                    37,
                                ),
                            ),
                            Assignment(
                                "purple",
                                InfixExpr(
                                    Atom(
                                        Ident(
                                            "red",
                                        ),
                                    ),
                                    Plus,
                                    Atom(
                                        Ident(
                                            "blue",
                                        ),
                                    ),
                                ),
                                InfixExpr(
                                    Atom(
                                        Ident(
                                            "purple",
                                        ),
                                    ),
                                    Star,
                                    Atom(
                                        Number(
                                            2,
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                    Atom(
                        Ident(
                            "foo",
                        ),
                    ),
                )
            "#]],
        );
    }
}
