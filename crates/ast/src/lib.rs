use smol_str::SmolStr;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Atom {
    Ident(SmolStr),
    Number(isize),
    Die(isize, isize),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Equal,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Postfix {
    Question,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
    InfixExpr(Box<Expr>, Infix, Box<Expr>),
    PrefixExpr(Prefix, Box<Expr>),
    PostfixExpr(Box<Expr>, Postfix),
    Assignment(SmolStr, Box<Expr>, Box<Expr>),
    Atom(Atom),
}
