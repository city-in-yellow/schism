#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Trivia<T> {
    pub inner: T,
    pub span: Range,
}

pub fn new<T>(inner: T, span: Range) -> Trivia<T> {
    Trivia { inner, span }
}

pub trait WithTrivia {
    fn pretty_string(&self, indent: usize) -> impl AsRef<str>;
}
