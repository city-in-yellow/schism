use std::ops::Range;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Trivia<T> {
    pub inner: T,
    pub span: Range<usize>,
}

pub fn new<T>(inner: T, span: Range<usize>) -> Trivia<T> {
    Trivia { inner, span }
}

pub trait WithTrivia {
    fn pretty_string(&self, indent: usize) -> impl AsRef<str>;
}
