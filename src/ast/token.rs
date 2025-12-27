use crate::ast::span::Span;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
    And,
    Bang,
    BangEquals,
    BooleanLiteral,
    Colon,
    EOF,
    Equals,
    EqualsEquals,
    FloatLiteral,
    GreaterThan,
    GreaterThanEquals,
    Identifier,
    IntegerLiteralBin,
    IntegerLiteralDec,
    IntegerLiteralHex,
    IntegerLiteralOct,
    LeftParen,
    LessThan,
    LessThanEquals,
    Let,
    Minus,
    Or,
    Plus,
    Return,
    RightParen,
    Semicolon,
    Slash,
    Star,
    StringLiteral,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Copy, Clone)]
pub struct Token<'contents> {
    pub kind: TokenKind,
    pub span: Span,
    pub text: &'contents str,
    pub newline_before: bool,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, span: Span, text: &'a str) -> Self {
        Token {
            kind,
            span,
            text,
            newline_before: false,
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{{} {:?}}}", self.kind, self.span, self.text)
    }
}

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{{} {:?}}}", self.kind, self.span, self.text)
    }
}
