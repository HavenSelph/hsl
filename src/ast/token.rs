use crate::ast::span::Span;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
    Ampersand,
    And,
    Assert,
    Bang,
    BangEquals,
    BooleanLiteral,
    Break,
    Caret,
    Colon,
    Comma,
    Continue,
    EOF,
    Echo,
    Else,
    Equals,
    EqualsEquals,
    FloatLiteral,
    Global,
    Greater,
    GreaterEquals,
    GreaterGreater,
    Identifier,
    If,
    IntegerLiteralBin,
    IntegerLiteralDec,
    IntegerLiteralHex,
    IntegerLiteralOct,
    IntegerLiteralRom,
    LeftBrace,
    LeftParen,
    Less,
    LessEquals,
    LessLess,
    Let,
    Loop,
    Minus,
    Or,
    Percent,
    Pipe,
    Plus,
    QuestionMark,
    RightBrace,
    RightParen,
    Semicolon,
    Slash,
    Star,
    StarStar,
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
