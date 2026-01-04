use crate::ast::span::Span;
use crate::ast::token::{Token, TokenKind};
use crate::report::{Maybe, ReportKind, ReportLevel, SpanToLabel};
use LexerReport::*;
use ariadne::Color;
use name_variant::NamedVariant;
use std::fmt::{Display, Formatter};
use std::iter::FusedIterator;

#[derive(NamedVariant)]
enum LexerReport {
    UnexpectedCharacter(char),
    UnterminatedString,
    SyntaxError,
}

impl Display for LexerReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnexpectedCharacter(c) => {
                write!(f, "{} {:?}", self.variant_name(), c)
            }
            _ => write!(f, "{}", self.variant_name()),
        }
    }
}

impl ReportKind for LexerReport {
    fn title(&self) -> String {
        self.to_string()
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

pub struct Lexer<'contents> {
    filename: &'static str,
    source: &'contents str,
    char_indices: std::iter::Peekable<std::str::CharIndices<'contents>>,
    current_char: Option<char>,
    current_index: usize,
    seen_newline: bool,
}

impl<'contents> Lexer<'contents> {
    pub fn new(filename: &'static str) -> Maybe<Self> {
        let source = crate::files::get_source(filename)?.text();
        let mut lexer = Self {
            filename,
            source,
            char_indices: source.char_indices().peekable(),
            current_char: None,
            current_index: 0,
            seen_newline: true,
        };
        lexer.advance();
        Ok(lexer)
    }

    fn advance(&mut self) {
        let current = self.char_indices.next();
        self.current_char = current.map(|(_, c)| c);
        self.current_index = current.map(|(i, _)| i).unwrap_or(self.current_index + 1);
    }
    fn peek(&mut self) -> Option<&(usize, char)> {
        self.char_indices.peek()
    }
    fn peek_char(&mut self) -> Option<&char> {
        self.peek().map(|(_, c)| c)
    }
    fn make(
        &mut self,
        start: usize,
        end: usize,
        token_kind: TokenKind,
        text: &'contents str,
    ) -> Maybe<Token<'contents>> {
        let mut token = Token::new(token_kind, self.span(start, end), text);
        token.newline_before = self.seen_newline;
        self.seen_newline = false;
        Ok(token)
    }
    fn make_simple(&mut self, start: usize, token_kind: TokenKind) -> Maybe<Token<'contents>> {
        self.make(
            start,
            self.current_index,
            token_kind,
            self.slice(start, self.current_index),
        )
    }
    fn make_advance(
        &mut self,
        start: usize,
        i: usize,
        token_kind: TokenKind,
    ) -> Maybe<Token<'contents>> {
        for _ in 0..i {
            self.advance();
        }
        self.make_simple(start, token_kind)
    }

    fn span(&self, start: usize, end: usize) -> Span {
        Span {
            filename: self.filename,
            start,
            end,
        }
    }
    fn span_from(&self, start: usize) -> Span {
        self.span(start, self.current_index)
    }

    fn span_at(&self, start: usize) -> Span {
        Span::at(self.filename, start)
    }
    fn slice(&self, start: usize, end: usize) -> &'contents str {
        &self.source[start..end]
    }

    fn unexpected(&mut self, char: char, start: usize) -> Maybe<Token<'contents>> {
        self.advance();
        Err(UnexpectedCharacter(char)
            .make_labeled(self.span_at(start).label())
            .into())
    }

    #[allow(clippy::cognitive_complexity)]
    pub fn lex_token(&mut self) -> Maybe<Token<'contents>> {
        loop {
            let Some(char) = self.current_char else {
                return Ok(Token::new(
                    TokenKind::EOF,
                    self.span_from(self.current_index),
                    "",
                ));
            };
            let start = self.current_index;
            return match char {
                '\n' => {
                    while let Some('\n') = self.current_char {
                        self.advance();
                    }
                    self.seen_newline = true;
                    continue;
                }
                c if c.is_whitespace() => {
                    self.advance();
                    continue;
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = self.current_char {
                        self.advance();
                    }
                    let kind = match self.slice(start, self.current_index) {
                        "True" | "False" => TokenKind::BooleanLiteral,
                        _ => TokenKind::Identifier,
                    };
                    self.make_simple(start, kind)
                }
                '0' if self.peek_char().is_some_and(|c| "box".contains(*c)) => {
                    self.advance();
                    let base = match self.current_char.unwrap() {
                        'b' => Base::Binary,
                        'o' => Base::Octal,
                        'x' => Base::Hexadecimal,
                        _ => unreachable!(),
                    };
                    self.advance();
                    self.lex_integer(start, base)?;
                    self.make_simple(start + 2, base.into())
                }
                '0'..='9' => {
                    self.lex_integer(start, Base::Decimal)?;
                    if let Some('.') = self.current_char {
                        if !self.peek_char().is_some_and(|c| c == &'.') {
                            self.advance();
                            self.lex_integer(start, Base::Decimal)?;
                        }
                    }
                    let kind = if self.slice(start, self.current_index).contains('.') {
                        TokenKind::FloatLiteral
                    } else {
                        TokenKind::IntegerLiteralDec
                    };
                    self.make_simple(start, kind)
                }
                '"' | '\'' => {
                    self.lex_quoted_literal(start, char)?;
                    self.make(
                        start,
                        self.current_index,
                        TokenKind::StringLiteral,
                        self.slice(start + 1, self.current_index - 1),
                    )
                }
                '/' => match self.peek_char() {
                    Some('/') => {
                        while self.current_char.is_some_and(|c| c != '\n') {
                            self.advance();
                        }
                        continue;
                    }
                    Some('*') => {
                        let mut depth = 0;
                        while let Some(c) = self.current_char {
                            match c {
                                '/' if self.peek_char().is_some_and(|c| *c == '*') => {
                                    depth += 1;
                                    self.advance();
                                    self.advance();
                                }
                                '*' if self.peek_char().is_some_and(|c| *c == '/') => {
                                    depth -= 1;
                                    self.advance();
                                    self.advance();
                                }
                                _ => self.advance(),
                            }
                            if depth == 0 {
                                break;
                            }
                        }
                        if depth > 0 {
                            return Err(if depth > 1 {
                                SyntaxError.make_labeled(
                                    self.span(start, start + 2)
                                        .labeled(format!("Opened {} more time(s)", depth - 1)),
                                )
                            } else {
                                SyntaxError.make_labeled(self.span(start, start + 2).label())
                            }
                            .into());
                        }
                        continue;
                    }
                    _ => self.make_advance(start, 1, TokenKind::Slash),
                },
                '+' => self.make_advance(start, 1, TokenKind::Plus),
                '-' => self.make_advance(start, 1, TokenKind::Minus),
                '*' => self.make_advance(start, 1, TokenKind::Star),
                '(' => self.make_advance(start, 1, TokenKind::LeftParen),
                ')' => self.make_advance(start, 1, TokenKind::RightParen),
                ';' => self.make_advance(start, 1, TokenKind::Semicolon),
                // '=' => match self.peek_char() {
                //     Some('=') => self.make_advance(start, 2, TokenKind::EqualsEquals),
                //     _ => self.make_advance(start, 1, TokenKind::Equals),
                // },
                // '>' => match self.peek_char() {
                //     Some('=') => self.make_advance(start, 2, TokenKind::GreaterThanEquals),
                //     _ => self.make_advance(start, 1, TokenKind::GreaterThan),
                // },
                // '<' => match self.peek_char() {
                //     Some('=') => self.make_advance(start, 2, TokenKind::LessThanEquals),
                //     _ => self.make_advance(start, 1, TokenKind::LessThan),
                // },
                // '!' => match self.peek_char() {
                //     Some('=') => self.make_advance(start, 2, TokenKind::BangEquals),
                //     _ => self.make_advance(start, 1, TokenKind::Bang),
                // },
                _ => self.unexpected(char, start),
            };
        }
    }

    fn lex_quoted_literal(&mut self, start: usize, closer: char) -> Maybe<()> {
        self.advance();
        while let Some(char) = self.current_char {
            match char {
                c if c == closer => break,
                '\\' if self.peek_char().is_some_and(|c| *c == closer) => {
                    self.advance();
                    self.advance();
                }
                // '\n' => break, // Should we do this?
                _ => self.advance(),
            }
        }
        if !self.current_char.is_some_and(|c| c == closer) {
            return Err(UnterminatedString
                .make_labeled(self.span_from(start).label())
                .into());
        }
        self.advance();
        Ok(())
    }

    fn lex_integer(&mut self, start: usize, base: Base) -> Maybe<()> {
        // todo: roman literal 0rIVVIM
        while let Some(char) = self.current_char {
            match (base, char.to_ascii_lowercase()) {
                (Base::Binary, '0'..='1')
                | (Base::Octal, '0'..='7')
                | (Base::Decimal, '0'..='9')
                | (Base::Hexadecimal, '0'..='9' | 'a'..='f') => {
                    self.advance();
                }
                (_, '0'..='9' | 'a'..='z') => {
                    return Err(SyntaxError
                        .make_labeled(
                            self.span_at(self.current_index)
                                .labeled(format!("{char:?} invalid for base {base:?}")),
                        )
                        .with_label(
                            self.span(start, self.current_index)
                                .label()
                                .with_color(Color::BrightBlue),
                        )
                        .into());
                }
                (_, '_') => self.advance(),
                _ => break,
            }
        }
        Ok(())
    }
}

pub struct LexerIterator<'contents> {
    exhausted: bool,
    lexer: Lexer<'contents>,
}

impl<'contents> Iterator for LexerIterator<'contents> {
    type Item = Maybe<Token<'contents>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }
        let token = self.lexer.lex_token();
        Some(match token {
            Ok(t) => {
                if t.kind == TokenKind::EOF {
                    self.exhausted = true;
                }
                Ok(t)
            }
            Err(e) => Err(e),
        })
    }
}

impl<'contents> FusedIterator for LexerIterator<'contents> {}

impl<'contents> IntoIterator for Lexer<'contents> {
    type Item = Maybe<Token<'contents>>;
    type IntoIter = LexerIterator<'contents>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIterator {
            exhausted: false,
            lexer: self,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Base {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl From<Base> for TokenKind {
    fn from(value: Base) -> Self {
        match value {
            Base::Binary => Self::IntegerLiteralBin,
            Base::Octal => Self::IntegerLiteralOct,
            Base::Decimal => Self::IntegerLiteralDec,
            Base::Hexadecimal => Self::IntegerLiteralHex,
        }
    }
}
