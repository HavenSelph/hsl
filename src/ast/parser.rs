use crate::ast::lexer::{Base, Lexer, LexerIterator};
use crate::ast::span::Span;
use crate::ast::token::{Token, TokenKind};
use crate::ast::{Node, NodeKind};
use crate::report::{Maybe, Report, ReportKind, ReportLevel, ReportSender, SpanToLabel};
use ParserError::*;
use ariadne::Color;
use name_variant::NamedVariant;
use std::fmt::{Display, Formatter};

#[derive(NamedVariant)]
enum ParserError {
    SyntaxError(String),
    UnexpectedEOF,
    UnexpectedToken(TokenKind),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            UnexpectedToken(kind) => write!(f, " {kind}")?,
            SyntaxError(msg) => write!(f, " {msg}")?,
            _ => (),
        }
        Ok(())
    }
}

impl ReportKind for ParserError {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }

    fn incomplete(self) -> bool {
        matches!(self, UnexpectedEOF)
    }
}

pub struct Parser<'contents> {
    lexer: std::iter::Peekable<LexerIterator<'contents>>,
    current: Token<'contents>,
    reporter: ReportSender,
}

impl<'contents> Parser<'contents> {
    pub fn new(filename: &'static str, reporter: ReportSender) -> Maybe<Self> {
        let mut lexer = Lexer::new(filename)?.into_iter().peekable();
        let current = loop {
            match lexer.next() {
                Some(Err(report)) => reporter.report(report.finish().into()),
                Some(Ok(token)) => break token,
                _ => unreachable!(),
            }
        };
        Ok(Self {
            current,
            lexer,
            reporter,
        })
    }

    fn report(&self, report: Box<Report>) {
        self.reporter.report(report);
    }

    fn advance(&mut self) {
        self.current = loop {
            match self.lexer.next().expect("Advanced past EOF") {
                Err(report) => self.report(report.finish().into()),
                Ok(token) => break token,
            }
        }
    }

    fn skip_until<F: Fn(Token) -> bool>(&'_ mut self, predicate: F) -> Option<Token<'_>> {
        loop {
            match self.current {
                token if predicate(token) => break Some(self.current.clone()),
                Token {
                    kind: TokenKind::EOF,
                    ..
                } => break None,
                _ => self.advance(),
            }
        }
    }

    fn sync<F: Fn(Token) -> bool>(&mut self, predicate: F) {
        self.skip_until(|token| /* Is it a new statement? */ matches!(token.kind,
                // This is where we will check for known statement beginners
                TokenKind::Semicolon
            ) || token.newline_before || predicate(token));
        if self.current.kind == TokenKind::Semicolon {
            self.advance();
        }
    }

    // fn peek_is(&mut self, kind: TokenKind) -> bool {
    //     self.lexer
    //         .peek()
    //         .is_some_and(|result| result.as_ref().is_ok_and(|token| token.kind == kind))
    // }

    fn consume<F: FnOnce(Token) -> bool, T: Display>(
        &mut self,
        predicate: F,
        message: T,
    ) -> Maybe<Token<'contents>> {
        match self.current {
            token if predicate(token) => {
                if token.kind != TokenKind::EOF {
                    self.advance();
                }
                Ok(token.clone())
            }
            token if token.kind == TokenKind::EOF => Err(UnexpectedEOF
                .make_labeled(token.span.labeled(message))
                .into()),
            token => Err(UnexpectedToken(token.kind)
                .make_labeled(token.span.labeled(message))
                .into()),
        }
    }

    // fn consume_line(&mut self) -> Maybe<()> {
    //     match self.current {
    //         Token {
    //             kind: TokenKind::Semicolon,
    //             ..
    //         } => self.advance(),
    //         Token {
    //             kind: TokenKind::EOF,
    //             ..
    //         } => (),
    //         token if token.newline_before => (),
    //         token => {
    //             return Err(UnexpectedToken(token.kind)
    //                 .make_labeled(token.span.labeled("Expected end of statement"))
    //                 .into());
    //         }
    //     }
    //     Ok(())
    // }

    fn consume_line_or(&mut self, expect: TokenKind) -> Maybe<()> {
        match self.current {
            Token {
                kind: TokenKind::Semicolon,
                ..
            } => self.advance(),
            Token {
                kind: TokenKind::EOF,
                ..
            } => (),
            token if token.newline_before || token.kind == expect => (),
            token => {
                return Err(UnexpectedToken(token.kind)
                    .make_labeled(
                        token
                            .span
                            .labeled(format!("Expected end of statement or {:?}", expect)),
                    )
                    .into());
            }
        }
        Ok(())
    }

    fn consume_one(&mut self, expect: TokenKind) -> Maybe<Token<'contents>> {
        self.consume(|token| token.kind == expect, format!("Expected {expect}"))
    }

    pub fn parse(&mut self) -> Box<Node> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Box<Node> {
        match self.parse_block(self.current.span, TokenKind::EOF) {
            Ok(val) => val,
            _ => panic!("Failed to parse global block."),
        }
    }

    fn parse_block(&mut self, start: Span, closer: TokenKind) -> Maybe<Box<Node>> {
        let mut stmts = Vec::new();
        let sync = |s: &mut Parser| s.sync(|token| token.kind == closer);

        while self.current.kind != closer && self.current.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(stmt) => match self.consume_line_or(closer) {
                    Ok(_) => stmts.push(*stmt),
                    Err(e) => {
                        self.report(e.finish().into());
                        sync(self);
                    }
                },
                Err(e) => {
                    self.report(e.finish().into());
                    sync(self);
                }
            }
        }
        let end = self.consume_one(closer)?.span;

        Ok(NodeKind::Block(stmts).make(start.extend(end)).into())
    }

    fn parse_statement(&mut self) -> Maybe<Box<Node>> {
        let Token { kind, span, .. } = self.current;
        match kind {
            TokenKind::Return => {
                self.advance();
                let expr = self.parse_expression(0)?;
                Ok(NodeKind::Return(expr).make(span).into())
            }
            TokenKind::Let => {
                self.advance();
                let ident = self.consume_one(TokenKind::Identifier)?.text;
                // let type_annotation: Option<Type> = match self.current.kind {
                //     TokenKind::Colon => {
                //         unimplemented!("Have not implemented type checking yet...")
                //     }
                //     _ => None
                // };
                self.consume_one(TokenKind::Equals)?;
                let expr = self.parse_expression(0)?;
                let span = span.extend(expr.span);
                Ok(NodeKind::VarDeclaration(ident.to_string(), expr)
                    .make(span)
                    .into())
            }
            _ => self.parse_expression(0),
        }
    }

    fn parse_expression(&mut self, min_bp: u8) -> Maybe<Box<Node>> {
        let mut lhs = match self.current.kind.as_prefix() {
            Some((op, _, rbp)) => {
                let span = self.current.span;
                self.advance();
                let rhs = self.parse_expression(rbp)?;
                let span = span.extend(rhs.span);
                NodeKind::UnaryOperation(op, rhs).make(span).into()
            }
            _ => self.parse_atom()?,
        };
        loop {
            if let Some((op, lbp, ())) = self.current.kind.as_postfix() {
                if lbp < min_bp {
                    break;
                }
                let span = self.current.span;
                self.advance();
                lhs = NodeKind::UnaryOperation(op, lhs).make(span).into();
                continue;
            }
            let Some((op, lbp, rbp)) = self.current.kind.as_infix() else {
                break;
            };
            if lbp < min_bp {
                break;
            }
            self.advance();
            let rhs = self.parse_expression(rbp)?;
            let span = lhs.span.extend(rhs.span);
            lhs = NodeKind::BinaryOperation(op, lhs, rhs).make(span).into();
        }
        Ok(lhs)
    }

    fn parse_atom(&mut self) -> Maybe<Box<Node>> {
        let Token {
            kind, text, span, ..
        } = self.current;
        match kind {
            TokenKind::LeftParen => {
                self.advance();
                let mut expr = self.parse_expression(0)?;
                let end = self.consume_one(TokenKind::RightParen)?.span;
                expr.span = span.extend(end);
                Ok(expr)
            }
            TokenKind::Identifier => {
                self.advance();
                Ok(NodeKind::Identifier(text.to_string()).make(span).into())
            }
            TokenKind::StringLiteral => {
                self.advance();
                Ok(
                    NodeKind::StringLiteral(StringParser::new(text, span).parse()?)
                        .make(span)
                        .into(),
                )
            }
            TokenKind::BooleanLiteral => {
                self.advance();
                Ok(NodeKind::BooleanLiteral(text.eq("True")).make(span).into())
            }
            TokenKind::FloatLiteral => {
                self.advance();
                let val = text.parse().map_err(|err| {
                    SyntaxError("Invalid Float Literal".to_string())
                        .make_labeled(span.label())
                        .with_note(err)
                })?;
                Ok(NodeKind::FloatLiteral(val).make(span).into())
            }
            TokenKind::IntegerLiteralBin
            | TokenKind::IntegerLiteralDec
            | TokenKind::IntegerLiteralHex
            | TokenKind::IntegerLiteralOct => {
                let Token { kind, .. } = self.current;
                self.advance();
                let (base, radix) = match kind {
                    TokenKind::IntegerLiteralBin => (Base::Binary, 2),
                    TokenKind::IntegerLiteralOct => (Base::Octal, 8),
                    TokenKind::IntegerLiteralDec => (Base::Decimal, 10),
                    TokenKind::IntegerLiteralHex => (Base::Hexadecimal, 16),
                    _ => unreachable!(),
                };
                let val = usize::from_str_radix(text, radix).map_err(|err| {
                    Box::new(
                        SyntaxError(format!("Invalid {base:?} Integer literal"))
                            .make_labeled(span.label())
                            .with_note(err),
                    )
                })?;
                Ok(NodeKind::IntegerLiteral(val).make(span).into())
            }
            TokenKind::EOF => Err(UnexpectedEOF
                .make_labeled(span.labeled("Expected an expression"))
                .into()),
            _ => {
                self.advance();
                Err(UnexpectedToken(kind).make_labeled(span.label()).into())
            }
        }
    }
}

struct StringParser<'contents> {
    span: Span,
    source: &'contents str,
    char_indices: std::iter::Peekable<std::str::CharIndices<'contents>>,
    current_char: Option<char>,
    current_index: usize,
}

impl<'contents> StringParser<'contents> {
    pub fn new(source: &'contents str, span: Span) -> Self {
        let mut parser = Self {
            span,
            source,
            char_indices: source.char_indices().peekable(),
            current_char: None,
            current_index: 0,
        };
        parser.advance();
        parser
    }
    fn advance(&mut self) {
        let current = self.char_indices.next();
        self.current_char = current.map(|(_, c)| c);
        self.current_index = current.map(|(i, _)| i).unwrap_or(self.current_index + 1);
    }
    fn span(&self, start: usize, end: usize) -> Span {
        Span {
            filename: self.span.filename,
            start: self.span.start + start + 1,
            end: self.span.start + end + 1,
        }
    }
    fn span_from(&self, start: usize) -> Span {
        self.span(start, self.current_index)
    }

    fn span_at(&self, start: usize) -> Span {
        Span::at(self.span.filename, self.span.start + start + 1)
    }

    pub fn parse(&mut self) -> Maybe<String> {
        let mut buf = String::with_capacity(self.source.len());
        while let Some(char) = self.current_char {
            let start = self.current_index;
            match char {
                '\\' => {
                    self.advance();
                    let escaped = self.current_char.expect("Lexer left a hanging escape");
                    self.advance();
                    match escaped {
                        '\\' | '\'' | '"' => buf.push(escaped),
                        'n' => buf.push('\n'),
                        'r' => buf.push('\r'),
                        't' => buf.push('\t'),
                        'b' => buf.push('\u{0008}'),
                        'f' => buf.push('\u{000C}'),
                        '0' => buf.push('\0'),
                        'u' => {
                            let code_start = self.current_index;
                            for _ in 0..4 {
                                match self.current_char {
                                    Some('0'..='9' | 'a'..='f' | 'A'..='F') => self.advance(),
                                    Some(c) => {
                                        return Err(SyntaxError(format!(
                                            "Unexpected character {c:?} for escape code"
                                        ))
                                        .make_labeled(
                                            self.span_at(self.current_index).labeled("here"),
                                        )
                                        .with_label(
                                            self.span_from(code_start)
                                                .label()
                                                .with_color(Color::Blue),
                                        )
                                        .into());
                                    }
                                    None => {
                                        return Err(SyntaxError(
                                            "Unexpected end of string.".to_string(),
                                        )
                                        .make_labeled(
                                            self.span_at(self.current_index).labeled("here"),
                                        )
                                        .with_label(
                                            self.span_from(code_start)
                                                .label()
                                                .with_color(Color::Blue),
                                        )
                                        .into());
                                    }
                                };
                            }
                            let code_span = self.span(code_start, self.current_index);
                            let code_text = &self.source[code_start..self.current_index];
                            let val = u16::from_str_radix(code_text, 16).map_err(|e| {
                                SyntaxError(format!("Invalid Unicode Escape Sequence: {code_text}"))
                                    .make_labeled(code_span.labeled(e))
                                    .with_label(self.span.label().with_color(Color::Blue))
                            })?;
                            let u_char = char::decode_utf16(vec![val])
                                .next()
                                .expect("Got None from unicode decoder")
                                .map_err(|_| {
                                    SyntaxError(format!(
                                        "Invalid Unicode Escape Sequence: {code_text}"
                                    ))
                                    .make_labeled(code_span.label())
                                    .with_label(self.span.label().with_color(Color::Blue))
                                })?;
                            buf.push(u_char);
                        }
                        unexpected => {
                            return Err(SyntaxError(format!(
                                "Invalid Escape Character: {unexpected}"
                            ))
                            .make_labeled(self.span_at(start).label())
                            .with_label(self.span.label().with_color(Color::Blue))
                            .into());
                        }
                    }
                }
                c => {
                    self.advance();
                    buf.push(c);
                }
            }
        }
        Ok(buf)
    }
}
