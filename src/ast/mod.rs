use crate::ast::span::Span;
use name_variant::NamedVariant;
use std::fmt::{Debug, Display, Formatter};
use token::TokenKind;

pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

#[derive(NamedVariant, Copy, Clone)]
pub enum Operator {
    ShiftLeft,
    ShiftRight,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    UnaryPlus,
    UnaryMinus,
    Add,
    And,
    Assign,
    Divide,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Modulo,
    Multiply,
    Not,
    NotEqual,
    Or,
    Power,
    Subtract,
}

impl Debug for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.variant_name())
    }
}

impl TokenKind {
    pub fn as_prefix(self) -> Option<(Operator, (), u8)> {
        Some(match self {
            TokenKind::Bang => (Operator::Not, (), 1),
            TokenKind::Plus => (Operator::UnaryPlus, (), 2),
            TokenKind::Minus => (Operator::UnaryMinus, (), 2),
            _ => return None,
        })
    }

    pub fn as_infix(self) -> Option<(Operator, u8, u8)> {
        Some(match self {
            // Lower Precedence
            TokenKind::Equals => (Operator::Assign, 1, 2),

            TokenKind::Or => (Operator::Or, 3, 4),
            TokenKind::And => (Operator::And, 5, 6),

            TokenKind::EqualsEquals => (Operator::Equal, 7, 8),
            TokenKind::BangEquals => (Operator::NotEqual, 7, 8),
            TokenKind::Greater => (Operator::Greater, 9, 10),
            TokenKind::GreaterEquals => (Operator::GreaterEqual, 9, 10),
            TokenKind::Less => (Operator::Less, 9, 10),
            TokenKind::LessEquals => (Operator::LessEqual, 9, 10),

            TokenKind::Pipe => (Operator::BitwiseOr, 11, 12),
            TokenKind::Caret => (Operator::BitwiseXor, 13, 14),
            TokenKind::Ampersand => (Operator::BitwiseAnd, 15, 16),
            TokenKind::LessLess => (Operator::ShiftLeft, 17, 18),
            TokenKind::GreaterGreater => (Operator::ShiftRight, 17, 18),

            TokenKind::Plus => (Operator::Add, 19, 20),
            TokenKind::Minus => (Operator::Subtract, 19, 20),
            TokenKind::Star => (Operator::Multiply, 21, 22),
            TokenKind::Slash => (Operator::Divide, 21, 22),
            TokenKind::Percent => (Operator::Modulo, 21, 22),

            TokenKind::StarStar => (Operator::Power, 24, 23),
            // Higher Precedence
            _ => return None,
        })
    }

    pub fn as_postfix(self) -> Option<(Operator, u8, ())> {
        // Some(match self {
        //     _ => return None,
        // })
        None
    }
}

#[derive(NamedVariant, Clone)]
pub enum NodeKind {
    Block(Vec<Node>),
    Echo(Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Assert(Box<Node>, String),
    UnaryOperation(Operator, Box<Node>),
    BinaryOperation(Operator, Box<Node>, Box<Node>),
    LocalDeclaration(String, Box<Node>),
    GlobalDeclaration(String, Option<Box<Node>>),
    Identifier(String),
    StringLiteral(String),
    FloatLiteral(f64),
    IntegerLiteral(usize),
    BooleanLiteral(bool),
}

impl Debug for NodeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.variant_name())
    }
}

impl NodeKind {
    pub fn make(self, span: Span) -> Node {
        Node {
            kind: self,
            span,
            expr: false,
            expr_stmt: false,
        }
    }
}

#[derive(Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub expr: bool,
    pub expr_stmt: bool,
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            NodeFormatter {
                node: self,
                indent: 0,
            }
        )
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

struct Indent<F> {
    f: F,
    indent: usize,
    stored_space: usize,
}

impl<F: std::fmt::Write> Indent<F> {
    pub fn new(f: F, indent: usize) -> Self {
        Self {
            f,
            indent,
            stored_space: indent,
        }
    }

    pub fn indent(&mut self, indent: usize) {
        self.indent += indent;
        self.stored_space = self.indent;
    }
    pub fn dedent(&mut self, indent: usize) {
        self.indent = self.indent.saturating_sub(indent);
        self.stored_space = self.indent;
    }
}

impl<F: std::fmt::Write> std::fmt::Write for Indent<F> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            self.write_char(c)?;
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        match c {
            '\n' => {
                self.f.write_char('\n')?;
                self.stored_space = self.indent;
            }
            '\r' => {
                self.stored_space = 0;
            }
            '\t' => {
                self.indent(2);
            }
            '\0' => {
                self.dedent(2);
            }
            ' ' => {
                self.stored_space += 1;
            }
            _ if c.is_whitespace() => {
                unimplemented!("unusual space characters aren't allowed");
            }
            _ => {
                for _ in 0..std::mem::take(&mut self.stored_space) {
                    self.f.write_char(' ')?;
                }
                self.f.write_char(c)?;
            }
        }
        Ok(())
    }
}

impl<F: std::fmt::Write> Indent<F> {
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        std::fmt::Write::write_fmt(self, args)
    }
}

struct NodeFormatter<'n> {
    node: &'n Node,
    indent: usize,
}

impl<'n> NodeFormatter<'n> {
    fn child(&self, node: &'n Node) -> Self {
        Self { node, indent: 2 }
    }
}

impl<'a> Display for NodeFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut f = Indent::new(f, self.indent);
        let node = self.node;
        write!(f, "{}", node.kind.variant_name())?;
        match &node.kind {
            NodeKind::If(condition, then_block, else_block) => {
                write!(f, "{{\n{}\n}}", self.child(condition))?;
                write!(f, "{{\n{}\n}}", self.child(then_block))?;
                if let Some(else_block) = else_block {
                    write!(f, "else {{\n{}\n}}", self.child(else_block))?;
                }
            }
            NodeKind::GlobalDeclaration(name, expr) => {
                write!(f, "({name:?})")?;
                if let Some(expr) = expr {
                    write!(f, " {{\n{}\n}}", self.child(expr))?;
                }
            }
            NodeKind::LocalDeclaration(name, expr) => {
                write!(f, "({name:?})")?;
                write!(f, " {{\n{}\n}}", self.child(expr))?;
            }
            NodeKind::UnaryOperation(op, expr) => {
                write!(f, "({}) {{\n{}\n}}", op.variant_name(), self.child(expr))?;
            }
            NodeKind::Echo(expr) => {
                write!(f, "{{\n{}\n}}", self.child(expr))?;
            }
            NodeKind::Assert(expr, message) => {
                write!(f, "{{\n{}\n{}\n}}", self.child(expr), message)?;
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                write!(
                    f,
                    "({}) {{\n{}\n{}\n}}",
                    op.variant_name(),
                    self.child(lhs),
                    self.child(rhs)
                )?;
            }
            NodeKind::StringLiteral(val) => write!(f, "({val:?})")?,
            NodeKind::FloatLiteral(val) => write!(f, "({val})")?,
            NodeKind::IntegerLiteral(val) => write!(f, "({val})")?,
            NodeKind::BooleanLiteral(val) => write!(f, "({val})")?,
            NodeKind::Block(stmts) => {
                writeln!(f, "({} statements) {{", stmts.len())?;
                for stmt in stmts {
                    writeln!(f, "{}", self.child(stmt))?;
                }
                write!(f, "}}")?;
            }
            NodeKind::Identifier(val) => write!(f, "({val:?})")?,
        }
        write!(f, "[{:?}]", self.node.span)?;
        Ok(())
    }
}
