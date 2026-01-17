use crate::ast::span::Span;
use name_variant::NamedVariant;
use std::fmt::{Debug, Display, Formatter};
use token::TokenKind;

pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableKind {
    Local,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResolvedVar {
    pub kind: VariableKind,
    pub index: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Nada,
    Unknown,
    Never,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Nada => write!(f, "nada"),
            Type::Unknown => write!(f, "unknown"),
            Type::Never => write!(f, "never"),
        }
    }
}

impl Type {
    pub fn from_str(s: &str) -> Option<Type> {
        match s {
            "int" => Some(Type::Int),
            "float" => Some(Type::Float),
            "string" => Some(Type::String),
            "bool" => Some(Type::Bool),
            "nada" => Some(Type::Nada),
            _ => None,
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }
}

#[derive(NamedVariant, Copy, Clone, PartialEq, Eq)]
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

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::ShiftLeft => write!(f, "<<"),
            Operator::ShiftRight => write!(f, ">>"),
            Operator::BitwiseOr => write!(f, "|"),
            Operator::BitwiseAnd => write!(f, "&"),
            Operator::BitwiseXor => write!(f, "^"),
            Operator::UnaryPlus => write!(f, "+"),
            Operator::UnaryMinus => write!(f, "-"),
            Operator::Add => write!(f, "+"),
            Operator::And => write!(f, "and"),
            Operator::Assign => write!(f, "="),
            Operator::Divide => write!(f, "/"),
            Operator::Equal => write!(f, "=="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Modulo => write!(f, "%"),
            Operator::Multiply => write!(f, "*"),
            Operator::Not => write!(f, "not"),
            Operator::NotEqual => write!(f, "!="),
            Operator::Or => write!(f, "or"),
            Operator::Power => write!(f, "**"),
            Operator::Subtract => write!(f, "-"),
        }
    }
}

impl Operator {
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            Operator::Equal
                | Operator::NotEqual
                | Operator::Greater
                | Operator::GreaterEqual
                | Operator::Less
                | Operator::LessEqual
        )
    }
}

impl TokenKind {
    pub fn as_prefix(self) -> Option<(Operator, (), u8)> {
        Some(match self {
            TokenKind::Bang => (Operator::Not, (), 15),
            TokenKind::Plus => (Operator::UnaryPlus, (), 15),
            TokenKind::Minus => (Operator::UnaryMinus, (), 15),
            _ => return None,
        })
    }

    pub fn as_infix(self) -> Option<(Operator, u8, u8)> {
        Some(match self {
            // Lower Precedence
            TokenKind::Equals => (Operator::Assign, 1, 2),

            TokenKind::Or => (Operator::Or, 3, 4),

            TokenKind::And => (Operator::And, 4, 5),

            TokenKind::EqualsEquals => (Operator::Equal, 5, 6),
            TokenKind::BangEquals => (Operator::NotEqual, 5, 6),

            TokenKind::Greater => (Operator::Greater, 7, 8),
            TokenKind::GreaterEquals => (Operator::GreaterEqual, 7, 8),
            TokenKind::Less => (Operator::Less, 7, 8),
            TokenKind::LessEquals => (Operator::LessEqual, 7, 8),

            TokenKind::Pipe => (Operator::BitwiseOr, 8, 9),

            TokenKind::Caret => (Operator::BitwiseXor, 9, 10),

            TokenKind::Ampersand => (Operator::BitwiseAnd, 10, 11),

            TokenKind::LessLess => (Operator::ShiftLeft, 11, 12),
            TokenKind::GreaterGreater => (Operator::ShiftRight, 11, 12),

            TokenKind::Plus => (Operator::Add, 12, 13),
            TokenKind::Minus => (Operator::Subtract, 12, 13),

            TokenKind::Star => (Operator::Multiply, 13, 14),
            TokenKind::Slash => (Operator::Divide, 13, 14),
            TokenKind::Percent => (Operator::Modulo, 13, 14),

            TokenKind::StarStar => (Operator::Power, 15, 14),
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
    Continue,
    Break(Option<Box<Node>>),
    Echo(Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Loop(Option<Box<Node>>, Box<Node>),
    Assert(Box<Node>, String),
    UnaryOperation(Operator, Box<Node>),
    BinaryOperation(Operator, Box<Node>, Box<Node>),
    CompoundComparison(Vec<Operator>, Vec<Box<Node>>),
    LocalDeclaration(String, Option<Type>, Box<Node>, Option<u16>),
    ConstDeclaration(String, Option<Type>, Box<Node>),
    Identifier(String, Option<ResolvedVar>),
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
            ty: None,
        }
    }
}

#[derive(Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub expr: bool,
    pub ty: Option<Type>,
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
            NodeKind::Continue => (),
            NodeKind::Break(expr) => {
                if let Some(expr) = expr {
                    write!(f, " {{\n{}\n}}", self.child(expr))?;
                }
            }
            NodeKind::Loop(condition, body) => {
                write!(f, "{{")?;
                if let Some(condition) = condition {
                    write!(f, "\n{}", self.child(condition))?;
                }
                write!(f, "\n{}\n}}", self.child(body))?;
            }
            NodeKind::If(condition, then_block, else_block) => {
                write!(f, "{{\n{}\n}}", self.child(condition))?;
                write!(f, "{{\n{}\n}}", self.child(then_block))?;
                if let Some(else_block) = else_block {
                    write!(f, "else {{\n{}\n}}", self.child(else_block))?;
                }
            }
            NodeKind::LocalDeclaration(name, ty, expr, _) => {
                write!(f, "({name:?}")?;
                if let Some(ty) = ty {
                    write!(f, ": {ty}")?;
                }
                write!(f, ")")?;
                write!(f, " {{\n{}\n}}", self.child(expr))?;
            }
            NodeKind::ConstDeclaration(name, ty, expr) => {
                write!(f, "({name:?}")?;
                if let Some(ty) = ty {
                    write!(f, ": {ty}")?;
                }
                write!(f, ")")?;
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
            NodeKind::CompoundComparison(ops, operands) => {
                let ops_str: Vec<_> = ops.iter().map(|op| op.variant_name()).collect();
                write!(f, "({}) {{", ops_str.join(", "))?;
                for operand in operands {
                    write!(f, "\n{}", self.child(operand))?;
                }
                write!(f, "\n}}")?;
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
            NodeKind::Identifier(val, resolved) => {
                write!(f, "({val:?}")?;
                if let Some(r) = resolved {
                    write!(f, " {:?}@{}", r.kind, r.index)?;
                }
                write!(f, ")")?;
            }
        }
        write!(f, "[{:?}]", self.node.span)?;
        Ok(())
    }
}
