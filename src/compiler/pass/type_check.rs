use crate::ast::{Node, NodeKind, Operator, Type};
use crate::compiler::pass::Pass;
use crate::report::{ReportKind, ReportLevel, ReportSender, SpanToLabel};
use std::collections::HashMap;
use std::fmt::Display;

enum TypeCheckError {
    TypeMismatch { expected: Type, found: Type },
    UndefinedVariable(String),
    InvalidOperandTypes { op: Operator, left: Type, right: Type },
    InvalidUnaryOperand { op: Operator, operand: Type },
    MissingElseBranch,
    IncompatibleBranchTypes { then_ty: Type, else_ty: Type },
}

impl Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeCheckError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            TypeCheckError::UndefinedVariable(name) => {
                write!(f, "Undefined variable: {}", name)
            }
            TypeCheckError::InvalidOperandTypes { op, left, right } => {
                write!(f, "Invalid types for operand {}: {} and {}", op, left, right)
            }
            TypeCheckError::InvalidUnaryOperand { op, operand } => {
                write!(f, "Invalid type for operand {}: {}", op, operand)
            }
            TypeCheckError::MissingElseBranch => {
                write!(f, "If expression used as value requires an else branch")
            }
            TypeCheckError::IncompatibleBranchTypes { then_ty, else_ty } => {
                write!(
                    f,
                    "Incompatible branch types: then is {}, else is {}",
                    then_ty, else_ty
                )
            }
        }
    }
}

impl ReportKind for TypeCheckError {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

struct Scope {
    variables: HashMap<String, Type>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}

pub struct TypeCheckPass {
    reporter: ReportSender,
    scopes: Vec<Scope>,
}

impl TypeCheckPass {
    pub fn new(reporter: ReportSender) -> Self {
        Self {
            reporter,
            scopes: vec![Scope::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_variable(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.variables.insert(name, ty);
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.variables.get(name) {
                return Some(*ty);
            }
        }
        None
    }

    fn check_node(&mut self, node: &mut Node) {
        match &mut node.kind {
            NodeKind::Block(stmts) => {
                self.push_scope();
                let mut last_ty = Type::Nada;
                let len = stmts.len();
                for (i, stmt) in stmts.iter_mut().enumerate() {
                    self.check_node(stmt);
                    if i == len - 1 && stmt.expr {
                        last_ty = stmt.ty.unwrap_or(Type::Nada);
                    }
                }
                self.pop_scope();
                node.ty = Some(last_ty);
            }
            NodeKind::IntegerLiteral(_) => {
                node.ty = Some(Type::Int);
            }
            NodeKind::FloatLiteral(_) => {
                node.ty = Some(Type::Float);
            }
            NodeKind::StringLiteral(_) => {
                node.ty = Some(Type::String);
            }
            NodeKind::BooleanLiteral(_) => {
                node.ty = Some(Type::Bool);
            }
            NodeKind::Identifier(name, _) => match self.lookup_variable(name) {
                Some(ty) => node.ty = Some(ty),
                None => {
                    self.reporter.report(
                        TypeCheckError::UndefinedVariable(name.clone())
                            .make_labeled(node.span.label())
                            .finish()
                            .into(),
                    );
                    node.ty = Some(Type::Unknown);
                }
            },
            NodeKind::LocalDeclaration(name, declared_ty, expr, _) => {
                self.check_node(expr);
                let expr_ty = expr.ty.unwrap_or(Type::Unknown);

                let final_ty = if let Some(declared) = declared_ty {
                    if expr_ty != Type::Unknown && expr_ty != *declared {
                        self.reporter.report(
                            TypeCheckError::TypeMismatch {
                                expected: *declared,
                                found: expr_ty,
                            }
                            .make_labeled(expr.span.label())
                            .finish()
                            .into(),
                        );
                    }
                    *declared
                } else {
                    expr_ty
                };

                self.declare_variable(name.clone(), final_ty);
                node.ty = Some(Type::Nada);
            }
            NodeKind::ConstDeclaration(name, declared_ty, expr) => {
                self.check_node(expr);
                let expr_ty = expr.ty.unwrap_or(Type::Unknown);

                let final_ty = if let Some(declared) = declared_ty {
                    if expr_ty != Type::Unknown && expr_ty != *declared {
                        self.reporter.report(
                            TypeCheckError::TypeMismatch {
                                expected: *declared,
                                found: expr_ty,
                            }
                            .make_labeled(expr.span.label())
                            .finish()
                            .into(),
                        );
                    }
                    *declared
                } else {
                    expr_ty
                };

                self.declare_variable(name.clone(), final_ty);
                node.ty = Some(Type::Nada);
            }
            NodeKind::UnaryOperation(op, operand) => {
                self.check_node(operand);
                let operand_ty = operand.ty.unwrap_or(Type::Unknown);

                let result_ty = match op {
                    Operator::UnaryMinus | Operator::UnaryPlus => {
                        if operand_ty.is_numeric() {
                            operand_ty
                        } else if operand_ty != Type::Unknown {
                            self.reporter.report(
                                TypeCheckError::InvalidUnaryOperand {
                                    op: *op,
                                    operand: operand_ty,
                                }
                                .make_labeled(node.span.label())
                                .finish()
                                .into(),
                            );
                            Type::Unknown
                        } else {
                            Type::Unknown
                        }
                    }
                    Operator::Not => Type::Bool,
                    _ => Type::Unknown,
                };
                node.ty = Some(result_ty);
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                self.check_node(lhs);
                self.check_node(rhs);
                let lhs_ty = lhs.ty.unwrap_or(Type::Unknown);
                let rhs_ty = rhs.ty.unwrap_or(Type::Unknown);

                let result_ty = self.infer_binary_op_type(*op, lhs_ty, rhs_ty, node);
                node.ty = Some(result_ty);
            }
            NodeKind::CompoundComparison(_, operands) => {
                for operand in operands.iter_mut() {
                    self.check_node(operand);
                }
                node.ty = Some(Type::Bool);
            }
            NodeKind::If(condition, then_block, else_block) => {
                self.check_node(condition);
                self.check_node(then_block);
                let then_ty = then_block.ty.unwrap_or(Type::Nada);

                if let Some(else_b) = else_block {
                    self.check_node(else_b);
                    let else_ty = else_b.ty.unwrap_or(Type::Nada);

                    if node.expr {
                        node.ty = Some(Type::Nada);
                    } else if then_ty == else_ty {
                        node.ty = Some(then_ty);
                    } else if then_ty == Type::Never {
                        node.ty = Some(else_ty);
                    } else if else_ty == Type::Never {
                        node.ty = Some(then_ty);
                    } else {
                        self.reporter.report(
                            TypeCheckError::IncompatibleBranchTypes { then_ty, else_ty }
                                .make_labeled(node.span.label())
                                .finish()
                                .into(),
                        );
                        node.ty = Some(Type::Unknown);
                    }
                } else if node.expr {
                    node.ty = Some(Type::Nada);
                } else {
                    self.reporter.report(
                        TypeCheckError::MissingElseBranch
                            .make_labeled(node.span.label())
                            .finish()
                            .into(),
                    );
                    node.ty = Some(Type::Unknown);
                }
            }
            NodeKind::Loop(condition, body) => {
                if let Some(cond) = condition {
                    self.check_node(cond);
                }
                self.check_node(body);
                node.ty = Some(Type::Nada);
            }
            NodeKind::Break(value) => {
                if let Some(val) = value {
                    self.check_node(val);
                }
                node.ty = Some(Type::Never);
            }
            NodeKind::Continue => {
                node.ty = Some(Type::Never);
            }
            NodeKind::Echo(expr) => {
                self.check_node(expr);
                node.ty = Some(Type::Nada);
            }
            NodeKind::Assert(expr, _) => {
                self.check_node(expr);
                node.ty = Some(Type::Nada);
            }
        }
    }

    fn infer_binary_op_type(&mut self, op: Operator, lhs: Type, rhs: Type, node: &Node) -> Type {
        if lhs == Type::Unknown || rhs == Type::Unknown {
            return Type::Unknown;
        }

        match op {
            Operator::Assign => rhs,

            Operator::Add => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Int, Type::Float) | (Type::Float, Type::Int) => Type::Float,
                (Type::String, Type::String) => Type::String,
                _ => {
                    self.reporter.report(
                        TypeCheckError::InvalidOperandTypes {
                            op,
                            left: lhs,
                            right: rhs,
                        }
                        .make_labeled(node.span.label())
                        .finish()
                        .into(),
                    );
                    Type::Unknown
                }
            },

            Operator::Subtract | Operator::Multiply | Operator::Power => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Int, Type::Float) | (Type::Float, Type::Int) => Type::Float,
                (Type::String, Type::Int) if op == Operator::Multiply => Type::String,
                _ => {
                    self.reporter.report(
                        TypeCheckError::InvalidOperandTypes {
                            op,
                            left: lhs,
                            right: rhs,
                        }
                        .make_labeled(node.span.label())
                        .finish()
                        .into(),
                    );
                    Type::Unknown
                }
            },

            Operator::Divide => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Float,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Int, Type::Float) | (Type::Float, Type::Int) => Type::Float,
                _ => {
                    self.reporter.report(
                        TypeCheckError::InvalidOperandTypes {
                            op,
                            left: lhs,
                            right: rhs,
                        }
                        .make_labeled(node.span.label())
                        .finish()
                        .into(),
                    );
                    Type::Unknown
                }
            },

            Operator::Modulo => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Int, Type::Float) | (Type::Float, Type::Int) => Type::Float,
                _ => {
                    self.reporter.report(
                        TypeCheckError::InvalidOperandTypes {
                            op,
                            left: lhs,
                            right: rhs,
                        }
                        .make_labeled(node.span.label())
                        .finish()
                        .into(),
                    );
                    Type::Unknown
                }
            },

            Operator::Equal | Operator::NotEqual => Type::Bool,

            Operator::Less | Operator::LessEqual | Operator::Greater | Operator::GreaterEqual => {
                if lhs.is_numeric() && rhs.is_numeric() {
                    Type::Bool
                } else {
                    self.reporter.report(
                        TypeCheckError::InvalidOperandTypes {
                            op,
                            left: lhs,
                            right: rhs,
                        }
                        .make_labeled(node.span.label())
                        .finish()
                        .into(),
                    );
                    Type::Bool
                }
            }

            Operator::And | Operator::Or => Type::Bool,

            Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseXor
            | Operator::ShiftLeft
            | Operator::ShiftRight => {
                if lhs == Type::Int && rhs == Type::Int {
                    Type::Int
                } else {
                    self.reporter.report(
                        TypeCheckError::InvalidOperandTypes {
                            op,
                            left: lhs,
                            right: rhs,
                        }
                        .make_labeled(node.span.label())
                        .finish()
                        .into(),
                    );
                    Type::Unknown
                }
            }

            _ => Type::Unknown,
        }
    }
}

impl Pass for TypeCheckPass {
    fn run(&mut self, node: &mut Node) {
        self.check_node(node);
    }
}
