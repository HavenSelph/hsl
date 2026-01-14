use crate::ast::{Node, NodeKind, Operator};
use crate::compiler::pass::Pass;
use crate::report::{ReportKind, ReportLevel, ReportSender, SpanToLabel};
use std::fmt::Display;

enum DeadCodeWarning {
    EffectlessExpression,
}

impl Display for DeadCodeWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeadCodeWarning::EffectlessExpression => {
                write!(f, "Expression has no effect and will be removed")
            }
        }
    }
}

impl ReportKind for DeadCodeWarning {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Warn
    }
}

pub struct DeadCodePass {
    reporter: ReportSender,
}

impl DeadCodePass {
    pub fn new(reporter: ReportSender) -> Self {
        Self { reporter }
    }

    fn process_node(&mut self, node: &mut Node) {
        match &mut node.kind {
            NodeKind::Block(stmts) => {
                let mut i = 0;
                while i < stmts.len() {
                    self.process_node(&mut stmts[i]);

                    if stmts[i].expr && self.is_effectless(&stmts[i]) {
                        self.reporter.report(
                            DeadCodeWarning::EffectlessExpression
                                .make_labeled(stmts[i].span.label())
                                .finish()
                                .into(),
                        );
                        stmts.remove(i);
                    } else {
                        i += 1;
                    }
                }
            }
            NodeKind::If(condition, then_block, else_block) => {
                self.process_node(condition);
                self.process_node(then_block);
                if let Some(else_b) = else_block {
                    self.process_node(else_b);
                }
            }
            NodeKind::Loop(condition, body) => {
                if let Some(cond) = condition {
                    self.process_node(cond);
                }
                self.process_node(body);
            }
            NodeKind::LocalDeclaration(_, _, expr, _) => {
                self.process_node(expr);
            }
            NodeKind::GlobalDeclaration(_, _, expr, _) => {
                if let Some(e) = expr {
                    self.process_node(e);
                }
            }
            NodeKind::Echo(expr) => {
                self.process_node(expr);
            }
            NodeKind::Assert(expr, _) => {
                self.process_node(expr);
            }
            NodeKind::Break(value) => {
                if let Some(val) = value {
                    self.process_node(val);
                }
            }
            NodeKind::UnaryOperation(_, operand) => {
                self.process_node(operand);
            }
            NodeKind::BinaryOperation(_, lhs, rhs) => {
                self.process_node(lhs);
                self.process_node(rhs);
            }
            NodeKind::CompoundComparison(_, operands) => {
                for operand in operands.iter_mut() {
                    self.process_node(operand);
                }
            }
            _ => {}
        }
    }

    fn is_effectless(&self, node: &Node) -> bool {
        match &node.kind {
            NodeKind::IntegerLiteral(_)
            | NodeKind::FloatLiteral(_)
            | NodeKind::StringLiteral(_)
            | NodeKind::BooleanLiteral(_)
            | NodeKind::Identifier(..) => true,

            NodeKind::UnaryOperation(op, operand) => match op {
                Operator::UnaryPlus | Operator::UnaryMinus | Operator::Not => {
                    self.is_effectless(operand)
                }
                _ => false,
            },

            NodeKind::BinaryOperation(op, lhs, rhs) => match op {
                Operator::Assign => false,
                Operator::Add
                | Operator::Subtract
                | Operator::Multiply
                | Operator::Divide
                | Operator::Modulo
                | Operator::Power
                | Operator::Equal
                | Operator::NotEqual
                | Operator::Less
                | Operator::LessEqual
                | Operator::Greater
                | Operator::GreaterEqual
                | Operator::And
                | Operator::Or
                | Operator::BitwiseAnd
                | Operator::BitwiseOr
                | Operator::BitwiseXor
                | Operator::ShiftLeft
                | Operator::ShiftRight => self.is_effectless(lhs) && self.is_effectless(rhs),
                _ => false,
            },

            NodeKind::CompoundComparison(_, operands) => {
                operands.iter().all(|op| self.is_effectless(op))
            }

            _ => false,
        }
    }
}

impl Pass for DeadCodePass {
    fn run(&mut self, node: &mut Node) {
        self.process_node(node);
    }
}
