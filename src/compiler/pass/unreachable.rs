use crate::ast::{Node, NodeKind};
use crate::compiler::pass::Pass;
use crate::report::{ReportKind, ReportLevel, ReportSender, SpanToLabel};
use std::fmt::Display;

enum UnreachableWarning {
    UnreachableCode,
}

impl Display for UnreachableWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnreachableWarning::UnreachableCode => {
                write!(f, "Unreachable code detected and will be removed")
            }
        }
    }
}

impl ReportKind for UnreachableWarning {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Warn
    }
}

pub struct UnreachablePass {
    reporter: ReportSender,
}

impl UnreachablePass {
    pub fn new(reporter: ReportSender) -> Self {
        Self { reporter }
    }

    fn is_diverging(&self, node: &Node) -> bool {
        match &node.kind {
            NodeKind::Break(_) | NodeKind::Continue => true,
            NodeKind::Block(stmts) => stmts.iter().any(|s| self.is_diverging(s)),
            NodeKind::If(_, then_block, Some(else_block)) => {
                self.is_diverging(then_block) && self.is_diverging(else_block)
            }
            _ => false,
        }
    }

    fn process_node(&mut self, node: &mut Node) {
        match &mut node.kind {
            NodeKind::Block(stmts) => {
                let mut found_diverging = None;

                for (i, stmt) in stmts.iter_mut().enumerate() {
                    self.process_node(stmt);

                    if found_diverging.is_none() && self.is_diverging(stmt) {
                        found_diverging = Some(i);
                    }
                }

                if let Some(diverge_idx) = found_diverging {
                    if diverge_idx + 1 < stmts.len() {
                        for stmt in stmts.iter().skip(diverge_idx + 1) {
                            self.reporter.report(
                                UnreachableWarning::UnreachableCode
                                    .make_labeled(stmt.span.label())
                                    .finish()
                                    .into(),
                            );
                        }
                        stmts.truncate(diverge_idx + 1);
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
}

impl Pass for UnreachablePass {
    fn run(&mut self, node: &mut Node) {
        self.process_node(node);
    }
}
