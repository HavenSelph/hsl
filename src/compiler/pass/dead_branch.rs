use crate::ast::{Node, NodeKind};
use crate::compiler::pass::Pass;
use crate::report::{ReportKind, ReportLevel, ReportSender, SpanToLabel};
use std::fmt::Display;

enum DeadBranchWarning {
    DeadThenBranch,
    DeadElseBranch,
    ConditionAlways(bool),
}

impl Display for DeadBranchWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeadBranchWarning::DeadThenBranch => {
                write!(
                    f,
                    "Then branch will never execute (condition is always false)"
                )
            }
            DeadBranchWarning::DeadElseBranch => {
                write!(
                    f,
                    "Else branch will never execute (condition is always true)"
                )
            }
            DeadBranchWarning::ConditionAlways(b) => {
                write!(f, "Condition is always {b}")
            }
        }
    }
}

impl ReportKind for DeadBranchWarning {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Warn
    }
}

pub struct DeadBranchPass {
    reporter: ReportSender,
}

impl DeadBranchPass {
    pub fn new(reporter: ReportSender) -> Self {
        Self { reporter }
    }

    fn process_node(&mut self, node: &mut Node) {
        match &mut node.kind {
            NodeKind::Block(stmts) => {
                for stmt in stmts.iter_mut() {
                    self.process_node(stmt);
                }
            }
            NodeKind::If(condition, then_block, else_block) => {
                self.process_node(condition);
                self.process_node(then_block);
                if let Some(else_b) = else_block {
                    self.process_node(else_b);
                }

                if let NodeKind::BooleanLiteral(val) = condition.kind {
                    if val {
                        self.reporter.report(
                            DeadBranchWarning::DeadElseBranch
                                .make_labeled(
                                    else_block
                                        .as_ref()
                                        .map(|b| b.span)
                                        .unwrap_or(condition.span)
                                        .label(),
                                )
                                .finish()
                                .into(),
                        );
                        let replacement = std::mem::replace(
                            then_block,
                            Box::new(NodeKind::BooleanLiteral(false).make(node.span)),
                        );
                        node.kind = replacement.kind;
                        node.ty = replacement.ty;
                    } else {
                        self.reporter.report(
                            DeadBranchWarning::DeadThenBranch
                                .make_labeled(then_block.span.label())
                                .finish()
                                .into(),
                        );
                        if let Some(else_b) = else_block.take() {
                            node.kind = else_b.kind;
                            node.ty = else_b.ty;
                        } else {
                            node.kind = NodeKind::Block(vec![]);
                            node.ty = Some(crate::ast::Type::Nada);
                        }
                    }
                }
            }
            NodeKind::Loop(condition, body) => {
                if let Some(cond) = condition {
                    self.process_node(cond);

                    if let NodeKind::BooleanLiteral(b) = cond.kind {
                        self.reporter.report(
                            DeadBranchWarning::ConditionAlways(b)
                                .make_labeled(cond.span.label())
                                .finish()
                                .into(),
                        );
                        node.kind = NodeKind::Block(vec![]);
                        node.ty = Some(crate::ast::Type::Nada);
                        return;
                    }
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

impl Pass for DeadBranchPass {
    fn run(&mut self, node: &mut Node) {
        self.process_node(node);
    }
}
