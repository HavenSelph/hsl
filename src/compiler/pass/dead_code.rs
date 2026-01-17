use crate::ast::{Node, NodeKind, Operator};
use crate::compiler::pass::Pass;
use crate::report::{ReportKind, ReportLevel, ReportSender, SpanToLabel};
use std::fmt::Display;

enum DeadCodeWarning {
    EffectlessExpression,
    DeadThenBranch,
    DeadElseBranch,
    ConditionAlways(bool),
    UnreachableCode,
}

impl Display for DeadCodeWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeadCodeWarning::EffectlessExpression => {
                write!(f, "Expression has no effect and will be removed")
            }
            DeadCodeWarning::DeadThenBranch => {
                write!(
                    f,
                    "Then branch will never execute (condition is always false)"
                )
            }
            DeadCodeWarning::DeadElseBranch => {
                write!(
                    f,
                    "Else branch will never execute (condition is always true)"
                )
            }
            DeadCodeWarning::ConditionAlways(b) => {
                write!(f, "Condition is always {b}")
            }
            DeadCodeWarning::UnreachableCode => {
                write!(f, "Unreachable code detected and will be removed")
            }
        }
    }
}

impl ReportKind for DeadCodeWarning {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Warning
    }
}

pub struct DeadCodePass {
    reporter: ReportSender,
}

impl DeadCodePass {
    pub fn new(reporter: ReportSender) -> Self {
        Self { reporter }
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
}

impl Pass for DeadCodePass {
    fn visit_block(&mut self, node: &mut Node) {
        if let NodeKind::Block(stmts) = &mut node.kind {
            let mut found_diverging = None;
            let mut i = 0;

            while i < stmts.len() {
                self.visit(&mut stmts[i]);

                // Check for unreachable code after diverging statement
                if found_diverging.is_none() && self.is_diverging(&stmts[i]) {
                    found_diverging = Some(i);
                }

                // Remove effectless expressions (expr=true means it's an expression)
                if !stmts[i].expr && self.is_effectless(&stmts[i]) {
                    self.reporter.report(
                        DeadCodeWarning::EffectlessExpression
                            .make_labeled(stmts[i].span.label())
                            .finish()
                            .into(),
                    );
                    stmts.remove(i);
                // Remove empty blocks
                } else if matches!(&stmts[i].kind, NodeKind::Block(inner) if inner.is_empty()) {
                    stmts.remove(i);
                } else {
                    i += 1;
                }
            }

            // Remove unreachable code after diverging statement
            if let Some(_diverge_idx) = found_diverging {
                // Recalculate the actual index after potential removals
                let actual_idx = stmts.iter().position(|s| self.is_diverging(s));
                if let Some(idx) = actual_idx {
                    if idx + 1 < stmts.len() {
                        for stmt in stmts.iter().skip(idx + 1) {
                            self.reporter.report(
                                DeadCodeWarning::UnreachableCode
                                    .make_labeled(stmt.span.label())
                                    .finish()
                                    .into(),
                            );
                        }
                        stmts.truncate(idx + 1);
                    }
                }
            }
        }
    }

    fn visit_if(&mut self, node: &mut Node) {
        if let NodeKind::If(condition, then_block, else_block) = &mut node.kind {
            self.visit(condition);
            self.visit(then_block);
            if let Some(else_b) = else_block {
                self.visit(else_b);
            }

            if let NodeKind::BooleanLiteral(val) = condition.kind {
                if val {
                    self.reporter.report(
                        DeadCodeWarning::DeadElseBranch
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
                        DeadCodeWarning::DeadThenBranch
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
    }

    fn visit_loop(&mut self, node: &mut Node) {
        if let NodeKind::Loop(condition, body) = &mut node.kind {
            if let Some(cond) = condition {
                self.visit(cond);

                if let NodeKind::BooleanLiteral(b) = cond.kind {
                    self.reporter.report(
                        DeadCodeWarning::ConditionAlways(b)
                            .make_labeled(cond.span.label())
                            .finish()
                            .into(),
                    );
                    node.kind = NodeKind::Block(vec![]);
                    node.ty = Some(crate::ast::Type::Nada);
                    return;
                }
            }
            self.visit(body);
        }
    }

    fn run(&mut self, node: &mut Node) {
        self.visit(node);
    }
}
