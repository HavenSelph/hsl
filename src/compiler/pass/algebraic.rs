use crate::ast::{Node, NodeKind, Operator};
use crate::compiler::pass::Pass;
use crate::report::{ReportKind, ReportLevel, ReportSender, SpanToLabel};
use std::fmt::Display;

enum AlgebraicWarning {
    SimplifiedExpression(String),
}

impl Display for AlgebraicWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AlgebraicWarning::SimplifiedExpression(desc) => {
                write!(f, "Expression simplified: {}", desc)
            }
        }
    }
}

impl ReportKind for AlgebraicWarning {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Advice
    }
}

pub struct AlgebraicPass {
    reporter: ReportSender,
}

impl AlgebraicPass {
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
            NodeKind::UnaryOperation(_, operand) => {
                self.process_node(operand);
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                self.process_node(lhs);
                self.process_node(rhs);

                if let Some((new_kind, desc)) = self.try_simplify(*op, lhs, rhs) {
                    self.reporter.report(
                        AlgebraicWarning::SimplifiedExpression(desc)
                            .make_labeled(node.span.label())
                            .finish()
                            .into(),
                    );
                    node.kind = new_kind;
                }
            }
            NodeKind::CompoundComparison(_, operands) => {
                for operand in operands.iter_mut() {
                    self.process_node(operand);
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
            _ => {}
        }
    }

    fn try_simplify(&self, op: Operator, lhs: &Node, rhs: &Node) -> Option<(NodeKind, String)> {
        let is_zero_int = |n: &Node| matches!(n.kind, NodeKind::IntegerLiteral(0));
        let is_zero_float = |n: &Node| matches!(n.kind, NodeKind::FloatLiteral(f) if f == 0.0);
        let is_zero = |n: &Node| is_zero_int(n) || is_zero_float(n);

        let is_one_int = |n: &Node| matches!(n.kind, NodeKind::IntegerLiteral(1));
        let is_one_float = |n: &Node| matches!(n.kind, NodeKind::FloatLiteral(f) if f == 1.0);
        let is_one = |n: &Node| is_one_int(n) || is_one_float(n);

        let is_true = |n: &Node| matches!(n.kind, NodeKind::BooleanLiteral(true));
        let is_false = |n: &Node| matches!(n.kind, NodeKind::BooleanLiteral(false));

        match op {
            Operator::Add => {
                if is_zero(rhs) {
                    return Some((lhs.kind.clone(), "x + 0 → x".to_string()));
                }
                if is_zero(lhs) {
                    return Some((rhs.kind.clone(), "0 + x → x".to_string()));
                }
            }
            Operator::Subtract => {
                if is_zero(rhs) {
                    return Some((lhs.kind.clone(), "x - 0 → x".to_string()));
                }
            }
            Operator::Multiply => {
                if is_zero(lhs) {
                    return Some((lhs.kind.clone(), "0 * x → 0".to_string()));
                }
                if is_zero(rhs) {
                    return Some((rhs.kind.clone(), "x * 0 → 0".to_string()));
                }
                if is_one(rhs) {
                    return Some((lhs.kind.clone(), "x * 1 → x".to_string()));
                }
                if is_one(lhs) {
                    return Some((rhs.kind.clone(), "1 * x → x".to_string()));
                }
            }
            Operator::Divide => {
                if is_one(rhs) {
                    return Some((lhs.kind.clone(), "x / 1 → x".to_string()));
                }
            }
            Operator::Power => {
                if is_zero(rhs) {
                    return Some((NodeKind::IntegerLiteral(1), "x ** 0 → 1".to_string()));
                }
                if is_one(rhs) {
                    return Some((lhs.kind.clone(), "x ** 1 → x".to_string()));
                }
            }
            Operator::And => {
                if is_true(lhs) {
                    return Some((rhs.kind.clone(), "True and x → x".to_string()));
                }
                if is_true(rhs) {
                    return Some((lhs.kind.clone(), "x and True → x".to_string()));
                }
                if is_false(lhs) {
                    return Some((
                        NodeKind::BooleanLiteral(false),
                        "False and x → False".to_string(),
                    ));
                }
                if is_false(rhs) {
                    return Some((
                        NodeKind::BooleanLiteral(false),
                        "x and False → False".to_string(),
                    ));
                }
            }
            Operator::Or => {
                if is_false(lhs) {
                    return Some((rhs.kind.clone(), "False or x → x".to_string()));
                }
                if is_false(rhs) {
                    return Some((lhs.kind.clone(), "x or False → x".to_string()));
                }
                if is_true(lhs) {
                    return Some((
                        NodeKind::BooleanLiteral(true),
                        "True or x → True".to_string(),
                    ));
                }
                if is_true(rhs) {
                    return Some((
                        NodeKind::BooleanLiteral(true),
                        "x or True → True".to_string(),
                    ));
                }
            }
            Operator::BitwiseAnd => {
                if is_zero(lhs) || is_zero(rhs) {
                    return Some((NodeKind::IntegerLiteral(0), "x & 0 → 0".to_string()));
                }
            }
            Operator::BitwiseOr => {
                if is_zero(lhs) {
                    return Some((rhs.kind.clone(), "0 | x → x".to_string()));
                }
                if is_zero(rhs) {
                    return Some((lhs.kind.clone(), "x | 0 → x".to_string()));
                }
            }
            Operator::BitwiseXor => {
                if is_zero(lhs) {
                    return Some((rhs.kind.clone(), "0 ^ x → x".to_string()));
                }
                if is_zero(rhs) {
                    return Some((lhs.kind.clone(), "x ^ 0 → x".to_string()));
                }
            }
            Operator::ShiftLeft | Operator::ShiftRight => {
                if is_zero(rhs) {
                    return Some((lhs.kind.clone(), "x << 0 → x / x >> 0 → x".to_string()));
                }
                if is_zero(lhs) {
                    return Some((
                        NodeKind::IntegerLiteral(0),
                        "0 << x → 0 / 0 >> x → 0".to_string(),
                    ));
                }
            }
            _ => {}
        }

        None
    }
}

impl Pass for AlgebraicPass {
    fn run(&mut self, node: &mut Node) {
        self.process_node(node);
    }
}
