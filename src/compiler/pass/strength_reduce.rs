use crate::ast::{Node, NodeKind, Operator};
use crate::compiler::pass::Pass;
use crate::report::ReportSender;

pub struct StrengthReducePass {
    #[allow(dead_code)]
    reporter: ReportSender,
}

impl StrengthReducePass {
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

                if let Some(new_kind) = self.try_reduce(*op, lhs, rhs, node.span) {
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

    fn try_reduce(
        &self,
        op: Operator,
        lhs: &Node,
        rhs: &Node,
        span: crate::ast::span::Span,
    ) -> Option<NodeKind> {
        match op {
            Operator::Multiply => {
                if let NodeKind::IntegerLiteral(2) = rhs.kind {
                    return Some(NodeKind::BinaryOperation(
                        Operator::ShiftLeft,
                        Box::new(lhs.clone()),
                        Box::new(NodeKind::IntegerLiteral(1).make(span)),
                    ));
                }
                if let NodeKind::IntegerLiteral(2) = lhs.kind {
                    return Some(NodeKind::BinaryOperation(
                        Operator::ShiftLeft,
                        Box::new(rhs.clone()),
                        Box::new(NodeKind::IntegerLiteral(1).make(span)),
                    ));
                }
                if let NodeKind::IntegerLiteral(n) = rhs.kind {
                    if n.is_power_of_two() && n > 1 {
                        let shift = n.trailing_zeros() as usize;
                        return Some(NodeKind::BinaryOperation(
                            Operator::ShiftLeft,
                            Box::new(lhs.clone()),
                            Box::new(NodeKind::IntegerLiteral(shift).make(span)),
                        ));
                    }
                }
                if let NodeKind::IntegerLiteral(n) = lhs.kind {
                    if n.is_power_of_two() && n > 1 {
                        let shift = n.trailing_zeros() as usize;
                        return Some(NodeKind::BinaryOperation(
                            Operator::ShiftLeft,
                            Box::new(rhs.clone()),
                            Box::new(NodeKind::IntegerLiteral(shift).make(span)),
                        ));
                    }
                }
            }
            Operator::Divide => {
                if let NodeKind::IntegerLiteral(n) = rhs.kind {
                    if n.is_power_of_two() && n > 1 {
                        let shift = n.trailing_zeros() as usize;
                        return Some(NodeKind::BinaryOperation(
                            Operator::ShiftRight,
                            Box::new(lhs.clone()),
                            Box::new(NodeKind::IntegerLiteral(shift).make(span)),
                        ));
                    }
                }
            }
            Operator::Power => {
                if let NodeKind::IntegerLiteral(2) = rhs.kind {
                    return Some(NodeKind::BinaryOperation(
                        Operator::Multiply,
                        Box::new(lhs.clone()),
                        Box::new(lhs.clone()),
                    ));
                }
                if let NodeKind::IntegerLiteral(3) = rhs.kind {
                    return Some(NodeKind::BinaryOperation(
                        Operator::Multiply,
                        Box::new(
                            NodeKind::BinaryOperation(
                                Operator::Multiply,
                                Box::new(lhs.clone()),
                                Box::new(lhs.clone()),
                            )
                            .make(span),
                        ),
                        Box::new(lhs.clone()),
                    ));
                }
            }
            Operator::Modulo => {
                if let NodeKind::IntegerLiteral(n) = rhs.kind {
                    if n.is_power_of_two() && n > 0 {
                        return Some(NodeKind::BinaryOperation(
                            Operator::BitwiseAnd,
                            Box::new(lhs.clone()),
                            Box::new(NodeKind::IntegerLiteral(n - 1).make(span)),
                        ));
                    }
                }
            }
            _ => {}
        }

        None
    }
}

impl Pass for StrengthReducePass {
    fn run(&mut self, node: &mut Node) {
        self.process_node(node);
    }
}
