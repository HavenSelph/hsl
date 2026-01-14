use crate::ast::{Node, NodeKind, Operator};
use crate::compiler::pass::Pass;
use crate::report::ReportSender;
use crate::vm::Value;

pub struct ConstFoldPass {
    #[allow(dead_code)]
    reporter: ReportSender,
}

impl ConstFoldPass {
    pub fn new(reporter: ReportSender) -> Self {
        Self { reporter }
    }

    fn fold_node(&mut self, node: &mut Node) {
        match &mut node.kind {
            NodeKind::Block(stmts) => {
                for stmt in stmts.iter_mut() {
                    self.fold_node(stmt);
                }
            }
            NodeKind::UnaryOperation(op, operand) => {
                self.fold_node(operand);
                if let Some(folded) = self.try_fold_unary(*op, operand) {
                    node.kind = folded;
                }
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                self.fold_node(lhs);
                self.fold_node(rhs);
                if let Some(folded) = self.try_fold_binary(*op, lhs, rhs) {
                    node.kind = folded;
                }
            }
            NodeKind::CompoundComparison(ops, operands) => {
                for operand in operands.iter_mut() {
                    self.fold_node(operand);
                }
                if let Some(folded) = self.try_fold_compound_comparison(ops, operands) {
                    node.kind = folded;
                }
            }
            NodeKind::If(condition, then_block, else_block) => {
                self.fold_node(condition);
                self.fold_node(then_block);
                if let Some(else_b) = else_block {
                    self.fold_node(else_b);
                }
            }
            NodeKind::Loop(condition, body) => {
                if let Some(cond) = condition {
                    self.fold_node(cond);
                }
                self.fold_node(body);
            }
            NodeKind::LocalDeclaration(_, _, expr, _) => {
                self.fold_node(expr);
            }
            NodeKind::GlobalDeclaration(_, _, expr, _) => {
                if let Some(e) = expr {
                    self.fold_node(e);
                }
            }
            NodeKind::Echo(expr) => {
                self.fold_node(expr);
            }
            NodeKind::Assert(expr, _) => {
                self.fold_node(expr);
            }
            NodeKind::Break(value) => {
                if let Some(val) = value {
                    self.fold_node(val);
                }
            }
            _ => {}
        }
    }

    fn try_fold_unary(&self, op: Operator, operand: &Node) -> Option<NodeKind> {
        match op {
            Operator::UnaryMinus => match &operand.kind {
                NodeKind::IntegerLiteral(val) => {
                    let negated = -(*val as isize);
                    if negated >= 0 {
                        Some(NodeKind::IntegerLiteral(negated as usize))
                    } else {
                        None
                    }
                }
                NodeKind::FloatLiteral(val) => Some(NodeKind::FloatLiteral(-val)),
                _ => None,
            },
            Operator::UnaryPlus => match &operand.kind {
                NodeKind::IntegerLiteral(_) | NodeKind::FloatLiteral(_) => {
                    Some(operand.kind.clone())
                }
                _ => None,
            },
            Operator::Not => match &operand.kind {
                NodeKind::BooleanLiteral(val) => Some(NodeKind::BooleanLiteral(!val)),
                _ => None,
            },
            _ => None,
        }
    }

    fn try_fold_binary(&self, op: Operator, lhs: &Node, rhs: &Node) -> Option<NodeKind> {
        let lhs_val = self.node_to_value(lhs)?;
        let rhs_val = self.node_to_value(rhs)?;

        let result = match op {
            Operator::Add => lhs_val.add(&rhs_val).ok()?,
            Operator::Subtract => lhs_val.sub(&rhs_val).ok()?,
            Operator::Multiply => lhs_val.mul(&rhs_val).ok()?,
            Operator::Divide => lhs_val.div(&rhs_val).ok()?,
            Operator::Modulo => lhs_val.modulo(&rhs_val).ok()?,
            Operator::Power => lhs_val.pow(&rhs_val).ok()?,
            Operator::Equal => lhs_val.equal(&rhs_val).ok()?,
            Operator::NotEqual => lhs_val.not_equal(&rhs_val).ok()?,
            Operator::Less => lhs_val.less(&rhs_val).ok()?,
            Operator::LessEqual => lhs_val.less_equal(&rhs_val).ok()?,
            Operator::Greater => lhs_val.greater(&rhs_val).ok()?,
            Operator::GreaterEqual => lhs_val.greater_equal(&rhs_val).ok()?,
            Operator::BitwiseAnd => lhs_val.bit_and(&rhs_val).ok()?,
            Operator::BitwiseOr => lhs_val.bit_or(&rhs_val).ok()?,
            Operator::BitwiseXor => lhs_val.bit_xor(&rhs_val).ok()?,
            Operator::ShiftLeft => lhs_val.shift_left(&rhs_val).ok()?,
            Operator::ShiftRight => lhs_val.shift_right(&rhs_val).ok()?,
            Operator::And => lhs_val.and(&rhs_val).ok()?,
            Operator::Or => lhs_val.or(&rhs_val).ok()?,
            _ => return None,
        };

        self.value_to_node_kind(result)
    }

    fn try_fold_compound_comparison(
        &self,
        ops: &[Operator],
        operands: &[Box<Node>],
    ) -> Option<NodeKind> {
        if operands.len() < 2 {
            return None;
        }

        let values: Vec<Value> = operands
            .iter()
            .map(|op| self.node_to_value(op))
            .collect::<Option<Vec<_>>>()?;

        let mut result = true;
        for i in 0..ops.len() {
            let cmp_result = match ops[i] {
                Operator::Less => values[i].less(&values[i + 1]).ok()?,
                Operator::LessEqual => values[i].less_equal(&values[i + 1]).ok()?,
                Operator::Greater => values[i].greater(&values[i + 1]).ok()?,
                Operator::GreaterEqual => values[i].greater_equal(&values[i + 1]).ok()?,
                Operator::Equal => values[i].equal(&values[i + 1]).ok()?,
                Operator::NotEqual => values[i].not_equal(&values[i + 1]).ok()?,
                _ => return None,
            };

            if let Value::Boolean(b) = cmp_result {
                if !b {
                    result = false;
                    break;
                }
            } else {
                return None;
            }
        }

        Some(NodeKind::BooleanLiteral(result))
    }

    fn node_to_value(&self, node: &Node) -> Option<Value> {
        match &node.kind {
            NodeKind::IntegerLiteral(val) => Some(Value::Integer(*val as isize)),
            NodeKind::FloatLiteral(val) => Some(Value::Float(*val)),
            NodeKind::StringLiteral(val) => Some(Value::String(val.clone())),
            NodeKind::BooleanLiteral(val) => Some(Value::Boolean(*val)),
            _ => None,
        }
    }

    fn value_to_node_kind(&self, value: Value) -> Option<NodeKind> {
        match value {
            Value::Integer(val) => Some(NodeKind::IntegerLiteral(val as usize)),
            Value::Float(val) => Some(NodeKind::FloatLiteral(val)),
            Value::String(val) => Some(NodeKind::StringLiteral(val)),
            Value::Boolean(val) => Some(NodeKind::BooleanLiteral(val)),
            Value::Nada => None,
        }
    }
}

impl Pass for ConstFoldPass {
    fn run(&mut self, node: &mut Node) {
        self.fold_node(node);
    }
}
