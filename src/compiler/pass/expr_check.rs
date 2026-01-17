use crate::ast::{Node, NodeKind, Type};
use crate::compiler::pass::Pass;
use crate::report::ReportSender;

/// This pass propagates `node.expr` status through blocks.
/// 
/// `expr = true` means it's an expression that leaves a value on the stack.
/// `expr = false` means it's a statement that does not leave a value.
///
/// For blocks:
/// - A block's expr status is inherited from its last statement
/// - An empty block is an expression that produces Nada
///
/// For if expressions:
/// - An if with else where both branches are expressions is an expression
/// - Otherwise it's a statement
pub struct ExprCheckPass {
    #[allow(dead_code)]
    reporter: ReportSender,
}

impl ExprCheckPass {
    pub fn new(reporter: ReportSender) -> Self {
        Self { reporter }
    }
}

impl Pass for ExprCheckPass {
    fn visit_block(&mut self, node: &mut Node) {
        if let NodeKind::Block(stmts) = &mut node.kind {
            // Visit all children first
            for stmt in stmts.iter_mut() {
                self.visit(stmt);
            }

            // A block's expr status depends on its last statement
            if stmts.is_empty() {
                // Empty block produces Nada, so it's an expression (leaves value on stack)
                node.expr = true;
                node.ty = Some(Type::Nada);
            } else {
                let last = stmts.last().unwrap();
                // Block inherits expr status from last statement
                node.expr = last.expr;
                node.ty = last.ty;
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

            // Only set expr if not already set by parser (i.e., if it's still true/expression)
            // An if is an expression only if it has an else and both branches are expressions
            if node.expr {
                if let Some(else_b) = else_block {
                    if !then_block.expr || !else_b.expr {
                        // At least one branch is a statement, so if is a statement
                        node.expr = false;
                    }
                    // else: both branches are expressions, if stays as expression
                } else {
                    // No else branch, always a statement
                    node.expr = false;
                }
            }
        }
    }

    fn run(&mut self, node: &mut Node) {
        self.visit(node);
    }
}
