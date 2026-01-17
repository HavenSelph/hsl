use crate::ARGS;
use crate::ast::{Node, NodeKind};
use crate::report::ReportSender;

mod const_fold;
mod dead_code;
mod expr_check;
mod type_check;
mod var_resolve;

use const_fold::ConstFoldPass;
use dead_code::DeadCodePass;
use expr_check::ExprCheckPass;
use type_check::TypeCheckPass;
use var_resolve::VarResolvePass;

pub trait Pass {
    /// Main entry point - dispatches to specific visit methods
    fn visit(&mut self, node: &mut Node) {
        match &node.kind {
            NodeKind::Block(..) => self.visit_block(node),
            NodeKind::Continue => self.visit_continue(node),
            NodeKind::Break(..) => self.visit_break(node),
            NodeKind::Echo(..) => self.visit_echo(node),
            NodeKind::If(..) => self.visit_if(node),
            NodeKind::Loop(..) => self.visit_loop(node),
            NodeKind::Assert(..) => self.visit_assert(node),
            NodeKind::UnaryOperation(..) => self.visit_unary(node),
            NodeKind::BinaryOperation(..) => self.visit_binary(node),
            NodeKind::CompoundComparison(..) => self.visit_compound(node),
            NodeKind::LocalDeclaration(..) => self.visit_local_decl(node),
            NodeKind::ConstDeclaration(..) => self.visit_const_decl(node),
            NodeKind::Identifier(..) => self.visit_identifier(node),
            NodeKind::StringLiteral(..) => self.visit_string(node),
            NodeKind::FloatLiteral(..) => self.visit_float(node),
            NodeKind::IntegerLiteral(..) => self.visit_integer(node),
            NodeKind::BooleanLiteral(..) => self.visit_boolean(node),
        }
    }

    fn visit_block(&mut self, node: &mut Node) {
        if let NodeKind::Block(stmts) = &mut node.kind {
            for stmt in stmts.iter_mut() {
                self.visit(stmt);
            }
        }
    }

    fn visit_continue(&mut self, _node: &mut Node) {}

    fn visit_break(&mut self, node: &mut Node) {
        if let NodeKind::Break(Some(value)) = &mut node.kind {
            self.visit(value);
        }
    }

    fn visit_echo(&mut self, node: &mut Node) {
        if let NodeKind::Echo(expr) = &mut node.kind {
            self.visit(expr);
        }
    }

    fn visit_assert(&mut self, node: &mut Node) {
        if let NodeKind::Assert(expr, _) = &mut node.kind {
            self.visit(expr);
        }
    }

    fn visit_if(&mut self, node: &mut Node) {
        if let NodeKind::If(condition, then_block, else_block) = &mut node.kind {
            self.visit(condition);
            self.visit(then_block);
            if let Some(else_b) = else_block {
                self.visit(else_b);
            }
        }
    }

    fn visit_loop(&mut self, node: &mut Node) {
        if let NodeKind::Loop(condition, body) = &mut node.kind {
            if let Some(cond) = condition {
                self.visit(cond);
            }
            self.visit(body);
        }
    }

    fn visit_unary(&mut self, node: &mut Node) {
        if let NodeKind::UnaryOperation(_, operand) = &mut node.kind {
            self.visit(operand);
        }
    }

    fn visit_binary(&mut self, node: &mut Node) {
        if let NodeKind::BinaryOperation(_, lhs, rhs) = &mut node.kind {
            self.visit(lhs);
            self.visit(rhs);
        }
    }

    fn visit_compound(&mut self, node: &mut Node) {
        if let NodeKind::CompoundComparison(_, operands) = &mut node.kind {
            for operand in operands.iter_mut() {
                self.visit(operand);
            }
        }
    }

    fn visit_local_decl(&mut self, node: &mut Node) {
        if let NodeKind::LocalDeclaration(_, _, expr, _) = &mut node.kind {
            self.visit(expr);
        }
    }

    fn visit_const_decl(&mut self, node: &mut Node) {
        if let NodeKind::ConstDeclaration(_, _, expr) = &mut node.kind {
            self.visit(expr);
        }
    }

    fn visit_identifier(&mut self, _node: &mut Node) {}
    fn visit_string(&mut self, _node: &mut Node) {}
    fn visit_float(&mut self, _node: &mut Node) {}
    fn visit_integer(&mut self, _node: &mut Node) {}
    fn visit_boolean(&mut self, _node: &mut Node) {}

    fn run(&mut self, node: &mut Node);
}

pub fn run_passes(node: &mut Node, reporter: ReportSender) {
    let mut passes: Vec<Box<dyn Pass>> = vec![
        Box::new(VarResolvePass::new(reporter.clone())),
        Box::new(TypeCheckPass::new(reporter.clone())),
        Box::new(ExprCheckPass::new(reporter.clone())),
    ];

    if !ARGS.no_optimize {
        passes.extend([
            Box::new(ConstFoldPass::new(reporter.clone())) as Box<dyn Pass>,
            Box::new(DeadCodePass::new(reporter)),
        ]);
    }

    for pass in passes.iter_mut() {
        pass.run(node);
    }
}
