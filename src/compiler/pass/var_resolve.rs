use crate::ast::{Node, NodeKind, Operator, ResolvedVar, VariableKind};
use crate::compiler::pass::Pass;
use crate::report::{ReportKind, ReportLevel, ReportSender, SpanToLabel};
use std::collections::HashMap;
use std::fmt::Display;

enum VarResolveError {
    VariableNotFound(String),
    VariableAlreadyDeclared(String),
    ConstNotCompileTime(String),
    CannotAssignToConst(String),
}

impl Display for VarResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarResolveError::VariableNotFound(name) => {
                write!(f, "Variable '{}' not found", name)
            }
            VarResolveError::VariableAlreadyDeclared(name) => {
                write!(f, "Variable '{}' already declared in this scope", name)
            }
            VarResolveError::ConstNotCompileTime(name) => {
                write!(
                    f,
                    "Constant '{}' must be initialized with a compile-time constant expression",
                    name
                )
            }
            VarResolveError::CannotAssignToConst(name) => {
                write!(f, "Cannot assign to constant '{}'", name)
            }
        }
    }
}

impl ReportKind for VarResolveError {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum InternalVarKind {
    Local,
    Const,
}

#[derive(Clone)]
struct ResolvedVariable {
    kind: InternalVarKind,
    index: u16,
    const_value: Option<Box<Node>>,
}

struct Scope {
    variables: HashMap<String, ResolvedVariable>,
    local_count: usize,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            local_count: 0,
        }
    }
}

pub struct VarResolvePass {
    reporter: ReportSender,
    scopes: Vec<Scope>,
    depth: usize,
}

impl VarResolvePass {
    pub fn new(reporter: ReportSender) -> Self {
        Self {
            reporter,
            scopes: vec![Scope::new()],
            depth: 0,
        }
    }

    fn push_scope(&mut self) {
        self.depth += 1;
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.depth -= 1;
        self.scopes.pop();
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn locals_count(&self) -> usize {
        self.scopes.iter().map(|s| s.local_count).sum()
    }

    fn declare_variable(
        &mut self,
        name: String,
        kind: InternalVarKind,
        const_value: Option<Box<Node>>,
        span: crate::ast::span::Span,
    ) {
        if self.current_scope().variables.contains_key(&name) {
            self.reporter.report(
                VarResolveError::VariableAlreadyDeclared(name.clone())
                    .make_labeled(span.label())
                    .finish()
                    .into(),
            );
            return;
        }

        let index = match kind {
            InternalVarKind::Local => {
                let idx = self.locals_count() as u16;
                self.current_scope().local_count += 1;
                idx
            }
            InternalVarKind::Const => 0,
        };

        self.current_scope().variables.insert(
            name,
            ResolvedVariable {
                kind,
                index,
                const_value,
            },
        );
    }

    fn lookup_variable(&self, name: &str) -> Option<ResolvedVariable> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.variables.get(name) {
                return Some(var.clone());
            }
        }
        None
    }

    fn is_const_expr(&self, node: &Node) -> bool {
        match &node.kind {
            NodeKind::IntegerLiteral(_)
            | NodeKind::FloatLiteral(_)
            | NodeKind::StringLiteral(_)
            | NodeKind::BooleanLiteral(_) => true,
            NodeKind::UnaryOperation(_, operand) => self.is_const_expr(operand),
            NodeKind::BinaryOperation(_, lhs, rhs) => {
                self.is_const_expr(lhs) && self.is_const_expr(rhs)
            }
            NodeKind::Identifier(name, _) => {
                if let Some(var) = self.lookup_variable(name) {
                    var.kind == InternalVarKind::Const
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn resolve_identifier(&mut self, node: &mut Node) {
        if let NodeKind::Identifier(name, resolved) = &mut node.kind {
            if let Some(var) = self.lookup_variable(name) {
                if var.kind == InternalVarKind::Const {
                    if let Some(const_val) = var.const_value {
                        node.kind = const_val.kind.clone();
                        node.ty = const_val.ty;
                    }
                } else {
                    *resolved = Some(ResolvedVar {
                        kind: VariableKind::Local,
                        index: var.index,
                    });
                }
            } else {
                self.reporter.report(
                    VarResolveError::VariableNotFound(name.clone())
                        .make_labeled(node.span.label())
                        .finish()
                        .into(),
                );
            }
        }
    }

    fn resolve_assignment(&mut self, lhs: &mut Node, rhs: &mut Node) {
        self.visit(rhs);

        if let NodeKind::Identifier(name, resolved) = &mut lhs.kind {
            if let Some(var) = self.lookup_variable(name) {
                if var.kind == InternalVarKind::Const {
                    self.reporter.report(
                        VarResolveError::CannotAssignToConst(name.clone())
                            .make_labeled(lhs.span.label())
                            .finish()
                            .into(),
                    );
                } else {
                    *resolved = Some(ResolvedVar {
                        kind: VariableKind::Local,
                        index: var.index,
                    });
                }
            } else {
                self.reporter.report(
                    VarResolveError::VariableNotFound(name.clone())
                        .make_labeled(lhs.span.label())
                        .finish()
                        .into(),
                );
            }
        }
    }
}

impl Pass for VarResolvePass {
    fn visit_block(&mut self, node: &mut Node) {
        self.push_scope();
        if let NodeKind::Block(stmts) = &mut node.kind {
            for stmt in stmts.iter_mut() {
                self.visit(stmt);
            }
        }
        self.pop_scope();
    }

    fn visit_local_decl(&mut self, node: &mut Node) {
        if let NodeKind::LocalDeclaration(name, _ty, expr, resolved_idx) = &mut node.kind {
            self.visit(expr);
            let idx = self.locals_count() as u16;
            self.declare_variable(name.clone(), InternalVarKind::Local, None, node.span);
            *resolved_idx = Some(idx);
        }
    }

    fn visit_const_decl(&mut self, node: &mut Node) {
        if let NodeKind::ConstDeclaration(name, _ty, expr) = &mut node.kind {
            self.visit(expr);

            if !self.is_const_expr(expr) {
                self.reporter.report(
                    VarResolveError::ConstNotCompileTime(name.clone())
                        .make_labeled(expr.span.label())
                        .finish()
                        .into(),
                );
            }

            self.declare_variable(
                name.clone(),
                InternalVarKind::Const,
                Some(expr.clone()),
                node.span,
            );
        }
    }

    fn visit_identifier(&mut self, node: &mut Node) {
        self.resolve_identifier(node);
    }

    fn visit_binary(&mut self, node: &mut Node) {
        if let NodeKind::BinaryOperation(Operator::Assign, lhs, rhs) = &mut node.kind {
            self.resolve_assignment(lhs, rhs);
        } else if let NodeKind::BinaryOperation(_, lhs, rhs) = &mut node.kind {
            self.visit(lhs);
            self.visit(rhs);
        }
    }

    fn run(&mut self, node: &mut Node) {
        self.visit(node);
    }
}
