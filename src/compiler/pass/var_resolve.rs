use crate::ast::{Node, NodeKind, ResolvedVar, VariableKind};
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
    Global,
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
    global_count: usize,
}

impl VarResolvePass {
    pub fn new(reporter: ReportSender) -> Self {
        Self {
            reporter,
            scopes: vec![Scope::new()],
            depth: 0,
            global_count: 0,
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
            InternalVarKind::Global => {
                let idx = self.global_count as u16;
                self.global_count += 1;
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

    fn resolve_node(&mut self, node: &mut Node) {
        match &mut node.kind {
            NodeKind::Block(stmts) => {
                self.push_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve_node(stmt);
                }
                self.pop_scope();
            }
            NodeKind::ConstDeclaration(name, _ty, expr) => {
                self.resolve_node(expr);

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
            NodeKind::LocalDeclaration(name, _ty, expr, resolved_idx) => {
                self.resolve_node(expr);
                let idx = self.locals_count() as u16;
                self.declare_variable(name.clone(), InternalVarKind::Local, None, node.span);
                *resolved_idx = Some(idx);
            }
            NodeKind::GlobalDeclaration(name, _ty, expr, resolved_idx) => {
                if let Some(e) = expr {
                    self.resolve_node(e);
                }
                let idx = self.global_count as u16;
                self.declare_variable(name.clone(), InternalVarKind::Global, None, node.span);
                *resolved_idx = Some(idx);
            }
            NodeKind::Identifier(name, resolved) => {
                if let Some(var) = self.lookup_variable(name) {
                    if var.kind == InternalVarKind::Const {
                        if let Some(const_val) = var.const_value {
                            node.kind = const_val.kind.clone();
                            node.ty = const_val.ty;
                        }
                    } else {
                        // Set resolved variable info for non-const variables
                        let ast_kind = match var.kind {
                            InternalVarKind::Local => VariableKind::Local,
                            InternalVarKind::Global => VariableKind::Global,
                            InternalVarKind::Const => unreachable!(),
                        };
                        *resolved = Some(ResolvedVar {
                            kind: ast_kind,
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
            NodeKind::BinaryOperation(crate::ast::Operator::Assign, lhs, rhs) => {
                self.resolve_node(rhs);

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
                            // Set resolved variable info for assignment target
                            let ast_kind = match var.kind {
                                InternalVarKind::Local => VariableKind::Local,
                                InternalVarKind::Global => VariableKind::Global,
                                InternalVarKind::Const => unreachable!(),
                            };
                            *resolved = Some(ResolvedVar {
                                kind: ast_kind,
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
            NodeKind::BinaryOperation(_, lhs, rhs) => {
                self.resolve_node(lhs);
                self.resolve_node(rhs);
            }
            NodeKind::UnaryOperation(_, operand) => {
                self.resolve_node(operand);
            }
            NodeKind::CompoundComparison(_, operands) => {
                for operand in operands.iter_mut() {
                    self.resolve_node(operand);
                }
            }
            NodeKind::If(condition, then_block, else_block) => {
                self.resolve_node(condition);
                self.resolve_node(then_block);
                if let Some(else_b) = else_block {
                    self.resolve_node(else_b);
                }
            }
            NodeKind::Loop(condition, body) => {
                if let Some(cond) = condition {
                    self.resolve_node(cond);
                }
                self.resolve_node(body);
            }
            NodeKind::Echo(expr) => {
                self.resolve_node(expr);
            }
            NodeKind::Assert(expr, _) => {
                self.resolve_node(expr);
            }
            NodeKind::Break(value) => {
                if let Some(val) = value {
                    self.resolve_node(val);
                }
            }
            _ => {}
        }
    }
}

impl Pass for VarResolvePass {
    fn run(&mut self, node: &mut Node) {
        self.resolve_node(node);
    }
}
