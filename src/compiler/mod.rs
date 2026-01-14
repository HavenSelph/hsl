use crate::ast::{Node, NodeKind, Operator};
use crate::compiler::bytecode::{Chunk, OpCode};
use crate::report::{ReportKind, ReportSender};
use crate::vm::Value;
use name_variant::NamedVariant;
use std::fmt::Display;

pub mod bytecode;
pub mod pass;

#[derive(NamedVariant)]
enum CompileReport {
    VariableNotFound(String),
    InvalidAssignmentTarget,
}

impl ReportKind for CompileReport {
    fn title(&self) -> String {
        format!("{}", self.variant_name())
    }

    fn level(&self) -> crate::report::ReportLevel {
        crate::report::ReportLevel::Error
    }
}

impl Display for CompileReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileReport::VariableNotFound(name) => write!(f, "Variable not found: {}", name),
            CompileReport::InvalidAssignmentTarget => write!(f, "Invalid assignment target"),
        }
    }
}

pub struct Compiler {
    reporter: ReportSender,
    local_count: Vec<u16>, // Stack of local counts per scope

    loop_depth: usize,
    loop_starts: Vec<usize>,
    loop_breaks: Vec<Vec<usize>>, // depth, index

    pub chunk: Chunk,
}

impl Compiler {
    pub fn new(reporter: ReportSender) -> Self {
        Self {
            reporter,
            local_count: vec![0],

            loop_depth: 0,
            loop_starts: Vec::new(),
            loop_breaks: Vec::new(),

            chunk: Chunk::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Node) {
        let NodeKind::Block(stmts) = &program.kind else {
            unreachable!()
        };
        for stmt in stmts {
            self.compile_statement(stmt);
        }
        if crate::ARGS.debug {
            self.chunk.disassemble();
        }
    }

    pub fn compile_statement(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Block(stmts) => {
                self.local_count.push(0);
                self.chunk.write_op(OpCode::PushScope);
                let len = stmts.len();
                for (i, stmt) in stmts.iter().enumerate() {
                    let is_last = i == len - 1;
                    if is_last && !stmt.expr {
                        self.compile_expression(stmt);
                    } else {
                        self.compile_statement(stmt);
                    }
                }
                if len == 0 {
                    self.chunk.write_op(OpCode::Nada);
                }
                self.local_count.pop();
                self.chunk.write_op(OpCode::PopScope);
            }
            NodeKind::Echo(expr) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Echo);
            }
            NodeKind::GlobalDeclaration(_name, _ty, expr, _) => {
                if let Some(val) = expr {
                    self.compile_expression(val);
                    self.chunk.write_op(OpCode::SetGlobal);
                    self.chunk.write_string(_name);
                    self.chunk.write_op(OpCode::Pop);
                }
            }
            NodeKind::LocalDeclaration(_name, _ty, expr, resolved_idx) => {
                self.compile_expression(expr);
                let idx = resolved_idx.expect("LocalDeclaration should have resolved index");
                *self.local_count.last_mut().unwrap() += 1;
                self.chunk.write_op_with_u16(OpCode::SetLocal, idx);
                self.chunk.write_op(OpCode::Pop);
            }
            NodeKind::Continue => {
                self.chunk.write_loop(self.loop_starts[self.loop_depth - 1]);
            }
            NodeKind::Break(value) => {
                if let Some(value) = value {
                    self.compile_expression(value);
                }
                self.loop_breaks[self.loop_depth - 1].push(self.chunk.write_jump(OpCode::Jump));
            }
            NodeKind::Assert(expr, message) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Not);
                let jump = self.chunk.write_jump(OpCode::JumpIfFalse);
                self.chunk.write_op(OpCode::ReadString);
                self.chunk.write_string(message);
                self.chunk.write_op(OpCode::ErrEcho);
                self.chunk.write_op(OpCode::Stop);
                self.chunk.write_u8(1);
                self.chunk.patch_jump(jump);
            }
            NodeKind::If(condition, then_block, else_block) if !node.expr => {
                self.compile_expression(condition);
                let else_jump = self.chunk.write_jump(OpCode::JumpIfFalse);
                self.compile_statement(then_block);
                self.chunk.patch_jump(else_jump);
                if let Some(else_block) = else_block {
                    let then_jump = self.chunk.write_jump(OpCode::Jump);
                    self.chunk.patch_jump(else_jump);
                    self.compile_statement(else_block);
                    self.chunk.patch_jump(then_jump);
                }
            }
            _ => {
                self.compile_expression(node);
                if node.expr {
                    self.chunk.write_op(OpCode::Pop);
                }
            }
        }
    }

    pub fn compile_expression(&mut self, node: &Node) {
        macro_rules! unimplemented_op {
            ($op:expr) => {
                unimplemented!("Operator {:?}", $op)
            };
        }

        match &node.kind {
            NodeKind::Block(stmts) => {
                self.local_count.push(0);
                self.chunk.write_op(OpCode::PushScope);
                let len = stmts.len();
                for (i, stmt) in stmts.iter().enumerate() {
                    let is_last = i == len - 1;
                    if is_last && !stmt.expr {
                        self.compile_expression(stmt);
                    } else {
                        self.compile_statement(stmt);
                    }
                }
                if len == 0 {
                    self.chunk.write_op(OpCode::Nada);
                }
                self.local_count.pop();
                self.chunk.write_op(OpCode::PopScope);
            }
            NodeKind::If(condition, then_block, else_block) => {
                self.compile_expression(condition);
                let else_jump = self.chunk.write_jump(OpCode::JumpIfFalse);
                self.compile_expression(then_block);
                self.chunk.patch_jump(else_jump);
                if let Some(else_block) = else_block {
                    let then_jump = self.chunk.write_jump(OpCode::Jump);
                    self.chunk.patch_jump(else_jump);
                    self.compile_expression(else_block);
                    self.chunk.patch_jump(then_jump);
                }
            }
            NodeKind::Loop(condition, body) => {
                let start = self.chunk.source.len();

                self.loop_depth += 1;
                self.loop_starts.push(start);
                self.loop_breaks.push(Vec::new());

                let condition_jump = if let Some(condition) = condition {
                    self.compile_expression(condition);
                    Some(self.chunk.write_jump(OpCode::JumpIfFalse))
                } else {
                    None
                };

                self.compile_statement(body);
                self.chunk.write_loop(start);
                if let Some(condition_jump) = condition_jump {
                    self.chunk.patch_jump(condition_jump);
                }

                assert_eq!(self.loop_starts.pop().unwrap(), start);
                for index in self.loop_breaks.pop().unwrap() {
                    self.chunk.patch_jump(index);
                }
                self.loop_depth -= 1;
            }
            NodeKind::UnaryOperation(Operator::UnaryPlus, val) => self.compile_expression(val),
            NodeKind::UnaryOperation(op, val) => {
                self.compile_expression(val);
                self.chunk.write_op(match op {
                    Operator::UnaryMinus => OpCode::Neg,
                    Operator::Not => OpCode::Not,
                    _ => unimplemented_op!(op),
                })
            }
            NodeKind::BinaryOperation(Operator::Assign, lhs, rhs) => {
                self.compile_expression(rhs);
                match &lhs.kind {
                    NodeKind::Identifier(name, Some(resolved)) => match resolved.kind {
                        crate::ast::VariableKind::Local => {
                            self.chunk
                                .write_op_with_u16(OpCode::SetLocal, resolved.index);
                        }
                        crate::ast::VariableKind::Global => {
                            self.chunk.write_op(OpCode::SetGlobal);
                            self.chunk.write_string(name);
                        }
                    },
                    NodeKind::Identifier(name, None) => {
                        // Variable not resolved - error already reported by var_resolve pass
                        self.reporter.report(
                            CompileReport::VariableNotFound(name.clone())
                                .make()
                                .finish()
                                .into(),
                        );
                    }
                    _ => self.reporter.report(
                        CompileReport::InvalidAssignmentTarget
                            .make()
                            .finish()
                            .into(),
                    ),
                }
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                self.compile_expression(lhs);
                self.compile_expression(rhs);
                self.handle_binary_op(op);
            }
            NodeKind::CompoundComparison(ops, operands) => {
                // Compile compound comparisons like: a < b < c < d
                // Evaluate each operand exactly once by storing them into temporary
                // local variables in a new scope, then load them as needed for each
                // comparison.
                self.local_count.push(0);
                self.chunk.write_op(OpCode::PushScope);

                // Get base index for temporaries (sum of all locals in outer scopes)
                let base_idx: u16 = self
                    .local_count
                    .iter()
                    .take(self.local_count.len() - 1)
                    .sum();

                let mut temp_indices = Vec::new();
                for (i, operand) in operands.iter().enumerate() {
                    self.compile_expression(operand);
                    let idx = base_idx + i as u16;
                    self.chunk.write_op_with_u16(OpCode::SetLocal, idx);
                    self.chunk.write_op(OpCode::Pop);
                    temp_indices.push(idx);
                    *self.local_count.last_mut().unwrap() += 1;
                }

                // Perform comparisons using the temporaries. Each comparison loads two
                // operands and pushes the boolean result. We then AND successive results.
                self.chunk
                    .write_op_with_u16(OpCode::LoadLocal, temp_indices[0]);
                self.chunk
                    .write_op_with_u16(OpCode::LoadLocal, temp_indices[1]);
                self.handle_binary_op(&ops[0]);

                for i in 1..ops.len() {
                    self.chunk
                        .write_op_with_u16(OpCode::LoadLocal, temp_indices[i]);
                    self.chunk
                        .write_op_with_u16(OpCode::LoadLocal, temp_indices[i + 1]);
                    self.handle_binary_op(&ops[i]);
                    self.chunk.write_op(OpCode::And);
                }

                self.local_count.pop();
                self.chunk.write_op(OpCode::PopScope);
            }
            NodeKind::Identifier(name, Some(resolved)) => match resolved.kind {
                crate::ast::VariableKind::Local => {
                    self.chunk
                        .write_op_with_u16(OpCode::LoadLocal, resolved.index);
                }
                crate::ast::VariableKind::Global => {
                    self.chunk.write_op(OpCode::LoadGlobal);
                    self.chunk.write_string(name);
                }
            },
            NodeKind::Identifier(name, None) => {
                // Variable not resolved - error already reported by var_resolve pass
                self.reporter.report(
                    CompileReport::VariableNotFound(name.clone())
                        .make()
                        .finish()
                        .into(),
                );
            }
            NodeKind::StringLiteral(val) => self.chunk.write_const_long(Value::String(val.clone())),
            NodeKind::FloatLiteral(val) => self.chunk.write_const_long(Value::Float(*val)),
            NodeKind::IntegerLiteral(val) => self.handle_integer_const(*val as isize),
            NodeKind::BooleanLiteral(val) => {
                self.chunk
                    .write_op(if *val { OpCode::True } else { OpCode::False })
            }
            NodeKind::Assert(expr, message) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Not);
                let jump = self.chunk.write_jump(OpCode::JumpIfFalse);
                self.chunk.write_op(OpCode::ReadString);
                self.chunk.write_string(message);
                self.chunk.write_op(OpCode::ErrEcho);
                self.chunk.write_op(OpCode::Stop);
                self.chunk.write_u8(1);
                self.chunk.patch_jump(jump);
                self.chunk.write_op(OpCode::Nada);
            }
            NodeKind::Echo(expr) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Echo);
                self.chunk.write_op(OpCode::Nada);
            }
            NodeKind::Continue => {
                self.chunk.write_loop(self.loop_starts[self.loop_depth - 1]);
                self.chunk.write_op(OpCode::Nada);
            }
            NodeKind::Break(value) => {
                if let Some(value) = value {
                    self.compile_expression(value);
                } else {
                    self.chunk.write_op(OpCode::Nada);
                }
                self.loop_breaks[self.loop_depth - 1].push(self.chunk.write_jump(OpCode::Jump));
            }
            NodeKind::LocalDeclaration(_name, _ty, expr, resolved_idx) => {
                self.compile_expression(expr);
                let idx = resolved_idx.expect("LocalDeclaration should have resolved index");
                *self.local_count.last_mut().unwrap() += 1;
                self.chunk.write_op_with_u16(OpCode::SetLocal, idx);
                self.chunk.write_op(OpCode::Nada);
            }
            NodeKind::GlobalDeclaration(_name, _ty, expr, _) => {
                if let Some(val) = expr {
                    self.compile_expression(val);
                    self.chunk.write_op(OpCode::SetGlobal);
                    self.chunk.write_string(_name);
                    self.chunk.write_op(OpCode::Pop);
                }
                self.chunk.write_op(OpCode::Nada);
            }
            NodeKind::ConstDeclaration(_, _, _) => (),
        }
    }

    pub fn handle_binary_op(&mut self, op: &Operator) {
        self.chunk.write_op(match op {
            Operator::Or => OpCode::Or,
            Operator::And => OpCode::And,
            Operator::Equal => OpCode::Equal,
            Operator::NotEqual => OpCode::NotEqual,
            Operator::Greater => OpCode::Greater,
            Operator::GreaterEqual => OpCode::GreaterEqual,
            Operator::Less => OpCode::Less,
            Operator::LessEqual => OpCode::LessEqual,
            Operator::BitwiseOr => OpCode::BitOr,
            Operator::BitwiseXor => OpCode::BitXor,
            Operator::BitwiseAnd => OpCode::BitAnd,
            Operator::ShiftLeft => OpCode::ShiftLeft,
            Operator::ShiftRight => OpCode::ShiftRight,
            Operator::Add => OpCode::Add,
            Operator::Subtract => OpCode::Sub,
            Operator::Multiply => OpCode::Mul,
            Operator::Divide => OpCode::Div,
            Operator::Modulo => OpCode::Mod,
            Operator::Power => OpCode::Pow,
            _ => unimplemented!("Operator {:?}", op),
        })
    }

    pub fn handle_integer_const(&mut self, val: isize) {
        // todo: Need to add a pass to the compiler to allow negative numbers to be inlined.
        match val {
            -128..=127 => self.chunk.write_const_short(val as i8),
            _ => self.chunk.write_const_long(Value::Integer(val)),
        }
    }
}
