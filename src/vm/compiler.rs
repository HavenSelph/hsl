use crate::ast::{Node, NodeKind, Operator};
use crate::report::{Maybe, ReportKind, ReportSender};
use crate::vm::Value;
use crate::vm::bytecode::{Chunk, OpCode};
use name_variant::NamedVariant;
use std::fmt::Display;

#[derive(NamedVariant)]
enum CompileReport {
    VariableNotFound(String),
    VariableAlreadyDeclared(String),
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
        write!(f, "{}", self.variant_name())?;
        match self {
            CompileReport::VariableNotFound(name)
            | CompileReport::VariableAlreadyDeclared(name) => write!(f, ": {}", name),
            CompileReport::InvalidAssignmentTarget => write!(f, "Invalid assignment target"),
        }
    }
}

#[derive(Copy, Clone)]
enum VariableKind {
    Local,
    Global,
}

struct Variable {
    kind: VariableKind,
    name: String,
    depth: usize,
    index: usize,
}

struct VariableStorage {
    variables: Vec<Variable>,
    depth: usize,
}

impl VariableStorage {
    fn new() -> Self {
        Self {
            variables: Vec::new(),
            depth: 0,
        }
    }

    fn locals_count(&self) -> usize {
        self.variables
            .iter()
            .filter(|var| matches![var.kind, VariableKind::Local])
            .count()
    }

    fn push_scope(&mut self) {
        self.depth += 1;
    }

    // Returns the number of variables removed from the storage.
    fn pop_scope(&mut self) -> usize {
        self.depth -= 1;
        let v = self.variables.len();
        self.variables.retain(|var| var.depth <= self.depth);
        v - self.variables.len()
    }

    fn declare(&mut self, name: String, kind: VariableKind) -> Maybe<()> {
        for var in self.variables.iter().rev() {
            if var.depth < self.depth {
                break;
            }
            if var.name == name {
                return Err(CompileReport::VariableAlreadyDeclared(name).make().into());
            }
        }
        self.variables.push(Variable {
            kind,
            name,
            depth: self.depth,
            index: self.locals_count(),
        });
        Ok(())
    }

    fn get(&mut self, name: String) -> Maybe<&Variable> {
        for var in self.variables.iter().rev() {
            if var.name == name {
                return Ok(&var);
            }
        }
        Err(CompileReport::VariableNotFound(name).make().into())
    }
}

pub struct Compiler {
    reporter: ReportSender,
    variables: VariableStorage,
    pub chunk: Chunk,
}

impl Compiler {
    pub fn new(reporter: ReportSender) -> Self {
        Self {
            reporter,
            variables: VariableStorage::new(),
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
        if crate::ARGS.debug() {
            self.chunk.disassemble();
        }
    }

    pub fn compile_statement(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Echo(expr) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Echo);
            }
            NodeKind::GlobalDeclaration(name, expr) => {
                match self.variables.declare(name.clone(), VariableKind::Global) {
                    Err(err) => self.reporter.report(err.finish().into()),
                    Ok(()) => (),
                }
                if let Some(val) = expr {
                    self.compile_expression(val);
                    self.chunk.write_op(OpCode::SetGlobal);
                    self.chunk.write_string(name);
                    self.chunk.write_op(OpCode::Pop);
                }
            }
            NodeKind::LocalDeclaration(name, expr) => {
                match self.variables.declare(name.clone(), VariableKind::Local) {
                    Err(err) => self.reporter.report(err.finish().into()),
                    Ok(()) => (),
                }
                self.compile_expression(expr);
                self.chunk.write_op_with_u16(
                    OpCode::SetLocal,
                    (self.variables.locals_count() - 1) as u16,
                );
                self.chunk.write_op(OpCode::Pop);
            }
            NodeKind::Assert(expr, message) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Not);
                self.chunk.write_op(OpCode::JumpIfFalse);
                let start = self.chunk.source.len() - 1;
                self.chunk.write_noops(2);
                self.chunk.write_op(OpCode::ReadString);
                self.chunk.write_string(message);
                self.chunk.write_op(OpCode::ErrEcho);
                self.chunk.write_op(OpCode::Stop);
                self.chunk.write_u8(1);
                let end = self.chunk.source.len() - 1;
                let jump_offset = (end - start - 2) as u16;
                self.chunk.source[start + 1] = (jump_offset >> 8) as u8;
                self.chunk.source[start + 2] = (jump_offset) as u8;
            }
            _ => {
                self.compile_expression(node);
                if node.expr_stmt {
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
                self.variables.push_scope();
                self.chunk.write_op(OpCode::PushScope);
                for stmt in stmts {
                    self.compile_statement(stmt);
                }
                self.variables.pop_scope();
                self.chunk.write_op(OpCode::PopScope);
            }
            NodeKind::If(condition, then_block, else_block) => {
                self.compile_expression(condition);

                self.chunk.write_op(OpCode::JumpIfFalse);
                let jump1 = self.chunk.source.len() - 1;
                self.chunk.write_noops(2);

                self.compile_statement(then_block);
                let mut end1 = self.chunk.source.len() - 1;

                if let Some(else_block) = else_block {
                    self.chunk.write_op(OpCode::Jump);
                    let jump2 = self.chunk.source.len() - 1;
                    self.chunk.write_noops(2);
                    end1 = self.chunk.source.len() - 1;

                    self.compile_statement(else_block);

                    let end2 = self.chunk.source.len() - 1;
                    let jump_offset = (end2 - jump2 - 2) as u16;
                    self.chunk.source[jump2 + 1] = (jump_offset >> 8) as u8;
                    self.chunk.source[jump2 + 2] = (jump_offset) as u8;
                }
                let jump_offset = (end1 - jump1 - 2) as u16;
                self.chunk.source[jump1 + 1] = (jump_offset >> 8) as u8;
                self.chunk.source[jump1 + 2] = (jump_offset) as u8;
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
                    NodeKind::Identifier(name) => {
                        match self.variables.get(name.clone()) {
                            Ok(Variable {
                                kind: VariableKind::Local,
                                index,
                                ..
                            }) => {
                                self.chunk
                                    .write_op_with_u16(OpCode::SetLocal, *index as u16);
                            }
                            Ok(Variable {
                                kind: VariableKind::Global,
                                ..
                            }) => {
                                self.chunk.write_op(OpCode::SetGlobal);
                                self.chunk.write_string(name);
                            }
                            Err(err) => {
                                self.reporter.report(err.finish().into());
                            }
                        };
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
                // Todo: handle compound expressions, i.e. 1<x<=3
                self.compile_expression(lhs);
                self.compile_expression(rhs);
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
                    _ => unimplemented_op!(op),
                });
            }
            NodeKind::Identifier(key) => {
                match self.variables.get(key.to_string()) {
                    Ok(Variable {
                        kind: VariableKind::Local,
                        index,
                        ..
                    }) => {
                        self.chunk
                            .write_op_with_u16(OpCode::LoadLocal, *index as u16);
                    }
                    Ok(Variable {
                        kind: VariableKind::Global,
                        ..
                    }) => {
                        self.chunk.write_op(OpCode::LoadGlobal);
                        self.chunk.write_string(key);
                    }
                    Err(err) => {
                        self.reporter.report(err.finish().into());
                    }
                };
            }
            NodeKind::StringLiteral(val) => self.chunk.write_const_long(Value::String(val.clone())),
            NodeKind::FloatLiteral(val) => self.chunk.write_const_long(Value::Float(*val)),
            NodeKind::IntegerLiteral(val) => self.handle_integer_const(*val as isize),
            NodeKind::BooleanLiteral(val) => {
                self.chunk
                    .write_op(if *val { OpCode::True } else { OpCode::False })
            }
            _ => unimplemented!("{:?}", node.kind),
        }
    }

    pub fn handle_integer_const(&mut self, val: isize) {
        // todo: Need to add a pass to the compiler to allow negative numbers to be inlined.
        match val {
            -128..=127 => self.chunk.write_const_short(val as i8),
            _ => self.chunk.write_const_long(Value::Integer(val)),
        }
    }
}
