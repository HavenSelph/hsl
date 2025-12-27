use crate::ast::{Node, NodeKind, Operator};
use crate::vm::Value;
use crate::vm::bytecode::{Chunk, OpCode};

pub struct Compiler {
    pub chunk: Chunk,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Node) {
        let NodeKind::Block(stmts) = &program.kind else {
            unreachable!()
        };
        for stmt in stmts {
            self.compile(stmt);
        }
        if crate::ARGS.debug() {
            self.chunk.disassemble();
        }
    }

    pub fn handle_binary_op(&mut self, op: &Operator) {
        self.chunk.write_op(match op {
            Operator::Plus => OpCode::Add,
            Operator::Minus => OpCode::Sub,
            Operator::Star => OpCode::Mul,
            Operator::Slash => OpCode::Div,
            Operator::Or => OpCode::Or,
            Operator::And => OpCode::And,
            Operator::GreaterThan => OpCode::Greater,
            Operator::LessThan => OpCode::Less,
            Operator::GreaterThanEquals => OpCode::Less, // Swap direction and invert result
            Operator::LessThanEquals => OpCode::Greater,
            Operator::Equals => OpCode::Equal,
            Operator::BangEquals => OpCode::Equal,
            _ => unreachable!(),
        });
        match op {
            Operator::GreaterThanEquals | Operator::LessThanEquals | Operator::BangEquals => {
                self.chunk.write_op(OpCode::Not)
            }
            _ => (),
        }
    }

    pub fn compile(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Return(val) => {
                self.compile(val);
                self.chunk.write_op(OpCode::Return);
            }
            NodeKind::Block(_) => unimplemented!("awaiting scopes"),
            NodeKind::VarDeclaration(_, _) => unimplemented!("awaiting scopes"),
            NodeKind::UnaryOperation(op, val) => {
                self.compile(val);
                self.chunk.write_op(match op {
                    Operator::Not => OpCode::Not,
                    _ => unreachable!(),
                })
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                // The parser will always parse compound expressions such that:
                // lhs will be either a value or another compound node
                // rhs will never be another compound node
                // Knowing this, we can handle the case where we are compiling
                // a compound expression specifically.
                self.compile(lhs);
                match &lhs.kind {
                    NodeKind::BinaryOperation(l_op, _, l_rhs)
                        if op.is_compound() && l_op.is_compound() =>
                    {
                        // We know we are compiling a compound expression
                        // i.e. 1 < 2 < 3
                        // Since we have already compiled the lhs, we now
                        // take the lhs node's rhs, and use it to compile
                        // our operator.
                        self.compile(&l_rhs);
                        self.compile(rhs);
                        self.handle_binary_op(op);
                        // Now by writing and, we effectively turn the above example
                        // into (1 < 2) and (2 < 3)
                        self.chunk.write_op(OpCode::And);
                    }
                    _ => {
                        self.compile(rhs);
                        self.handle_binary_op(op);
                    }
                }
            }
            NodeKind::Identifier(_) => unimplemented!("awaiting var declaration"),
            NodeKind::StringLiteral(val) => self.chunk.write_const(Value::String(val.clone())),
            NodeKind::FloatLiteral(val) => self.chunk.write_const(Value::Float(*val)),
            NodeKind::IntegerLiteral(val) => self.chunk.write_const(Value::Integer(*val as isize)),
            NodeKind::BooleanLiteral(val) => self.chunk.write_const(Value::Boolean(*val)),
        }
    }
}
