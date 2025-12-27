mod bytecode;
mod compiler;
mod value;

use crate::report::{Maybe, ReportKind, ReportLevel};
pub use crate::vm::bytecode::{Chunk, OpCode};
pub use crate::vm::compiler::Compiler;
pub use crate::vm::value::Value;

struct VMError(String);

impl ReportKind for VMError {
    fn title(&self) -> String {
        format!("VM Error: {}", self.0)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn run(&mut self, chunk: &mut Chunk) -> Maybe<Value> {
        self.ip = 0;
        while self.ip < chunk.source.len() {
            let op = chunk.read_op(&mut self.ip);
            if crate::ARGS.trace_execution() {
                chunk.disassemble_op(op, &mut self.ip.clone())
            }
            match op {
                OpCode::Return => {
                    return Ok(self.stack.pop().unwrap_or(Value::Nada));
                }
                _ => self.run_op(chunk, op)?,
            };
        }
        Ok(Value::Nada)
    }

    pub fn run_op(&mut self, chunk: &mut Chunk, op: OpCode) -> Maybe<()> {
        macro_rules! unary {
            ($op:path) => {{
                let val = self.stack.pop().unwrap();
                self.stack.push($op(&val)?)
            }};
        }

        macro_rules! binary {
            ($op:path) => {{
                let rhs = self.stack.pop().unwrap();
                let lhs = self.stack.pop().unwrap();
                self.stack.push($op(&lhs, &rhs)?);
            }};
        }

        match op {
            OpCode::Const => {
                let val = chunk.read_const(&mut self.ip);
                self.stack.push(val);
            }
            OpCode::Add => binary!(Value::add),
            OpCode::Sub => binary!(Value::sub),
            OpCode::Mul => binary!(Value::mul),
            OpCode::Div => binary!(Value::div),
            OpCode::Less => binary!(Value::lt),
            OpCode::Greater => binary!(Value::gt),
            OpCode::Equal => binary!(Value::equals),
            OpCode::And => binary!(Value::and),
            OpCode::Or => binary!(Value::or),
            OpCode::Not => unary!(Value::not),
            OpCode::Return => unimplemented!(),
        }
        Ok(())
    }
}
