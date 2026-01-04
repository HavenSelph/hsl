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
            if crate::ARGS.trace_execution() {
                chunk.disassemble_op(&mut self.ip.clone())
            }
            let op = chunk.read_op(&mut self.ip);
            match op {
                OpCode::Stop => {
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
            OpCode::Pop => {
                self.stack.pop().unwrap();
            }
            OpCode::Dup => {
                self.stack
                    .push(self.stack.get(self.stack.len() - 1).unwrap().clone());
            }
            OpCode::Swap => {
                let idx = self.stack.len() - 1;
                self.stack.swap(idx, idx - 1);
            }

            OpCode::Jump => {
                let offset = chunk.read_u16(&mut self.ip);
                self.ip += offset as usize;
            }
            OpCode::JumpIfFalse => {
                let offset = chunk.read_u16(&mut self.ip);
                let val = self.stack.pop().unwrap();
                if val.not()?.bool()? {
                    self.ip += offset as usize;
                }
            }
            OpCode::Loop => {
                let offset = chunk.read_u16(&mut self.ip);
                self.ip -= offset as usize;
            }

            OpCode::ReadConst => {
                let val = chunk.read_u8(&mut self.ip) as isize;
                self.stack.push(Value::Integer(val));
            }
            OpCode::LoadConst8 | OpCode::LoadConst16 => {
                let idx = if matches![op, OpCode::LoadConst8] {
                    chunk.read_u8(&mut self.ip) as usize
                } else {
                    chunk.read_u16(&mut self.ip) as usize
                };
                self.stack.push(chunk.get_const(idx));
            }
            OpCode::Nada => self.stack.push(Value::Nada),
            OpCode::True | OpCode::False => {
                self.stack.push(Value::Boolean(matches![op, OpCode::True]))
            }

            OpCode::Add => binary!(Value::add),
            OpCode::Sub => binary!(Value::sub),
            OpCode::Mul => binary!(Value::mul),
            OpCode::Div => binary!(Value::div),
            OpCode::Mod => binary!(Value::modulo),
            OpCode::Neg => unary!(Value::negate),
            _ => unimplemented!(),
        };
        Ok(())
    }
}
