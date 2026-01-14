pub use crate::compiler::bytecode::{Chunk, OpCode};
use crate::report::{Maybe, ReportKind, ReportLevel};
pub use crate::vm::value::Value;

pub mod value;

enum VMError {
    GlobalNotFound(String),
}

impl ReportKind for VMError {
    fn title(&self) -> String {
        match self {
            VMError::GlobalNotFound(name) => format!("Global '{}' not found.", name),
        }
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

pub struct Scope {
    depth: usize,
    variables: Vec<(usize, Value)>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            depth: 0,
            variables: Vec::new(),
        }
    }

    pub fn push(&mut self) {
        self.depth += 1;
    }

    pub fn pop(&mut self) {
        self.depth -= 1;
        self.variables.retain(|var| var.0 <= self.depth);
    }

    pub fn set(&mut self, idx: usize, value: Value) {
        if idx == self.variables.len() {
            self.variables.push((self.depth, value));
        } else {
            let v = self.variables.get_mut(idx).expect("Compiler error.");
            v.1 = value;
        }
    }
}

pub struct VM {
    ip: usize,
    stack: Vec<Value>,
    globals: Vec<(String, Value)>,
    scope: Scope,
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::new(),
            globals: Vec::new(),
            scope: Scope::new(),
        }
    }

    pub fn set_global(&mut self, key: &str, value: Value) {
        match self.globals.iter_mut().find(|v| v.0 == key) {
            Some((_, val)) => *val = value,
            None => self.globals.push((key.to_string(), value)),
        }
    }

    pub fn get_global(&self, key: &str) -> Maybe<Value> {
        match self.globals.iter().find(|v| v.0 == key) {
            Some((_, val)) => Ok(val.clone()),
            None => Err(VMError::GlobalNotFound(key.to_string()).make().into()),
        }
    }

    pub fn run(&mut self, chunk: &mut Chunk) -> Maybe<Value> {
        self.ip = 0;
        while self.ip < chunk.source.len() {
            if crate::ARGS.trace_execution {
                chunk.disassemble_op(&mut self.ip.clone())
            }
            let op = chunk.read_op(&mut self.ip);
            match op {
                OpCode::Nop => (),
                OpCode::Stop => {
                    let code = chunk.read_u8(&mut self.ip);
                    std::process::exit(code as i32);
                }
                _ => self.run_op(chunk, op)?,
            };
        }
        assert_eq!(self.ip, chunk.source.len());
        Ok(self.stack.pop().unwrap_or(Value::Nada))
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
            OpCode::Nop | OpCode::Stop => unreachable!(),
            OpCode::Echo => {
                println!("{}", self.stack.pop().unwrap());
            }
            OpCode::ErrEcho => {
                eprintln!("{}", self.stack.pop().unwrap());
            }
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
                if !val.as_bool()? {
                    self.ip += offset as usize;
                }
            }
            OpCode::Loop => {
                let offset = chunk.read_u16(&mut self.ip);
                self.ip -= (offset + 3) as usize;
            }
            OpCode::ReadConst => {
                let val = chunk.read_u8(&mut self.ip) as isize;
                self.stack.push(Value::Integer(val));
            }
            OpCode::ReadString => {
                let val = chunk.read_string(&mut self.ip);
                self.stack.push(Value::String(val));
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

            OpCode::SetLocal => {
                let idx = chunk.read_u16(&mut self.ip) as usize;
                self.scope.set(idx, self.stack.last().unwrap().clone());
            }
            OpCode::LoadLocal => {
                let idx = chunk.read_u16(&mut self.ip) as usize;
                self.stack.push(self.scope.variables[idx].1.clone());
            }
            OpCode::LoadGlobal => {
                let key = chunk.read_string(&mut self.ip);
                self.stack.push(self.get_global(&key)?);
            }
            OpCode::SetGlobal => {
                let key = chunk.read_string(&mut self.ip);
                let val = self.stack.last().unwrap().clone();
                self.set_global(&key, val);
            }
            OpCode::PushScope => self.scope.push(),
            OpCode::PopScope => self.scope.pop(),

            OpCode::Add => binary!(Value::add),
            OpCode::Sub => binary!(Value::sub),
            OpCode::Mul => binary!(Value::mul),
            OpCode::Div => binary!(Value::div),
            OpCode::Mod => binary!(Value::modulo),
            OpCode::Pow => binary!(Value::pow),
            OpCode::Neg => unary!(Value::negate),

            OpCode::BitAnd => binary!(Value::bit_and),
            OpCode::BitOr => binary!(Value::bit_or),
            OpCode::BitXor => binary!(Value::bit_xor),
            OpCode::ShiftLeft => binary!(Value::shift_left),
            OpCode::ShiftRight => binary!(Value::shift_right),

            OpCode::Equal => binary!(Value::equal),
            OpCode::NotEqual => binary!(Value::not_equal),
            OpCode::Greater => binary!(Value::greater),
            OpCode::GreaterEqual => binary!(Value::greater_equal),
            OpCode::Less => binary!(Value::less),
            OpCode::LessEqual => binary!(Value::less_equal),

            OpCode::Not => unary!(Value::not),
            OpCode::Or => binary!(Value::or),
            OpCode::And => binary!(Value::and),
        };
        Ok(())
    }
}
