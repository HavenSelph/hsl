use crate::vm::value::Value;
use int_enum::IntEnum;
use name_variant::NamedVariant;

#[repr(u8)]
#[derive(IntEnum, NamedVariant, Debug, Copy, Clone)]
pub enum OpCode {
    // 0x00-0x01 VM Control
    Stop = 0x00,

    // 0x02-0x09 Stack
    Pop = 0x02,
    Dup = 0x03,
    Swap = 0x04,

    // 0x0a-0x0f Control Flow
    Jump = 0x0a,        // Next u16 represents offset
    JumpIfFalse = 0x0b, // Next u16 represents offset
    Loop = 0x0c,        // Next u16 represents offset

    // 0x10-0x1f Constants
    ReadConst = 0x10,   // Next u8 represents an i8
    LoadConst8 = 0x11,  // Next u8 represents an index
    LoadConst16 = 0x12, // Next u16 represents an index
    Nada = 0x1a,
    True = 0x1b,
    False = 0x1c,

    // 0x20-0x2f Variables
    // LoadLocal = 0x20,
    // SetLocal = 0x21,
    // LoadGlobal = 0x22,
    // SetGlobal = 0x23,

    // 0x30-0x3f Arithmetic
    Add = 0x30,
    Sub = 0x31,
    Mul = 0x32,
    Div = 0x33,
    Mod = 0x34,
    Neg = 0x35,
    // 0x40-0x4f Bitwise
    // BitAnd = 0x40,
    // BitOr = 0x41,
    // BitXor = 0x42,
    // BitNot = 0x43,
    // ShiftLeft = 0x44,
    // ShiftRight = 0x45,

    // 0x50-0x5f Comparison & Logic
    // Equal = 0x50,
    // NotEqual = 0x51,
    // Greater = 0x52,
    // GreaterEqual = 0x53,
    // Less = 0x54,
    // LessEqual = 0x55,
    // Not = 0x56, // Logical NOT
}

pub struct Chunk {
    pub source: Vec<u8>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            source: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn get_const(&self, idx: usize) -> Value {
        self.constants[idx].clone()
    }

    pub fn write_u8(&mut self, value: u8) {
        self.source.push(value);
    }

    pub fn write_u16(&mut self, value: u16) {
        self.write_u8((value >> 8) as u8);
        self.write_u8(value as u8);
    }

    pub fn write_u32(&mut self, value: u32) {
        self.write_u16((value >> 16) as u16);
        self.write_u16(value as u16);
    }

    pub fn write_u64(&mut self, value: u64) {
        self.write_u32((value >> 32) as u32);
        self.write_u32(value as u32);
    }

    pub fn write_op(&mut self, op: OpCode) {
        self.source.push(op as u8);
    }

    pub fn write_op_with_u8(&mut self, op: OpCode, value: u8) {
        self.write_op(op);
        self.write_u8(value);
    }

    pub fn write_op_with_u16(&mut self, op: OpCode, value: u16) {
        self.write_op(op);
        self.write_u16(value);
    }

    pub fn write_op_with_u32(&mut self, op: OpCode, value: u32) {
        self.write_op(op);
        self.write_u32(value);
    }

    pub fn write_op_with_u64(&mut self, op: OpCode, value: u64) {
        self.write_op(op);
        self.write_u64(value);
    }

    pub fn write_const_short(&mut self, value: i8) {
        self.write_op_with_u8(OpCode::ReadConst, value as u8);
    }

    pub fn write_const_long(&mut self, value: Value) {
        let idx = match self.constants.iter().position(|v| *v == value) {
            Some(idx) => idx,
            None => {
                self.constants.push(value);
                self.constants.len() - 1
            }
        };
        match idx {
            0..=255 => {
                self.write_op_with_u8(OpCode::LoadConst8, idx as u8);
            }
            256..=65_535 => {
                self.write_op_with_u16(OpCode::LoadConst16, idx as u16);
            }
            _ => {
                panic!("Seriously, over 65k constants?")
            }
        }
    }

    pub fn read_u8(&self, offset: &mut usize) -> u8 {
        let res = self.source[*offset];
        *offset += 1;
        res
    }

    pub fn read_u16(&self, offset: &mut usize) -> u16 {
        (self.read_u8(offset) as u16) << 8 | self.read_u8(offset) as u16
    }

    pub fn read_u32(&self, offset: &mut usize) -> u32 {
        (self.read_u16(offset) as u32) << 16 | self.read_u16(offset) as u32
    }

    pub fn read_u64(&self, offset: &mut usize) -> u64 {
        (self.read_u32(offset) as u64) << 32 | self.read_u32(offset) as u64
    }

    pub fn read_op(&self, offset: &mut usize) -> OpCode {
        self.read_u8(offset).try_into().unwrap()
    }

    pub fn disassemble_op(&self, offset: &mut usize) {
        let idx = *offset;
        let op = self.read_op(offset);
        print!("{:03} | {:#04x} {:12}", idx, op as usize, op.variant_name());

        match op {
            OpCode::ReadConst => {
                let val = self.read_u8(offset) as i8;
                println!(" {:#04x} ({val})", val);
            }
            OpCode::LoadConst8 | OpCode::LoadConst16 => {
                let idx = match op {
                    OpCode::LoadConst8 => self.read_u8(offset) as usize,
                    OpCode::LoadConst16 => self.read_u16(offset) as usize,
                    _ => unreachable!(),
                };
                let val = &self.constants[idx];
                println!(" {:4} ({val})", idx);
            }
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::Loop => {
                let jump_offset = self.read_u16(offset) as usize;
                let target = if matches!(op, OpCode::Loop) {
                    *offset - jump_offset
                } else {
                    *offset + jump_offset
                };

                println!(
                    " {:4} ({}{:04})",
                    offset,
                    match op {
                        OpCode::Jump => "->",
                        OpCode::JumpIfFalse => "?->",
                        OpCode::Loop => "<-",
                        _ => unreachable!(),
                    },
                    target
                );
            }
            _ => println!(),
        }
    }

    pub fn disassemble(&self) {
        let mut instructions = 0usize;
        let mut offset = 0;

        while offset < self.source.len() {
            instructions += 1;
            self.disassemble_op(&mut offset);
        }

        println!(
            "{} instructions and {} bytes",
            instructions,
            self.source.len()
        );
    }
}
