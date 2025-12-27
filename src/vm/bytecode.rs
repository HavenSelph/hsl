use crate::vm::value::Value;
use int_enum::IntEnum;
use name_variant::NamedVariant;

#[repr(u8)]
#[derive(IntEnum, NamedVariant, Debug, Copy, Clone)]
pub enum OpCode {
    Return = 0,
    Const = 1,
    Add = 3,
    Sub = 4,
    Mul = 5,
    Div = 6,
    Less = 7,
    Greater = 8,
    Equal = 9,
    And = 11,
    Or = 12,
    Not = 10,
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

    pub fn write_const(&mut self, value: Value) {
        if let Some(idx) = self.constants.iter().position(|v| *v == value) {
            self.write_op_with_u16(OpCode::Const, idx as u16);
        } else {
            let idx = self.constants.len();
            self.constants.push(value);
            self.write_op_with_u16(OpCode::Const, idx as u16);
        }
    }

    pub fn read_u8(&mut self, offset: &mut usize) -> u8 {
        let res = self.source[*offset];
        *offset += 1;
        res
    }

    pub fn read_u16(&mut self, offset: &mut usize) -> u16 {
        let res: u16 = (self.read_u8(offset) as u16) << 8 | self.read_u8(offset) as u16;
        res
    }

    pub fn read_u32(&mut self, offset: &mut usize) -> u32 {
        let res: u32 = (self.read_u16(offset) as u32) << 16 | self.read_u16(offset) as u32;
        res
    }

    pub fn read_op(&mut self, offset: &mut usize) -> OpCode {
        let op = self.read_u8(offset).try_into().unwrap();
        op
    }

    pub fn read_const(&mut self, offset: &mut usize) -> Value {
        let idx = self.read_u16(offset);
        self.constants[idx as usize].clone()
    }

    pub fn disassemble_op(&mut self, op: OpCode, offset: &mut usize) {
        print!("{:03} | {:#04x} {}", offset, op as usize, op.variant_name());
        match op {
            OpCode::Const => {
                let idx = self.read_u16(offset);
                let val = &self.constants[idx as usize];
                print!(" | {:04x} = {:?}", idx, val);
            }
            _ => (),
        }
        println!();
    }

    pub fn disassemble(&mut self) {
        let mut offset = 0;
        let mut instructions = 0usize;

        while offset < self.source.len() {
            instructions += 1;
            let op = self.read_op(&mut offset);
            self.disassemble_op(op, &mut offset);
        }

        println!(
            "{} instructions and {} bytes",
            instructions,
            self.source.len()
        );
    }
}
