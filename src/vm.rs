use num_traits::FromPrimitive;

use crate::{
    bytecode::{BytecodeChunk, OpCode},
    value::Value,
};

#[derive(Debug)]
pub struct VM<'a> {
    ip: usize,
    stack: Vec<Value>,
    bytecode: &'a BytecodeChunk,
}

impl<'a> VM<'a> {
    pub fn new(bytecode: &'a BytecodeChunk) -> Self {
        Self {
            stack: vec![],
            ip: 0,
            bytecode,
        }
    }

    pub fn run(&mut self) {
        loop {
            let opcode = OpCode::from_u8(self.read_byte()).unwrap();

            match opcode {
                OpCode::Return => {
                    println!("{}", self.stack.pop().unwrap());
                    break;
                }
                OpCode::LoadConstant => {
                    let id = self.read_byte();
                    self.stack.push(self.bytecode.get_constant(id as usize));
                }
            }
        }
    }

    #[inline(always)]
    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode.content[self.ip];
        self.ip += 1;
        byte
    }
}
