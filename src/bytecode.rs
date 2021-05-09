use std::{borrow::Cow, fmt};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::value::Value;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return = 0,
    LoadConstant = 1,
}

impl From<OpCode> for u8 {
    fn from(val: OpCode) -> Self {
        val as Self
    }
}

#[derive(Debug)]
pub struct BytecodeChunk {
    name: Cow<'static, str>,
    content: Vec<u8>,
    constants: Vec<Value>,
}

impl BytecodeChunk {
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self {
            name: name.into(),
            content: vec![],
            constants: vec![],
        }
    }

    pub fn write(&mut self, value: impl Into<u8>) {
        self.content.push(value.into())
    }

    pub fn write_constant(&mut self, value: impl Into<Value>) -> u8 {
        self.constants.push(value.into());
        (self.constants.len() - 1) as _
    }
}

impl Default for BytecodeChunk {
    fn default() -> Self {
        Self::new("Unknown Chunk")
    }
}

impl fmt::Display for BytecodeChunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "=== {} ===", self.name)?;

        let mut offset = 0;
        while offset < self.content.len() {
            offset = self.disassemble_instruction(f, offset)?;
        }

        Ok(())
    }
}

impl BytecodeChunk {
    pub fn disassemble_instruction(
        &self,
        f: &mut fmt::Formatter,
        offset: usize,
    ) -> Result<usize, fmt::Error> {
        write!(f, "{:04} ", offset)?;

        let opcode = match OpCode::from_u8(self.content[offset]) {
            Some(val) => val,
            None => {
                writeln!(f, "Unknown opcode")?;
                return Ok(offset + 1);
            }
        };

        let offset = match opcode {
            OpCode::Return => self.simple_instruction(f, "OP_RETURN", offset)?,
            OpCode::LoadConstant => self.constant_instruction(f, "OP_LOAD_CONSTANT", offset)?,
        };

        Ok(offset)
    }

    fn simple_instruction(
        &self,
        f: &mut fmt::Formatter,
        name: &str,
        offset: usize,
    ) -> Result<usize, fmt::Error> {
        writeln!(f, "{}", name)?;
        Ok(offset + 1)
    }

    fn constant_instruction(
        &self,
        f: &mut fmt::Formatter,
        name: &str,
        offset: usize,
    ) -> Result<usize, fmt::Error> {
        if let Some(&id) = self.content.get(offset + 1) {
            if let Some(constant) = self.constants.get(id as usize) {
                writeln!(f, "{}  id: {} => {}", name, id, constant)?;
            } else {
                writeln!(f, "{}  id: {} => invalid_constant", name, id)?;
            }

            Ok(offset + 2)
        } else {
            writeln!(f, "{}  id => invalid_bytecode", name)?;
            Ok(offset + 1)
        }
    }
}
