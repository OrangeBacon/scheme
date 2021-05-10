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
    pub(crate) content: Vec<u8>,
    pub(crate) constants: Vec<Value>,
    location_info: Vec<(usize, usize)>,
}

impl BytecodeChunk {
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self {
            name: name.into(),
            content: vec![],
            constants: vec![],
            location_info: vec![(0, 0)],
        }
    }

    pub fn location(&mut self, value: usize) {
        self.location_info.push((value, 0));
    }

    pub fn write(&mut self, value: impl Into<u8>) {
        self.content.push(value.into());

        let latest_location = self.location_info.len() - 1;
        self.location_info[latest_location].1 += 1;
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

        let mut location_info_idx = 0;
        let mut location_info_bytes_left = 0;

        let mut offset = 0;
        while offset < self.content.len() {
            write!(f, "{:04} ", offset)?;

            if location_info_bytes_left == 0 {
                while location_info_bytes_left == 0 {
                    location_info_bytes_left = self.location_info[location_info_idx].1;

                    location_info_idx += 1;
                }

                write!(f, "{:>5} ", self.location_info[location_info_idx - 1].0)?;
            } else {
                write!(f, "    | ")?;
            }

            let bytes_consumed = self.disassemble_instruction(f, offset)?;

            offset += bytes_consumed;
            location_info_bytes_left = location_info_bytes_left.saturating_sub(bytes_consumed);
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
        let opcode = match OpCode::from_u8(self.content[offset]) {
            Some(val) => val,
            None => {
                writeln!(f, "Unknown opcode")?;
                return Ok(1);
            }
        };

        let offset = match opcode {
            OpCode::Return => self.simple_instruction(f, "OP_RETURN")?,
            OpCode::LoadConstant => self.constant_instruction(f, "OP_LOAD_CONSTANT", offset)?,
        };

        Ok(offset)
    }

    fn simple_instruction(&self, f: &mut fmt::Formatter, name: &str) -> Result<usize, fmt::Error> {
        writeln!(f, "{}", name)?;
        Ok(1)
    }

    fn constant_instruction(
        &self,
        f: &mut fmt::Formatter,
        name: &str,
        offset: usize,
    ) -> Result<usize, fmt::Error> {
        if let Some(&id) = self.content.get(offset + 1) {
            if let Some(constant) = self.constants.get(id as usize) {
                writeln!(f, "{:<20} id: {} => {}", name, id, constant)?;
            } else {
                writeln!(f, "{:<20} id: {} => invalid_constant", name, id)?;
            }

            Ok(2)
        } else {
            writeln!(f, "{:<20}  id => invalid_bytecode", name)?;
            Ok(1)
        }
    }
}
