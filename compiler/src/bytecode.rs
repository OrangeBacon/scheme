use std::{borrow::Cow, fmt};

use num_bigint::BigInt;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{memory::Heap, numerics::Number, value::Value};

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

/// Bytecode representation of a single function
#[derive(Debug)]
pub struct BytecodeChunk {
    name: Cow<'static, str>,
    pub(crate) content: Vec<u8>,
    constant_heap: Heap,
    constants: Vec<Value>,
    location_info: Vec<(usize, usize)>,
}

impl BytecodeChunk {
    /// Create a new chunk of bytecode with a given name
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self {
            name: name.into(),
            content: vec![],
            constant_heap: Heap::new(),
            constants: vec![],
            location_info: vec![(0, 0)],
        }
    }

    /// Set the current character index information for debug output and stack traces
    pub fn location(&mut self, value: usize) {
        self.location_info.push((value, 0));
    }

    /// Write a single byte to the bytecode
    pub fn write(&mut self, value: impl Into<u8>) {
        self.content.push(value.into());

        let latest_location = self.location_info.len() - 1;
        self.location_info[latest_location].1 += 1;
    }

    /// Create a new boolean constant
    pub fn bool(&mut self, val: bool) -> usize {
        let val = self.constant_heap.bool(val);
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Create a new character constant
    pub fn char(&mut self, val: char) -> usize {
        let val = self.constant_heap.char(val);
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Create a new string constant
    pub fn string(&mut self, val: String) -> usize {
        let val = self.constant_heap.string(val);
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Create a new list constant
    pub fn list(&mut self, val: Vec<Value>) -> usize {
        let val = self.constant_heap.list(val);
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Create a new vector constant
    pub fn vector(&mut self, val: Vec<Value>) -> usize {
        let val = self.constant_heap.vector(val);
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Convert a constant number into a constant
    pub fn number(&mut self, val: Number) -> usize {
        let val = self.constant_heap.number(val);
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Convert constant numbers into a complex number constant
    pub fn complex(&mut self, real: Number, imaginary: Number) -> usize {
        let val = self.constant_heap.complex(real, imaginary);
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Create an i64 number constant
    pub fn integer(&mut self, val: i64) -> Number {
        self.constant_heap.integer(val)
    }

    /// Create an f32 number constant
    pub fn single(&mut self, val: f32) -> Number {
        self.constant_heap.single(val)
    }

    /// Create an f64 number constant
    pub fn double(&mut self, val: f64) -> Number {
        self.constant_heap.double(val)
    }

    /// Create a big integer number constant
    pub fn bigint(&mut self, val: BigInt) -> Number {
        self.constant_heap.bigint(val)
    }

    /// Create a rational number constant
    pub fn rational(&mut self, numerator: BigInt, denominator: BigInt) -> Number {
        self.constant_heap.rational(numerator, denominator)
    }

    /// Get a constant value stored in the bytecode
    pub fn get_constant(&self, idx: usize) -> Value {
        self.constants[idx]
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
                writeln!(f, "{:<20} id: {} => {:?}", name, id, constant)?;
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
