use num_bigint::BigInt;

use crate::{
    numerics::{ComplexNumber, Number, NumberContent},
    value::{CustomSchemeValue, Value, ValueContents},
};

/// Heap storage for values, all values must be allocated through a heap.
/// This will allow for garbage collection (todo) and changing whether a
/// particular value requires heap memory or not without changing the method
/// that creates the value.
/// Note: currently the heap references in values are indices into vecs,
/// so a heap access requires indexing an array rather than dereferencing a
/// pointer.
#[derive(Debug)]
pub struct Heap {
    string: Vec<String>,
    bigint: Vec<BigInt>,
    vec: Vec<Vec<Value>>,
    native: Vec<Box<dyn CustomSchemeValue>>,
}

impl Heap {
    /// Create a new value heap
    pub fn new() -> Self {
        Self {
            string: Vec::with_capacity(0),
            bigint: Vec::with_capacity(0),
            vec: Vec::with_capacity(0),
            native: Vec::with_capacity(0),
        }
    }

    /// Create a new boolean value
    pub fn bool(&mut self, val: bool) -> Value {
        Value(ValueContents::Boolean(val))
    }

    /// Create a new character value
    pub fn char(&mut self, val: char) -> Value {
        Value(ValueContents::Character(val))
    }

    /// Create a new string value
    pub fn string(&mut self, val: String) -> Value {
        self.string.push(val);
        Value(ValueContents::String(self.string.len() - 1))
    }

    /// Create a new list value
    pub fn list(&mut self, val: Vec<Value>) -> Value {
        self.vec.push(val);
        Value(ValueContents::List(self.vec.len() - 1))
    }

    /// Create a new vector value
    pub fn vector(&mut self, val: Vec<Value>) -> Value {
        self.vec.push(val);
        Value(ValueContents::Vector(self.vec.len() - 1))
    }

    /// Convert a heap allocated number into a value
    pub fn number(&mut self, val: Number) -> Value {
        Value(ValueContents::Number(val))
    }

    /// Convert heap allocated numbers into a complex number value
    pub fn complex(&mut self, real: Number, imaginary: Number) -> Value {
        Value(ValueContents::Complex(ComplexNumber::new(real, imaginary)))
    }

    /// Create an i64 number
    pub fn integer(&mut self, val: i64) -> Number {
        Number(NumberContent::Integer(val))
    }

    /// Create an f32 number
    pub fn single(&mut self, val: f32) -> Number {
        Number(NumberContent::Single(val))
    }

    /// Create an f64 number
    pub fn double(&mut self, val: f64) -> Number {
        Number(NumberContent::Double(val))
    }

    /// Create a big integer number
    pub fn bigint(&mut self, val: BigInt) -> Number {
        self.bigint.push(val);
        let id = self.bigint.len() - 1;
        Number(NumberContent::BigInteger(id))
    }

    /// Create a rational number
    pub fn rational(&mut self, numerator: BigInt, denominator: BigInt) -> Number {
        self.bigint.push(numerator);
        let numerator = self.bigint.len() - 1;
        self.bigint.push(denominator);
        let denominator = self.bigint.len() - 1;
        Number(NumberContent::Rational(numerator, denominator))
    }

    /// Store an arbitrary value within the heap
    pub fn native(&mut self, val: impl CustomSchemeValue) -> Value {
        self.native.push(Box::new(val));
        Value(ValueContents::NativeValue(self.native.len() - 1))
    }

    /// Get a string from the heap
    pub fn get_string(&self, idx: usize) -> &str {
        &self.string[idx]
    }

    /// Get a big integer from the heap
    pub fn get_bigint(&self, idx: usize) -> &BigInt {
        &self.bigint[idx]
    }

    /// Get a vector of values from the heap
    pub fn get_vec(&self, idx: usize) -> &[Value] {
        &self.vec[idx]
    }

    /// Get a native value from the heap
    pub fn get_native(&self, idx: usize) -> &dyn CustomSchemeValue {
        &self.native[idx]
    }
}
