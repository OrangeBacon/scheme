use std::{any::Any, fmt};

use crate::numerics::{ComplexNumber, Number};

/// Wrapper to allow value to be an opaque type, so value goes through
/// a single allocator, allowing changing allocation strategy.
/// Some methods for manipulating values require a reference to a heap,
/// this must be the heap the value was allocated using
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Value(pub(crate) ValueContents);

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum ValueContents {
    Boolean(bool),
    Character(char),
    String(usize),
    Number(Number),
    Complex(ComplexNumber),
    List(usize),
    Vector(usize),
    NativeFunction(u32),
    NativeValue(usize),
}

pub trait CustomSchemeValue: Any + fmt::Debug + fmt::Display {}
impl<T: Any + fmt::Debug + fmt::Display> CustomSchemeValue for T {}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for ValueContents {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueContents::Boolean(val) => {
                if *val {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }

            ValueContents::Character(val) => {
                if *val == ' ' {
                    write!(f, r"#\space")
                } else if *val == '\n' {
                    write!(f, r"#\newline")
                } else {
                    write!(f, r"#\{}", *val)
                }
            }

            ValueContents::Vector(val) => write!(f, "#(vector {})", *val),
            ValueContents::String(val) => write!(f, "{:?}", val),
            ValueContents::Number(val) => write!(f, "{:?}", val),
            ValueContents::Complex(val) => write!(f, "{:?}", val),
            ValueContents::List(val) => write!(f, "(list {})", *val),
            ValueContents::NativeFunction(val) => write!(f, "{{native fn {}}}", val),
            ValueContents::NativeValue(val) => write!(f, "{{native value: {:p}}}", val),
        }
    }
}

impl fmt::Debug for ValueContents {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
