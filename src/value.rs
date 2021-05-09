use std::{any::Any, fmt};

use crate::numerics::{ComplexNumber, Number};

pub enum Value {
    Boolean(bool),
    Character(char),
    String(String),
    Number(Number),
    Complex(Box<ComplexNumber>),
    Pair([Box<Value>; 2]),
    Vector(Vec<Value>),
    NativeFunction(u32),
    NativeValue(Box<dyn CustomSchemeValue>),
}

pub trait CustomSchemeValue: Any + fmt::Display {}
impl<T: Any + fmt::Display> CustomSchemeValue for T {}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(val) => {
                if *val {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }

            Value::Character(val) => {
                if *val == ' ' {
                    write!(f, r"#\space")
                } else if *val == '\n' {
                    write!(f, r"#\newline")
                } else {
                    write!(f, r"#\{}", *val)
                }
            }

            Value::Vector(val) => {
                write!(f, "#(")?;
                for (idx, val) in val.iter().enumerate() {
                    if idx != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, ")")
            }

            Value::String(val) => write!(f, "{:?}", val),
            Value::Number(val) => write!(f, "{}", val),
            Value::Complex(val) => write!(f, "{}", val),
            Value::Pair(val) => write!(f, "({} . {})", val[0], val[1]),
            Value::NativeFunction(val) => write!(f, "{{native fn {}}}", val),
            Value::NativeValue(val) => write!(f, "{{native value: {:p}}}", val),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
