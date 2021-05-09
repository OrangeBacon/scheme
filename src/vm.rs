use std::{any::Any, fmt::Display};

use lasso::Spur;

use crate::numerics::{ComplexNumber, Number};

pub enum Value {
    Boolean(bool),
    Character(char),
    String(Spur),
    Number(Number),
    Complex(Box<ComplexNumber>),
    Pair([Box<Value>; 2]),
    Vector(Vec<Value>),
    ExternFunction(u32),
    ExternValue(Box<dyn CustomSchemeValue>),
}

pub trait CustomSchemeValue: Any + Display {}
impl<T: Any + Display> CustomSchemeValue for T {}
