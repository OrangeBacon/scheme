use std::{borrow::Cow, fmt, num::ParseFloatError};

use num_bigint::{BigInt, ParseBigIntError};
use num_traits::{Num, Signed, ToPrimitive};
use thiserror::Error;

use crate::vm::Value;

#[derive(Debug, Error)]
pub enum NumericError {
    #[error("Error converting number to big integer, this should not be possible: {source}")]
    BigIntError {
        #[from]
        source: ParseBigIntError,
    },

    #[error("Error parsing number as float, this should not be possible: {source}")]
    FloatParseError {
        #[from]
        source: ParseFloatError,
    },

    #[error("A numeric literal containing hashes (`#`) cannot be specified as exact.")]
    ExactHash,

    #[error("Unable to convert big integer to floating point type")]
    BigIntFloatConvert,

    #[error("Numbers using either decimal points or exponents cannot be specified as exact")]
    ExactDecimal,

    #[error("Decimal numbers must be in base 10")]
    DecimalNotBase10,

    #[error("Complex numbers specified in polar form must have both magnitude and angle")]
    PolarMissingComponent,

    #[error("Complex numbers specified in polar form cannot be represented exactly")]
    PolarExact,
}

/// Data required for storing a number as a series of strings
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteralString {
    pub radix: Option<Radix>,
    pub exact: Option<bool>,
    pub polar_form: bool,
    pub real: Option<NumberString>,
    pub imaginary: Option<NumberString>,
}

/// A single numeric component stored as strings
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumberString {
    Decimal(Cow<'static, str>, Option<ExponentKind>),
    Integer(Cow<'static, str>),
    Fraction(Cow<'static, str>, Cow<'static, str>),
}

/// Radices (bases) available for numeric literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Radix {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

/// The kind of exponent that was specified in a decimal number
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExponentKind {
    Exponential(Cow<'static, str>),
    Short(Cow<'static, str>),
    Float(Cow<'static, str>),
    Double(Cow<'static, str>),
    Long(Cow<'static, str>),
}

/// Representation of a number
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Number {
    Integer(i64),
    BigInteger(BigInt),
    Single(f32),
    Double(f64),
    Real(Box<[BigInt; 2]>),
}

/// Representation of a complex number
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ComplexNumber {
    real: Number,
    imaginary: Number,
}

// implementations

impl NumericLiteralString {
    pub fn to_value(self) -> Result<Value, NumericError> {
        let radix = self.radix.unwrap_or(Radix::Decimal);
        let exactness = self.exact;

        if self.polar_form {
            if self.exact == Some(true) {
                return Err(NumericError::PolarExact);
            }

            let magnitude = self.real.ok_or(NumericError::PolarMissingComponent)?;
            let angle = self.imaginary.ok_or(NumericError::PolarMissingComponent)?;

            let magnitude = magnitude.to_number(radix, exactness)?;
            let angle = angle.to_number(radix, exactness)?;

            let magnitude = magnitude.to_f64().ok_or(NumericError::BigIntFloatConvert)?;
            let angle = angle.to_f64().ok_or(NumericError::BigIntFloatConvert)?;

            // for polar->rectangular conversion:
            // real = mag*cos(angle)
            // imaginary = mag*sin(angle)

            Ok(Value::Complex(Box::new(ComplexNumber {
                real: Number::Double(magnitude * angle.cos()),
                imaginary: Number::Double(magnitude * angle.sin()),
            })))
        } else if self.imaginary.is_none() {
            let real = self
                .real
                .map(|num| num.to_number(radix, exactness))
                .unwrap_or(Ok(Number::Integer(0)))?;

            Ok(Value::Number(real))
        } else {
            let real = self
                .real
                .map(|num| num.to_number(radix, exactness))
                .unwrap_or(Ok(Number::Integer(0)))?;

            let imaginary = self
                .imaginary
                .map(|num| num.to_number(radix, exactness))
                .unwrap_or(Ok(Number::Integer(0)))?;

            Ok(Value::Complex(Box::new(ComplexNumber { real, imaginary })))
        }
    }
}

impl NumberString {
    fn to_number(&self, radix: Radix, exactness: Option<bool>) -> Result<Number, NumericError> {
        let radix_num = radix.to_number();

        match self {
            NumberString::Decimal(val, exp) => {
                if exactness == Some(true) {
                    return Err(NumericError::ExactDecimal);
                }

                if radix != Radix::Decimal {
                    return Err(NumericError::DecimalNotBase10);
                }

                let val = val.replace('#', "0");

                match exp {
                    Some(
                        ExponentKind::Exponential(exp)
                        | ExponentKind::Double(exp)
                        | ExponentKind::Long(exp),
                    ) => {
                        let num = format!("{}e{}", val, exp);
                        Ok(Number::Double(num.parse()?))
                    }
                    Some(ExponentKind::Float(exp) | ExponentKind::Short(exp)) => {
                        let num = format!("{}e{}", val, exp);
                        Ok(Number::Single(num.parse()?))
                    }
                    None => Ok(Number::Double(val.parse()?)),
                }
            }
            NumberString::Integer(val) => {
                let contains_hash = val.contains('#');

                if contains_hash && exactness == Some(true) {
                    return Err(NumericError::ExactHash);
                }

                let val = val.replace('#', "0");

                if let Ok(num) = i64::from_str_radix(&val, radix_num) {
                    if contains_hash || exactness == Some(false) {
                        Ok(Number::Double(num as f64))
                    } else {
                        Ok(Number::Integer(num))
                    }
                } else {
                    // already validated the digits within the number, so if
                    // parsing fails, the number is too large to fit in
                    let num = BigInt::from_str_radix(&val, radix_num)?;

                    if contains_hash || exactness == Some(false) {
                        // it is still worth going via a bigint as it allows
                        // integers larger than i64 to be made inexact
                        Ok(Number::Double(
                            num.to_f64().ok_or(NumericError::BigIntFloatConvert)?,
                        ))
                    } else {
                        Ok(Number::BigInteger(num))
                    }
                }
            }
            NumberString::Fraction(numerator, denominator) => {
                let contains_hash = numerator.contains('#') || denominator.contains('#');

                let numerator = numerator.replace('#', "0");
                let denominator = denominator.replace('#', "0");

                let numbers = [
                    BigInt::from_str_radix(&numerator, radix_num)?,
                    BigInt::from_str_radix(&denominator, radix_num)?,
                ];

                if contains_hash || exactness == Some(false) {
                    let numerator = numbers[0]
                        .to_f64()
                        .ok_or(NumericError::BigIntFloatConvert)?;
                    let denominator = numbers[1]
                        .to_f64()
                        .ok_or(NumericError::BigIntFloatConvert)?;

                    Ok(Number::Double(numerator / denominator))
                } else {
                    Ok(Number::Real(Box::new(numbers)))
                }
            }
        }
    }
}

impl Radix {
    fn to_number(&self) -> u32 {
        match self {
            Radix::Binary => 2,
            Radix::Octal => 8,
            Radix::Decimal => 10,
            Radix::Hexadecimal => 16,
        }
    }
}

impl Number {
    fn to_f64(&self) -> Option<f64> {
        Some(match self {
            Number::Integer(val) => *val as _,
            Number::Single(val) => *val as _,
            Number::Double(val) => *val,

            Number::BigInteger(val) => val.to_f64()?,
            Number::Real(val) => val[0].to_f64()? / val[1].to_f64()?,
        })
    }
}

// Formatters

impl fmt::Display for ComplexNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let is_positive = match &self.imaginary {
            Number::Integer(val) => val.is_positive(),
            Number::BigInteger(val) => val.is_positive(),
            Number::Single(val) => val.is_positive(),
            Number::Double(val) => val.is_positive(),
            Number::Real(val) => val[0].is_positive() && val[1].is_positive(),
        };

        let sign = if is_positive { "+" } else { "" };

        write!(f, "{}{}{}i", self.real, sign, self.imaginary)
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Integer(val) => write!(f, "{}", val),
            Number::BigInteger(val) => write!(f, "{}", val),
            Number::Single(val) => write!(f, "{}", val),
            Number::Double(val) => write!(f, "{}", val),
            Number::Real(val) => {
                write!(f, "({} / {})", val[0], val[1])
            }
        }
    }
}

impl fmt::Display for NumericLiteralString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone().to_value() {
            Ok(val) => write!(f, "{}", val),
            Err(_) => write!(f, "invalid_number"),
        }
    }
}
