use std::{borrow::Cow, fmt, num::ParseFloatError};

use num_bigint::{BigInt, ParseBigIntError};
use num_traits::{Num, ToPrimitive};
use thiserror::Error;

use crate::{memory::Heap, value::Value};

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

    #[error("Divide by zero in rational numeric literal")]
    DivideZero,
}

/// Data required for storing a number as a series of strings
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Number(pub(crate) NumberContent);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum NumberContent {
    Integer(i64),
    BigInteger(usize),
    Single(f32),
    Double(f64),
    Rational(usize, usize),
}

/// Representation of a complex number
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct ComplexNumber {
    real: Number,
    imaginary: Number,
}

// implementations

impl NumericLiteralString {
    /// Convert a string based number to a value on a given heap
    pub fn into_value(self, heap: &mut Heap) -> Result<Value, NumericError> {
        let radix = self.radix.unwrap_or(Radix::Decimal);
        let exactness = self.exact;

        if self.polar_form {
            if self.exact == Some(true) {
                return Err(NumericError::PolarExact);
            }

            let magnitude = self.real.ok_or(NumericError::PolarMissingComponent)?;
            let angle = self.imaginary.ok_or(NumericError::PolarMissingComponent)?;

            let magnitude = magnitude.to_number(heap, radix, exactness)?;
            let angle = angle.to_number(heap, radix, exactness)?;

            let magnitude = magnitude
                .to_f64(heap)
                .ok_or(NumericError::BigIntFloatConvert)?;
            let angle = angle.to_f64(heap).ok_or(NumericError::BigIntFloatConvert)?;

            // for polar->rectangular conversion:
            // real = mag*cos(angle)
            // imaginary = mag*sin(angle)

            let real = heap.double(magnitude * angle.cos());
            let imaginary = heap.double(magnitude * angle.sin());

            Ok(heap.complex(real, imaginary))
        } else if self.imaginary.is_none() {
            let real = self
                .real
                .map(|num| num.to_number(heap, radix, exactness))
                .unwrap_or_else(|| Ok(heap.integer(0)))?;

            Ok(heap.number(real))
        } else {
            let real = self
                .real
                .map(|num| num.to_number(heap, radix, exactness))
                .unwrap_or_else(|| Ok(heap.integer(0)))?;

            let imaginary = self
                .imaginary
                .map(|num| num.to_number(heap, radix, exactness))
                .unwrap_or_else(|| Ok(heap.integer(0)))?;

            Ok(heap.complex(real, imaginary))
        }
    }
}

impl NumberString {
    /// Convert a string based numeric component into a number on a heap
    fn to_number(
        &self,
        heap: &mut Heap,
        radix: Radix,
        exactness: Option<bool>,
    ) -> Result<Number, NumericError> {
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
                        Ok(heap.double(num.parse()?))
                    }
                    Some(ExponentKind::Float(exp) | ExponentKind::Short(exp)) => {
                        let num = format!("{}e{}", val, exp);
                        Ok(heap.single(num.parse()?))
                    }
                    None => Ok(heap.double(val.parse()?)),
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
                        Ok(heap.double(num as f64))
                    } else {
                        Ok(heap.integer(num))
                    }
                } else {
                    // already validated the digits within the number, so if
                    // parsing fails, the number is too large to fit in
                    let num = BigInt::from_str_radix(&val, radix_num)?;

                    if contains_hash || exactness == Some(false) {
                        // it is still worth going via a bigint as it allows
                        // integers larger than i64 to be made inexact
                        Ok(heap.double(num.to_f64().ok_or(NumericError::BigIntFloatConvert)?))
                    } else {
                        Ok(heap.bigint(num))
                    }
                }
            }
            NumberString::Fraction(numerator, denominator) => {
                let contains_hash = numerator.contains('#') || denominator.contains('#');

                let numerator = numerator.replace('#', "0");
                let denominator = denominator.replace('#', "0");

                let numerator = BigInt::from_str_radix(&numerator, radix_num)?;
                let denominator = BigInt::from_str_radix(&denominator, radix_num)?;

                // catch divide by 0 in literal, does not actually cause an error
                // now as no integer division is done and f64 division will
                // return inf, but is probably an error so should be reported
                if denominator == BigInt::from(0) {
                    return Err(NumericError::DivideZero);
                }

                if contains_hash || exactness == Some(false) {
                    let numerator = numerator.to_f64().ok_or(NumericError::BigIntFloatConvert)?;
                    let denominator = denominator
                        .to_f64()
                        .ok_or(NumericError::BigIntFloatConvert)?;

                    Ok(heap.double(numerator / denominator))
                } else {
                    Ok(heap.rational(numerator, denominator))
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
    fn to_f64(&self, heap: &mut Heap) -> Option<f64> {
        Some(match self {
            Number(NumberContent::Integer(val)) => *val as _,
            Number(NumberContent::Single(val)) => *val as _,
            Number(NumberContent::Double(val)) => *val,

            Number(NumberContent::BigInteger(idx)) => heap.get_bigint(*idx).to_f64()?,
            Number(NumberContent::Rational(numerator, denominator)) => {
                heap.get_bigint(*numerator).to_f64()? / heap.get_bigint(*denominator).to_f64()?
            }
        })
    }

    pub fn print(&self, f: &mut fmt::Formatter, heap: &Heap) -> fmt::Result {
        match self.0 {
            NumberContent::Integer(val) => write!(f, "{}", val),
            NumberContent::Single(val) => write!(f, "{}", val),
            NumberContent::Double(val) => write!(f, "{}", val),

            NumberContent::BigInteger(idx) => {
                write!(f, "{}", heap.get_bigint(idx))
            }
            NumberContent::Rational(idx_1, idx_2) => {
                write!(f, "{}/{}", heap.get_bigint(idx_1), heap.get_bigint(idx_2))
            }
        }
    }

    fn is_zero(&self, heap: &Heap) -> bool {
        match self.0 {
            NumberContent::Integer(val) => val == 0,
            NumberContent::Single(val) => val == 0.0,
            NumberContent::Double(val) => val == 0.0,
            NumberContent::BigInteger(idx) => *heap.get_bigint(idx) == BigInt::from(0),

            // only check the numerator as the denominator should never be 0
            NumberContent::Rational(idx, _) => *heap.get_bigint(idx) == BigInt::from(0),
        }
    }

    fn is_positive(&self, heap: &Heap) -> bool {
        match self.0 {
            NumberContent::Integer(val) => val > 0,
            NumberContent::Single(val) => val > 0.0,
            NumberContent::Double(val) => val > 0.0,
            NumberContent::BigInteger(idx) => *heap.get_bigint(idx) > BigInt::from(0),

            // only check the numerator as the denominator should always be positive
            NumberContent::Rational(idx, _) => *heap.get_bigint(idx) > BigInt::from(0),
        }
    }
}

impl ComplexNumber {
    pub fn new(real: Number, imaginary: Number) -> Self {
        Self { real, imaginary }
    }

    pub fn print(&self, f: &mut fmt::Formatter, heap: &Heap) -> fmt::Result {
        let has_real = !self.real.is_zero(heap);
        let has_imaginary = !self.imaginary.is_zero(heap);

        if has_real {
            self.real.print(f, heap)?;
        }

        if has_imaginary {
            if has_real && self.imaginary.is_positive(heap) {
                write!(f, "+")?;
            }
            self.imaginary.print(f, heap)?;
            write!(f, "i")?;
        }

        if !has_real && !has_imaginary {
            write!(f, "0")?;
        }

        Ok(())
    }
}
