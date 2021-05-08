use std::{borrow::Cow, fmt};

/// Data required for storing a number
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteralString {
    pub radix: Option<Radix>,
    pub exact: Option<bool>,
    pub polar_form: bool,
    pub real: NumberString,
    pub imaginary: NumberString,
}

/// A Single numeric component
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumberString {
    Decimal(Cow<'static, str>),
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

impl NumberString {
    pub const ZERO: NumberString = NumberString::Integer(Cow::Borrowed("0"));
}

// Formatters

impl fmt::Display for NumericLiteralString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let NumericLiteralString {
            exact,
            radix,
            real,
            imaginary,
            polar_form,
        } = self;

        if *exact == Some(true) {
            write!(f, "#e")?;
        } else if *exact == Some(false) {
            write!(f, "#i")?;
        }
        match radix {
            Some(Radix::Binary) => write!(f, "#b")?,
            Some(Radix::Octal) => write!(f, "#o")?,
            Some(Radix::Hexadecimal) => write!(f, "#x")?,
            _ => (),
        }
        if *polar_form {
            write!(f, "Number({} @ {})", real, imaginary)?;
        } else {
            write!(f, "Number({} + {}i)", real, imaginary)?;
        }

        Ok(())
    }
}

impl fmt::Display for NumberString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NumberString::Decimal(val) => write!(f, "Decimal({})", val),
            NumberString::Integer(val) => write!(f, "Integer({})", val),
            NumberString::Fraction(num, denom) => write!(f, "Fraction({} / {})", num, denom),
        }
    }
}
