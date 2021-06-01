use crate::{
    lexer::LexerError,
    numerics::{ExponentKind, NumberString, NumericLiteralString, Radix},
};

use super::{Lexer, Token};

impl Lexer {
    /// Parse any kind of generic numeric literal
    pub fn parse_number(&mut self, mut next: char) -> Option<Token> {
        let mut number = NumericLiteralString {
            radix: None,
            exact: None,
            polar_form: false,
            real: None,
            imaginary: None,
        };

        let mut started = false;

        // prefixes
        fn match_exactness(ch: char) -> Option<bool> {
            match ch {
                'e' | 'E' => Some(true),
                'i' | 'I' => Some(false),
                _ => None,
            }
        }

        fn match_radix(ch: char) -> Option<Radix> {
            match ch {
                'b' | 'B' => Some(Radix::Binary),
                'o' | 'O' => Some(Radix::Octal),
                'd' | 'D' => Some(Radix::Decimal),
                'x' | 'X' => Some(Radix::Hexadecimal),
                _ => None,
            }
        }

        let error = |num, error| -> Option<Token> {
            Some(
                Token::Number {
                    value: Box::new(num),
                    error: Some(error),
                }
                .into(),
            )
        };

        if next == '#' {
            let mut peek = self.peek(0)?;
            number.radix = match_radix(peek);
            number.exact = match_exactness(peek);

            if number.radix != None || number.exact != None {
                self.advance();
                next = match self.advance() {
                    Some(c) => c,
                    None => return error(number, LexerError::NonTerminatedNumber),
                };
                if next == '#' {
                    peek = match self.advance() {
                        Some(c) => c,
                        None => return error(number, LexerError::NonTerminatedNumber),
                    };

                    if number.radix == None {
                        number.radix = match_radix(peek);
                    } else {
                        number.exact = match_exactness(peek);
                    }

                    if number.exact == None || number.radix == None {
                        return error(number, LexerError::InvalidNumericPrefix);
                    }

                    next = match self.advance() {
                        Some(c) => c,
                        None => return error(number, LexerError::NonTerminatedNumber),
                    }
                }
            }
        }

        // parse the complex number after the prefix
        if (next == '+' || next == '-') && self.peek_is(0, "iI") {
            // either +i or -i
            self.advance();

            if next == '+' {
                number.imaginary = Some(NumberString::Integer("1".into()));
            } else {
                number.imaginary = Some(NumberString::Integer("-1".into()));
            }

            if !self.is_delimiter(self.peek(0)) {
                return error(number, LexerError::InvalidNumericTerminator);
            }

            return Some(
                Token::Number {
                    value: Box::new(number),
                    error: None,
                }
                .into(),
            );
        }

        let first = match self.parse_real(&number, &mut started, Some(next)) {
            Ok(num) => num,
            Err(err) => {
                if started {
                    // if a only a single character was consumed in the current number
                    // and that character was a dot and (given there was a parse real
                    // error) there are no numeric tokens following the dot that could
                    // make it a number starting with a dot, the token is instead the
                    // '.' symbol, not a number.
                    if self.current - self.start == 1 && self.source[self.start] == '.' {
                        return Some(Token::Dot);
                    }

                    return error(number, err);
                } else {
                    return None;
                }
            }
        };

        match self.peek(0) {
            Some('@') => {
                // polar form complex number
                number.polar_form = true;
                self.advance();
                number.real = Some(first);
                let next = self.advance();
                number.imaginary = match self.parse_real(&number, &mut started, next) {
                    Ok(num) => Some(num),
                    Err(err) => return error(number, err),
                };
            }
            Some(val @ ('+' | '-')) => {
                if self.peek_is(1, "iI") {
                    // <real R> [+-] i
                    self.advance();
                    self.advance();
                    number.real = Some(first);
                    if val == '+' {
                        number.imaginary = Some(NumberString::Integer("1".into()));
                    } else {
                        number.imaginary = Some(NumberString::Integer("-1".into()));
                    }
                } else {
                    // <real R> [+-] <ureal R> i
                    number.real = Some(first);
                    let next = self.advance();

                    let err = match self.parse_real(&number, &mut started, next) {
                        Ok(real) => {
                            number.imaginary = Some(real);
                            None
                        }
                        Err(err) => Some(err),
                    };

                    if !self.peek_is(0, "iI") {
                        if let Some(err) = err {
                            return error(number, err);
                        }
                        return error(number, LexerError::NonTerminatedNumber);
                    }
                    self.advance();

                    if let Some(err) = err {
                        return error(number, err);
                    }
                }
            }
            Some('i') => {
                // [+-] <ureal R> i
                self.advance();
                number.imaginary = Some(first);
            }
            _ => number.real = Some(first),
        }

        if !self.is_delimiter(self.peek(0)) {
            return error(number, LexerError::InvalidNumericTerminator.into());
        }

        Some(
            Token::Number {
                value: Box::new(number),
                error: None,
            }
            .into(),
        )
    }

    fn parse_real(
        &mut self,
        number: &NumericLiteralString,
        started: &mut bool,
        next: Option<char>,
    ) -> Result<NumberString, LexerError> {
        let mut num = String::new();
        let mut next = match next {
            Some(ch) => ch,
            None => return Err(LexerError::NonTerminatedNumber),
        };

        if matches!(next, '+' | '-') {
            *started = true;
            num.push(next);
            next = self.advance().ok_or(LexerError::NonTerminatedNumber)?;
        }

        if next == '.' {
            // decimal number beginning with a dot
            num.push('.');
            *started = true;

            self.unsigned_integer(number.radix, &mut num)?;

            while self.peek_is(0, "#") {
                num.push('#');
                self.advance();
            }

            let exponent = self.decimal_suffix()?;

            if number.radix != Some(Radix::Decimal) && number.radix != None {
                return Err(LexerError::DecimalRadix);
            }

            return Ok(NumberString::Decimal(num.into(), exponent));
        }

        if !Self::digits(number.radix).contains(next) {
            return Err(LexerError::NonTerminatedNumber);
        }
        num.push(next);
        *started = true;

        while let Some(ch) = self.peek(0) {
            if !Self::digits(number.radix).contains(ch) {
                break;
            }
            num.push(ch);
            self.advance();
        }

        match self.peek(0) {
            Some('/') => {
                // fractional number
                self.advance();
                let mut part2 = String::new();
                self.unsigned_integer(number.radix, &mut part2)?;
                while self.peek_is(0, "#") {
                    part2.push('#');
                    self.advance();
                }
                Ok(NumberString::Fraction(num.into(), part2.into()))
            }
            Some('e' | 's' | 'f' | 'd' | 'l') => {
                // number with exponential suffix
                // does not consume the peeked character as that is
                // performed inside the suffix parsing
                let exponent = self.decimal_suffix()?;

                if number.radix != Some(Radix::Decimal) && number.radix != None {
                    return Err(LexerError::DecimalRadix);
                }
                Ok(NumberString::Decimal(num.into(), exponent))
            }
            Some('#') => {
                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance();
                }

                if !self.peek_is(0, ".") {
                    // found <uinteger R>
                    return Ok(NumberString::Integer(num.into()));
                }

                // <digit 10>+ #+ . #* <suffix>
                self.advance();
                num.push('.');

                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance();
                }

                let exponent = self.decimal_suffix()?;

                if number.radix != Some(Radix::Decimal) && number.radix != None {
                    return Err(LexerError::DecimalRadix);
                }

                Ok(NumberString::Decimal(num.into(), exponent))
            }
            Some('.') => {
                // <digit 10>+ . <digit 10>* #* <suffix>
                self.advance();
                num.push('.');

                while let Some(ch) = self.peek(0) {
                    if !Self::digits(Some(Radix::Decimal)).contains(ch) {
                        break;
                    }
                    num.push(ch);
                    self.advance();
                }

                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance();
                }

                let exponent = self.decimal_suffix()?;

                if number.radix != Some(Radix::Decimal) && number.radix != None {
                    return Err(LexerError::DecimalRadix);
                }

                Ok(NumberString::Decimal(num.into(), exponent))
            }
            _ => Ok(NumberString::Integer(num.into())),
        }
    }

    /// Parse a single decimal prefix
    fn decimal_suffix(&mut self) -> Result<Option<ExponentKind>, LexerError> {
        let kind = match self.peek(0) {
            Some(peek) => {
                if !"esfdl".contains(peek) {
                    return Ok(None);
                }
                peek
            }
            None => return Ok(None),
        };

        let mut num = String::new();
        self.advance();

        let mut next = self.advance().ok_or(LexerError::NonTerminatedNumber)?;

        if matches!(next, '+' | '-') {
            num.push(next);
            next = self.advance().ok_or(LexerError::NonTerminatedNumber)?;
        }

        if !"0123456789".contains(next) {
            return Err(LexerError::InvalidExponential);
        }
        num.push(next);

        while let Some(val) = self.peek(0) {
            if "0123456789".contains(val) {
                num.push(val);
                self.advance();
            } else {
                break;
            }
        }

        Ok(Some(match kind {
            'e' => ExponentKind::Exponential(num.into()),
            's' => ExponentKind::Short(num.into()),
            'f' => ExponentKind::Float(num.into()),
            'd' => ExponentKind::Double(num.into()),
            'l' => ExponentKind::Long(num.into()),
            _ => unreachable!(),
        }))
    }

    fn unsigned_integer(
        &mut self,
        radix: Option<Radix>,
        num: &mut String,
    ) -> Result<(), LexerError> {
        if let Some(ch) = self.peek(0) {
            if !Self::digits(radix).contains(ch) {
                return Err(LexerError::NonTerminatedNumber);
            }
            num.push(ch);
            self.advance();
        } else {
            return Err(LexerError::NonTerminatedNumber);
        }

        while let Some(ch) = self.peek(0) {
            if !Self::digits(radix).contains(ch) {
                break;
            }
            num.push(ch);
            self.advance();
        }

        Ok(())
    }

    /// Get the character set for the given number's radix
    fn digits(radix: Option<Radix>) -> &'static str {
        match radix {
            Some(Radix::Binary) => "01",
            Some(Radix::Octal) => "01234567",
            Some(Radix::Hexadecimal) => "0123456789abcdefABCDEF",
            Some(Radix::Decimal) | None => "0123456789",
        }
    }
}
