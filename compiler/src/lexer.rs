use std::{fmt, ops::Range};

use anyhow::Result;
use lasso::Spur;
use thiserror::Error;

use crate::{
    environment::Environment,
    numerics::{ExponentKind, NumberString, NumericLiteralString, Radix},
};

#[derive(Debug, Error, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LexerError {
    #[error("Unexpected characters: {chars:?}")]
    UnexpectedChars { chars: String },

    #[error("Unexpected value in character literal: {chars:?}")]
    BadCharacterLiteral { chars: String },

    #[error("Unexpected backslash in string literal")]
    StringBackslash,

    #[error("Non-terminated string literal")]
    NonTerminatedString,

    #[error("Non-terminated numeric literal")]
    NonTerminatedNumber,

    #[error("Invalid prefix in numeric literal")]
    InvalidNumericPrefix,

    #[error("Decimal numbers only supported in base 10")]
    DecimalRadix,

    #[error("Invalid character in identifier")]
    InvalidIdentifier,

    #[error("Invalid character in decimal exponential suffix")]
    InvalidExponential,

    #[error("Invalid character following numeric literal")]
    InvalidNumericTerminator,
}

/// Individual units of source code
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    LeftParen,
    RightParen,
    VecStart,
    Quote,
    BackQuote,
    Comma,
    CommaAt,
    Dot,
    Identifier {
        value: Spur,
        error: Option<LexerError>,
    },
    Boolean {
        value: bool,
    },
    Number {
        value: Box<NumericLiteralString>,
        error: Option<LexerError>,
    },
    Character {
        value: char,
        error: Option<LexerError>,
    },
    String {
        value: String,
        error: Option<LexerError>,
    },
    Eof,
    Error {
        error: LexerError,
    },
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::LeftParen => write!(f, "left paren '('")?,
            Token::RightParen => write!(f, "right paren ')'")?,
            Token::VecStart => write!(f, "vec start '#('")?,
            Token::Quote => write!(f, "quote '\''")?,
            Token::BackQuote => write!(f, "back quote '`'")?,
            Token::Comma => write!(f, "comma ','")?,
            Token::CommaAt => write!(f, "comma at ',@'")?,
            Token::Dot => write!(f, "dot '.'")?,
            Token::Identifier { value, error } => {
                write!(f, "Identifier {:?}", value)?;
                if let Some(error) = error {
                    write!(f, " with error `{}`", error)?;
                }
            }
            Token::Boolean { value } => {
                if *value {
                    write!(f, "boolean #t")?
                } else {
                    write!(f, "boolean #f")?
                }
            }
            Token::Number { value, error } => {
                write!(f, "{:?}", value)?;
                if let Some(error) = error {
                    write!(f, " with error `{}`", error)?;
                }
            }
            Token::Character { value, error } => {
                write!(f, "character {:?}", value)?;
                if let Some(error) = error {
                    write!(f, " with error `{}`", error)?;
                }
            }
            Token::String { value, error } => {
                write!(f, "string {:?}", value)?;
                if let Some(error) = error {
                    write!(f, " with error `{}`", error)?;
                }
            }
            Token::Eof => write!(f, "eof")?,
            Token::Error { error } => write!(f, "{}", error)?,
        }

        Ok(())
    }
}

/// Wrapper providing source location information for a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithLocation<T> {
    /// The file being parsed, an index into the environment's file list
    file: usize,

    /// The number of characters this location spans
    length: usize,

    /// The character offset into the file that this span starts at
    start_offset: usize,

    /// The data that the span's source has been converted into
    content: T,
}

impl<T> WithLocation<T> {
    /// Get a ref to the content stored
    pub fn content(&self) -> &T {
        &self.content
    }

    /// Separate the content from the location data
    pub fn split(self) -> (T, WithLocation<()>) {
        (
            self.content,
            WithLocation {
                content: (),
                file: self.file,
                length: self.length,
                start_offset: self.start_offset,
            },
        )
    }

    /// Join location data with different contents
    pub fn join<U>(val: T, loc: &WithLocation<U>) -> WithLocation<T> {
        WithLocation {
            content: val,
            file: loc.file,
            length: loc.length,
            start_offset: loc.start_offset,
        }
    }

    /// Extends the source location of self until it contains up to
    /// the end of other.  If the spans are in different files, takes
    /// the file name of the first span.
    pub fn extend<U>(self, other: &WithLocation<U>) -> WithLocation<T> {
        WithLocation {
            length: other.start_offset + other.length - self.start_offset,
            ..self
        }
    }

    /// Extract the location without consuming the contents
    pub fn extract(&self) -> WithLocation<()> {
        WithLocation {
            file: self.file,
            length: self.length,
            start_offset: self.start_offset,
            content: (),
        }
    }

    pub fn file_idx(&self) -> usize {
        self.file
    }

    pub fn source_range(&self) -> Range<usize> {
        self.start_offset..(self.start_offset + self.length)
    }
}

impl<T: fmt::Display> fmt::Display for WithLocation<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{} | {}",
            self.start_offset,
            self.start_offset + self.length,
            self.content
        )
    }
}

/// State used when converting a string into a token list
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lexer {
    /// The source code being lexed
    source: Vec<char>,

    /// The index of the character at the start of the current token
    start: usize,

    /// The index of the current character being checked
    current: usize,

    /// The file number in the environment
    file_idx: usize,

    /// Data used to convert between character offsets and line/column numbers,
    /// should be much smaller than storing the information inside each token
    line_numbering: Vec<Range<usize>>,
}

impl Lexer {
    /// Create a new lexer
    pub fn new(file_idx: usize, env: &Environment) -> Self {
        Self {
            source: env.file(file_idx).content().chars().collect(),
            file_idx,
            current: 0,
            start: 0,
            line_numbering: vec![],
        }
    }

    /// Get the next token and its source location
    pub fn get_token_loc(&mut self, env: &mut Environment) -> WithLocation<Token> {
        // need to skip whitespace before recording line and column positions
        // otherwise the whitespace will be included in the token's source
        // location
        self.skip_whitespace(env);

        let tok = self.get_token(env);

        let length = self.current - self.start;

        WithLocation {
            file: self.file_idx,
            length,
            content: tok,
            start_offset: self.start,
        }
    }

    /// Get the next token from the source
    pub fn get_token(&mut self, env: &mut Environment) -> Token {
        self.skip_whitespace(env);
        self.start = self.current;

        let next = if let Some(next) = self.advance(env) {
            next
        } else {
            return Token::Eof.into();
        };

        if let Some(tok) = self.parse_identifier(next, env) {
            return tok;
        }
        if let Some(tok) = self.parse_boolean(next, env) {
            return tok;
        }
        if let Some(tok) = self.parse_number(next, env) {
            return tok;
        }
        if let Some(tok) = self.parse_character(next, env) {
            return tok;
        }
        if let Some(tok) = self.parse_string(next, env) {
            return tok;
        }
        if let Some(tok) = self.parse_symbol(next, env) {
            return tok;
        }

        Token::Error {
            error: LexerError::UnexpectedChars {
                chars: next.to_string(),
            },
        }
    }

    /// Parse a new identifier
    fn parse_identifier(&mut self, next: char, env: &mut Environment) -> Option<Token> {
        // peculiar identifiers  '+', '-', '...'
        if next == '+' && self.is_delimiter(self.peek(0), env) {
            return Some(
                Token::Identifier {
                    value: env.symbols_mut().get_or_intern_static("+"),
                    error: None,
                }
                .into(),
            );
        }

        if next == '-' && self.is_delimiter(self.peek(0), env) {
            return Some(
                Token::Identifier {
                    value: env.symbols_mut().get_or_intern_static("-"),
                    error: None,
                }
                .into(),
            );
        }

        if next == '.'
            && self.peek_is(0, ".")
            && self.peek_is(1, ".")
            && self.is_delimiter(self.peek(2), env)
        {
            self.advance(env);
            self.advance(env);
            return Some(
                Token::Identifier {
                    value: env.symbols_mut().get_or_intern_static("..."),
                    error: None,
                }
                .into(),
            );
        }

        // regular identifiers
        if self.is_initial(next, env) {
            let mut ident = String::from(next);

            while let Some(ch) = self.peek(0) {
                let test = if env.config().unicode_identifiers {
                    ch.is_numeric()
                } else {
                    ch.is_ascii_digit()
                };

                if test || self.is_initial(ch, env) || "+-.@".contains(ch) {
                    self.advance(env);
                    ident.push(ch);
                } else {
                    break;
                }
            }

            let error = if !self.is_delimiter(self.peek(0), env) {
                Some(LexerError::InvalidIdentifier)
            } else {
                None
            };

            return Some(
                Token::Identifier {
                    value: env.symbols_mut().get_or_intern(ident),
                    error,
                }
                .into(),
            );
        }

        None
    }

    /// Parse a boolean true or false
    fn parse_boolean(&mut self, next: char, env: &mut Environment) -> Option<Token> {
        if next == '#' && self.peek_is(0, "tfTF") {
            self.advance(env);
            return Some(
                Token::Boolean {
                    value: self.peek_is(0, "tT"),
                }
                .into(),
            );
        }

        None
    }

    /// Parse any kind of generic numeric literal
    fn parse_number(&mut self, mut next: char, env: &mut Environment) -> Option<Token> {
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
                self.advance(env);
                next = match self.advance(env) {
                    Some(c) => c,
                    None => return error(number, LexerError::NonTerminatedNumber),
                };
                if next == '#' {
                    peek = match self.advance(env) {
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

                    next = match self.advance(env) {
                        Some(c) => c,
                        None => return error(number, LexerError::NonTerminatedNumber),
                    }
                }
            }
        }

        // parse the complex number after the prefix
        if (next == '+' || next == '-') && self.peek_is(0, "iI") {
            // either +i or -i
            self.advance(env);

            if next == '+' {
                number.imaginary = Some(NumberString::Integer("1".into()));
            } else {
                number.imaginary = Some(NumberString::Integer("-1".into()));
            }

            if !self.is_delimiter(self.peek(0), env) {
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

        let first = match self.parse_real(&number, &mut started, Some(next), env) {
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
                self.advance(env);
                number.real = Some(first);
                let next = self.advance(env);
                number.imaginary = match self.parse_real(&number, &mut started, next, env) {
                    Ok(num) => Some(num),
                    Err(err) => return error(number, err),
                };
            }
            Some(val @ ('+' | '-')) => {
                if self.peek_is(1, "iI") {
                    // <real R> [+-] i
                    self.advance(env);
                    self.advance(env);
                    number.real = Some(first);
                    if val == '+' {
                        number.imaginary = Some(NumberString::Integer("1".into()));
                    } else {
                        number.imaginary = Some(NumberString::Integer("-1".into()));
                    }
                } else {
                    // <real R> [+-] <ureal R> i
                    number.real = Some(first);
                    let next = self.advance(env);

                    let err = match self.parse_real(&number, &mut started, next, env) {
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
                    self.advance(env);

                    if let Some(err) = err {
                        return error(number, err);
                    }
                }
            }
            Some('i') => {
                // [+-] <ureal R> i
                self.advance(env);
                number.imaginary = Some(first);
            }
            _ => number.real = Some(first),
        }

        if !self.is_delimiter(self.peek(0), env) {
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
        env: &mut Environment,
    ) -> Result<NumberString, LexerError> {
        let mut num = String::new();
        let mut next = match next {
            Some(ch) => ch,
            None => return Err(LexerError::NonTerminatedNumber),
        };

        if matches!(next, '+' | '-') {
            *started = true;
            num.push(next);
            next = self.advance(env).ok_or(LexerError::NonTerminatedNumber)?;
        }

        if next == '.' {
            // decimal number beginning with a dot
            num.push('.');
            *started = true;

            self.unsigned_integer(number.radix, &mut num, env)?;

            while self.peek_is(0, "#") {
                num.push('#');
                self.advance(env);
            }

            let exponent = self.decimal_suffix(env)?;

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
            self.advance(env);
        }

        match self.peek(0) {
            Some('/') => {
                // fractional number
                self.advance(env);
                let mut part2 = String::new();
                self.unsigned_integer(number.radix, &mut part2, env)?;
                while self.peek_is(0, "#") {
                    part2.push('#');
                    self.advance(env);
                }
                Ok(NumberString::Fraction(num.into(), part2.into()))
            }
            Some('e' | 's' | 'f' | 'd' | 'l') => {
                // number with exponential suffix
                // does not consume the peeked character as that is
                // performed inside the suffix parsing
                let exponent = self.decimal_suffix(env)?;

                if number.radix != Some(Radix::Decimal) && number.radix != None {
                    return Err(LexerError::DecimalRadix);
                }
                Ok(NumberString::Decimal(num.into(), exponent))
            }
            Some('#') => {
                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance(env);
                }

                if !self.peek_is(0, ".") {
                    // found <uinteger R>
                    return Ok(NumberString::Integer(num.into()));
                }

                // <digit 10>+ #+ . #* <suffix>
                self.advance(env);
                num.push('.');

                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance(env);
                }

                let exponent = self.decimal_suffix(env)?;

                if number.radix != Some(Radix::Decimal) && number.radix != None {
                    return Err(LexerError::DecimalRadix);
                }

                Ok(NumberString::Decimal(num.into(), exponent))
            }
            Some('.') => {
                // <digit 10>+ . <digit 10>* #* <suffix>
                self.advance(env);
                num.push('.');

                while let Some(ch) = self.peek(0) {
                    if !Self::digits(Some(Radix::Decimal)).contains(ch) {
                        break;
                    }
                    num.push(ch);
                    self.advance(env);
                }

                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance(env);
                }

                let exponent = self.decimal_suffix(env)?;

                if number.radix != Some(Radix::Decimal) && number.radix != None {
                    return Err(LexerError::DecimalRadix);
                }

                Ok(NumberString::Decimal(num.into(), exponent))
            }
            _ => Ok(NumberString::Integer(num.into())),
        }
    }

    /// Parse a single decimal prefix
    fn decimal_suffix(
        &mut self,
        env: &mut Environment,
    ) -> Result<Option<ExponentKind>, LexerError> {
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
        self.advance(env);

        let mut next = self.advance(env).ok_or(LexerError::NonTerminatedNumber)?;

        if matches!(next, '+' | '-') {
            num.push(next);
            next = self.advance(env).ok_or(LexerError::NonTerminatedNumber)?;
        }

        if !"0123456789".contains(next) {
            return Err(LexerError::InvalidExponential);
        }
        num.push(next);

        while let Some(val) = self.peek(0) {
            if "0123456789".contains(val) {
                num.push(val);
                self.advance(env);
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
        env: &mut Environment,
    ) -> Result<(), LexerError> {
        if let Some(ch) = self.peek(0) {
            if !Self::digits(radix).contains(ch) {
                return Err(LexerError::NonTerminatedNumber);
            }
            num.push(ch);
            self.advance(env);
        } else {
            return Err(LexerError::NonTerminatedNumber);
        }

        while let Some(ch) = self.peek(0) {
            if !Self::digits(radix).contains(ch) {
                break;
            }
            num.push(ch);
            self.advance(env);
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

    /// Parse a single character literal
    fn parse_character(&mut self, next: char, env: &mut Environment) -> Option<Token> {
        if next != '#' || self.peek(0) != Some('\\') {
            return None;
        }

        self.advance(env);

        let mut content = String::new();
        while let Some(char) = self.peek(0) {
            if char.is_ascii_whitespace()
                || ("\"();".contains(char) && !content.is_empty())
                || (env.config().extended_whitespace && char.is_whitespace())
            {
                break;
            } else {
                content.push(char);
                self.advance(env);
            }
        }

        let lower = content.to_lowercase();
        if lower == "space" {
            content.clear();
            content.push(' ')
        }
        if lower == "newline" {
            content.clear();
            content.push('\n')
        }

        Some(
            Token::Character {
                value: content.chars().next().unwrap_or_default(),
                error: if content.len() != 1 {
                    Some(LexerError::BadCharacterLiteral { chars: content })
                } else {
                    None
                },
            }
            .into(),
        )
    }

    // Parse a string literal
    fn parse_string(&mut self, next: char, env: &mut Environment) -> Option<Token> {
        if next != '"' {
            return None;
        }

        // ignore errors in escape sequences until the end of the string is
        // found, means more accurate error recovery
        let mut has_error = false;

        let mut literal = String::new();
        while let Some(char) = self.peek(0) {
            // interpret escape sequences
            if char == '\\' && self.peek_is(1, "\"") {
                literal.push('"');
                self.advance(env);
                self.advance(env);
            } else if char == '\\' && self.peek_is(1, "\\") {
                literal.push('\\');
                self.advance(env);
                self.advance(env);
            } else if char == '\\' {
                has_error = true;
                self.advance(env);
            } else if char == '"' {
                break;
            } else {
                literal.push(char);
                self.advance(env);
            }
        }

        let error = if self.advance(env) != Some('"') {
            Some(LexerError::NonTerminatedString)
        } else if has_error {
            Some(LexerError::StringBackslash)
        } else {
            None
        };

        Some(
            Token::String {
                value: literal,
                error,
            }
            .into(),
        )
    }

    fn parse_symbol(&mut self, next: char, env: &mut Environment) -> Option<Token> {
        let tok = match next {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '\'' => Token::Quote,
            '`' => Token::BackQuote,
            '.' => Token::Dot,

            '#' if self.peek(0) == Some('(') => {
                self.advance(env);
                Token::VecStart
            }
            ',' if self.peek(0) == Some('@') => {
                self.advance(env);
                Token::CommaAt
            }
            ',' => Token::Comma,

            _ => return None,
        };

        Some(tok.into())
    }

    // skips whitespace and comments that are not part of a token
    fn skip_whitespace(&mut self, env: &mut Environment) {
        loop {
            match self.peek(0) {
                // default whitespace
                Some(' ') | Some('\n') => {
                    self.advance(env);
                }
                // all whitespace
                Some(c) if env.config().extended_whitespace && c.is_whitespace() => {
                    self.advance(env);
                }
                // comments
                Some(';') => {
                    self.advance(env);
                    while self.peek(0) != Some('\n') {
                        self.advance(env);
                    }
                }
                _ => return,
            }
        }
    }

    /// is a character a valid letter for the start of an identifier
    fn is_initial(&self, c: char, env: &mut Environment) -> bool {
        let res = if env.config().unicode_identifiers {
            c.is_alphabetic()
        } else {
            c.is_ascii_alphabetic()
        };

        res || "!$%&*/:<=>?^_~".contains(c)
    }

    /// is a character a valid delimiter between tokens
    fn is_delimiter(&self, val: Option<char>, env: &mut Environment) -> bool {
        if let Some(val) = val {
            matches!(val, '(' | ')' | '"' | ';' | ' ' | '\n')
                || (env.config().extended_whitespace && val.is_whitespace())
        } else {
            true
        }
    }

    /// returns if the count 'th peeked character is in val
    fn peek_is(&self, count: usize, val: &str) -> bool {
        if let Some(peek) = self.peek(count) {
            val.contains(peek)
        } else {
            false
        }
    }

    /// Consume and return one character from the input
    fn advance(&mut self, env: &mut Environment) -> Option<char> {
        let res = self.peek(0);

        let is_unicode_eol = env.config().extended_whitespace
            && res
                .map(|c| "\u{B}\u{C}\r\u{85}\u{2028}\u{2029}".contains(c))
                .unwrap_or(false);

        // check for crlf being a single line terminator, not one so the line numbering
        // is extended to include the \n, not register it as a new line
        if env.config().extended_whitespace
            && res == Some('\n')
            && self
                .current
                .checked_sub(1)
                .and_then(|n| self.source.get(n))
                .copied()
                == Some('\r')
        {
            self.line_numbering.last_mut().map(|f| f.end += 1);
        } else if is_unicode_eol || res == Some('\n') {
            let start = if let Some(prev) = self.line_numbering.last() {
                prev.end
            } else {
                1
            };

            // current is a consumed character, the ranges exclude the last character
            let end = self.current + 1;

            self.line_numbering.push(start..end);
        }

        // don't change positions if at EOF
        if res.is_some() {
            self.current += 1;
        }

        res
    }

    /// Try to get the next character from the input without consuming it.
    /// count is the number of characters ahead to look, if count == 0 => peek
    /// count == 1 => peekNext
    fn peek(&self, count: usize) -> Option<char> {
        self.source.get(self.current + count).copied()
    }

    pub fn file_idx(&self) -> usize {
        self.file_idx
    }

    pub fn line_numbering(&self) -> &[Range<usize>] {
        &self.line_numbering
    }
}
