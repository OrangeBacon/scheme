use std::{borrow::Cow, fmt, rc::Rc};

use anyhow::Result;
use thiserror::Error;

use crate::run::{RuntimeConfig, SourceFile};

#[derive(Debug, Error, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum LexerError {
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
    Identifier { value: String },
    Boolean { value: bool },
    Number { value: NumericLiteral },
    Character { value: char },
    String { value: String },
    Eof,
}

/// Data required for storing a number
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteral {
    radix: Option<Radix>,
    exact: Option<bool>,
    polar_form: bool,
    real: Number,
    imaginary: Number,
}

/// A Single numeric component
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Number {
    Decimal(Cow<'static, str>),
    Integer(Cow<'static, str>),
    Fraction(Cow<'static, str>, Cow<'static, str>),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Decimal(val) => write!(f, "Decimal({})", val),
            Number::Integer(val) => write!(f, "Integer({})", val),
            Number::Fraction(num, denom) => write!(f, "Fraction({} / {})", num, denom),
        }
    }
}

impl Number {
    const ZERO: Number = Number::Integer(Cow::Borrowed("0"));
}

/// Radices (bases) available for numeric literals
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Radix {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::LeftParen => write!(f, "Left paren '('")?,
            Token::RightParen => write!(f, "Right paren ')'")?,
            Token::VecStart => write!(f, "Vec Start '#('")?,
            Token::Quote => write!(f, "Quote '\''")?,
            Token::BackQuote => write!(f, "Back Quote '`'")?,
            Token::Comma => write!(f, "Comma ','")?,
            Token::CommaAt => write!(f, "Comma at ',@'")?,
            Token::Dot => write!(f, "Dot '.'")?,
            Token::Identifier { value } => write!(f, "Identifier {:?}", value)?,
            Token::Boolean { value } => {
                if *value {
                    write!(f, "Boolean #t")?
                } else {
                    write!(f, "Boolean #f")?
                }
            }
            Token::Number {
                value:
                    NumericLiteral {
                        exact,
                        radix,
                        real,
                        imaginary,
                        polar_form,
                    },
            } => {
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
            }
            Token::Character { value } => write!(f, "Character {:?}", value)?,
            Token::String { value } => write!(f, "String {:?}", value)?,
            Token::Eof => write!(f, "EOF")?,
        }

        Ok(())
    }
}

/// Wrapper type to provide errors or tokens
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ErrorToken {
    Token(Token),
    Error(LexerError),
}

impl fmt::Display for ErrorToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorToken::Token(tok) => write!(f, "{}", tok),
            ErrorToken::Error(err) => write!(f, "{}", err),
        }
    }
}

impl From<Token> for ErrorToken {
    fn from(t: Token) -> Self {
        ErrorToken::Token(t)
    }
}

impl From<LexerError> for ErrorToken {
    fn from(e: LexerError) -> Self {
        ErrorToken::Error(e)
    }
}

/// Wrapper providing source location information for a type
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithLocation<T> {
    file_name: Option<Rc<String>>,
    line: usize,
    column: usize,
    length: usize,
    content: T,
}

impl<T: fmt::Display> fmt::Display for WithLocation<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} | {}", self.line, self.column, self.content)
    }
}

/// State used when converting a string into a token list
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lexer {
    /// Location the source was found in.  Rc is used so that locations can contain
    /// the file name without having to clone the whole string or take a lifetime
    /// parameter.
    path: Option<Rc<String>>,

    /// Global configuration options
    config: RuntimeConfig,

    /// The source code being lexed
    source: Vec<char>,

    /// The index of the character at the start of the current token
    start: usize,

    /// The index of the current character being checked
    current: usize,

    /// The line number of current
    line: usize,

    /// The column number of current
    column: usize,
}

impl Lexer {
    /// Create a new lexer
    pub fn new(source: SourceFile, config: RuntimeConfig) -> Self {
        let path = source.path.map(Rc::new);

        Self {
            config,
            path,
            source: source.content.chars().collect(),
            current: 0,
            start: 0,
            line: 1,
            column: 1,
        }
    }

    /// Print all tokens in the source
    pub fn lex(&mut self) -> Result<()> {
        loop {
            let token = self.get_token_loc()?;
            if token.content == ErrorToken::Token(Token::Eof) {
                break;
            }
            println!("{}", token);
        }

        Ok(())
    }

    /// Get the next token and its source location
    fn get_token_loc(&mut self) -> Result<WithLocation<ErrorToken>, LexerError> {
        // need to skip whitespace before recording line and column positions
        // otherwise the whitespace will be included in the token's source
        // location
        self.skip_whitespace();

        let line = self.line;
        let column = self.column;

        let tok = self.get_token()?;

        let length = self.current - self.start;

        Ok(WithLocation {
            file_name: self.get_path(),
            line,
            column,
            length,
            content: tok,
        })
    }

    /// Get the next token from the source
    fn get_token(&mut self) -> Result<ErrorToken, LexerError> {
        self.skip_whitespace();
        self.start = self.current;

        let next = if let Some(next) = self.advance() {
            next
        } else {
            return Ok(Token::Eof.into());
        };

        // all parsers to try in order
        const PARSERS: [fn(&mut Lexer, char) -> Option<ErrorToken>; 6] = [
            Lexer::parse_identifier,
            Lexer::parse_boolean,
            Lexer::parse_number,
            Lexer::parse_character,
            Lexer::parse_string,
            Lexer::parse_symbol,
        ];

        let tok = PARSERS.iter().find_map(|f| f(self, next));

        if let Some(tok) = tok {
            return Ok(tok);
        }

        Err(LexerError::UnexpectedChars {
            chars: next.to_string(),
        })
    }

    /// Parse a new identifier
    fn parse_identifier(&mut self, next: char) -> Option<ErrorToken> {
        // peculiar identifiers  '+', '-', '...'
        if matches!(next, '+' | '-') && self.is_delimiter(self.peek(0)) {
            return Some(
                Token::Identifier {
                    value: next.to_string(),
                }
                .into(),
            );
        }

        if next == '.'
            && self.peek_is(0, ".")
            && self.peek_is(1, ".")
            && self.is_delimiter(self.peek(2))
        {
            self.advance();
            self.advance();
            return Some(
                Token::Identifier {
                    value: "...".to_string(),
                }
                .into(),
            );
        }

        // regular identifiers
        if self.is_initial(next) {
            let mut ident = String::from(next);

            while let Some(ch) = self.peek(0) {
                let test = if self.config.unicode_identifiers {
                    ch.is_numeric()
                } else {
                    ch.is_ascii_digit()
                };

                if test || self.is_initial(ch) || "+-.@".contains(ch) {
                    self.advance();
                    ident.push(ch);
                } else {
                    break;
                }
            }

            if !self.is_delimiter(self.peek(0)) {
                return Some(LexerError::InvalidIdentifier.into());
            }

            return Some(Token::Identifier { value: ident }.into());
        }

        None
    }

    /// Parse a boolean true or false
    fn parse_boolean(&mut self, next: char) -> Option<ErrorToken> {
        if next == '#' && self.peek_is(0, "tf") {
            self.advance();
            return Some(
                Token::Boolean {
                    value: self.peek_is(0, "t"),
                }
                .into(),
            );
        }

        None
    }

    /// Parse any kind of generic numeric literal
    fn parse_number(&mut self, mut next: char) -> Option<ErrorToken> {
        let mut number = NumericLiteral {
            radix: None,
            exact: None,
            polar_form: false,
            real: Number::ZERO,
            imaginary: Number::ZERO,
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

        if next == '#' {
            let mut peek = self.peek(0)?;
            number.radix = match_radix(peek);
            number.exact = match_exactness(peek);

            if number.radix != None || number.exact != None {
                self.advance();
                next = match self.advance() {
                    Some(c) => c,
                    None => return Some(LexerError::NonTerminatedNumber.into()),
                };
                if next == '#' {
                    peek = match self.advance() {
                        Some(c) => c,
                        None => return Some(LexerError::NonTerminatedNumber.into()),
                    };

                    if number.radix == None {
                        number.radix = match_radix(peek);
                    } else {
                        number.exact = match_exactness(peek);
                    }

                    if number.exact == None || number.radix == None {
                        return Some(LexerError::InvalidNumericPrefix.into());
                    }

                    next = match self.advance() {
                        Some(c) => c,
                        None => return Some(LexerError::NonTerminatedNumber.into()),
                    }
                }
            }
        }

        // parse the complex number after the prefix
        if (next == '+' || next == '-') && self.peek_is(0, "iI") {
            // either +i or -i
            self.advance();

            if next == '+' {
                number.imaginary = Number::Integer("1".into());
            } else {
                number.imaginary = Number::Integer("-1".into());
            }

            if !self.is_delimiter(self.peek(0)) {
                return Some(LexerError::InvalidNumericTerminator.into());
            }

            return Some(Token::Number { value: number }.into());
        }

        let first = match self.parse_real(&number, &mut started, Some(next)) {
            Ok(num) => num,
            Err(err) => {
                if started {
                    return Some(err.into());
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
                number.real = first;
                let next = self.advance();
                number.imaginary = match self.parse_real(&number, &mut started, next) {
                    Ok(num) => num,
                    Err(err) => return Some(err.into()),
                };
            }
            Some(val @ ('+' | '-')) => {
                if self.peek_is(1, "iI") {
                    // <real R> [+-] i
                    self.advance();
                    self.advance();
                    number.real = first;
                    if val == '+' {
                        number.imaginary = Number::Integer("1".into());
                    } else {
                        number.imaginary = Number::Integer("-1".into());
                    }
                } else {
                    // <real R> [+-] <ureal R> i
                    number.real = first;
                    let next = self.advance();
                    number.imaginary = match self.parse_real(&number, &mut started, next) {
                        Ok(num) => num,
                        Err(err) => return Some(err.into()),
                    };
                    if !self.peek_is(0, "iI") {
                        return Some(LexerError::NonTerminatedNumber.into());
                    }
                    self.advance();
                }
            }
            Some('i') => {
                // [+-] <ureal R> i
                self.advance();
                number.real = Number::ZERO;
                number.imaginary = first;
            }
            _ => number.real = first,
        }

        if !self.is_delimiter(self.peek(0)) {
            return Some(LexerError::InvalidNumericTerminator.into());
        }

        Some(Token::Number { value: number }.into())
    }

    fn parse_real(
        &mut self,
        number: &NumericLiteral,
        started: &mut bool,
        next: Option<char>,
    ) -> Result<Number, LexerError> {
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
            if number.radix != Some(Radix::Decimal) && number.radix != None {
                return Err(LexerError::DecimalRadix);
            }

            num.push('.');

            self.unsigned_integer(number.radix, &mut num)?;
            *started = true;

            while self.peek_is(0, "#") {
                num.push('#');
                self.advance();
            }

            self.decimal_suffix(&mut num)?;

            return Ok(Number::Decimal(num.into()));
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
                Ok(Number::Fraction(num.into(), part2.into()))
            }
            Some('e' | 's' | 'f' | 'd' | 'l') => {
                // number with exponential suffix
                // does not consume the peeked character as that is
                // performed inside the suffix parsing
                self.decimal_suffix(&mut num)?;
                Ok(Number::Decimal(num.into()))
            }
            Some('#') => {
                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance();
                }

                if !self.peek_is(0, ".") {
                    // found <uinteger R>
                    return Ok(Number::Integer(num.into()));
                }

                // <digit 10>+ #+ . #* <suffix>
                self.advance();
                num.push('.');

                while self.peek_is(0, "#") {
                    num.push('#');
                    self.advance();
                }

                self.decimal_suffix(&mut num)?;

                Ok(Number::Decimal(num.into()))
            }
            Some('.') => {
                // <digit 10>+ . <digit 10>* #* <suffix>
                self.advance();
                num.push('.');

                if number.radix != Some(Radix::Decimal) && number.radix != None {
                    return Err(LexerError::DecimalRadix);
                }

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

                self.decimal_suffix(&mut num)?;

                Ok(Number::Decimal(num.into()))
            }
            _ => Ok(Number::Integer(num.into())),
        }
    }

    /// Parse a single decimal prefix
    fn decimal_suffix(&mut self, num: &mut String) -> Result<(), LexerError> {
        let peek = match self.peek(0) {
            Some(peek) => {
                if !"esfdl".contains(peek) {
                    return Ok(());
                }
                peek
            }
            None => return Ok(()),
        };

        num.push(peek);
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

        Ok(())
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

    /// Parse a single character literal
    fn parse_character(&mut self, next: char) -> Option<ErrorToken> {
        if next != '#' || self.peek(0) != Some('\\') {
            return None;
        }

        self.advance();

        let mut content = String::new();
        while let Some(char) = self.peek(0) {
            if char.is_ascii_whitespace()
                || ("\"();".contains(char) && !content.is_empty())
                || (self.config.extended_whitespace && char.is_whitespace())
            {
                break;
            } else {
                content.push(char);
                self.advance();
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

        if content.len() != 1 {
            return Some(LexerError::BadCharacterLiteral { chars: content }.into());
        }

        Some(
            Token::Character {
                value: content.chars().next().unwrap(),
            }
            .into(),
        )
    }

    // Parse a string literal
    fn parse_string(&mut self, next: char) -> Option<ErrorToken> {
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
                self.advance();
                self.advance();
            } else if char == '\\' && self.peek_is(1, "\\") {
                literal.push('\\');
                self.advance();
                self.advance();
            } else if char == '\\' {
                has_error = true;
                self.advance();
            } else if char == '"' {
                break;
            } else {
                literal.push(char);
                self.advance();
            }
        }

        if self.advance() != Some('"') {
            Some(LexerError::NonTerminatedString.into())
        } else if has_error {
            Some(LexerError::StringBackslash.into())
        } else {
            Some(Token::String { value: literal }.into())
        }
    }

    fn parse_symbol(&mut self, next: char) -> Option<ErrorToken> {
        let tok = match next {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '\'' => Token::Quote,
            '`' => Token::BackQuote,
            '.' => Token::Dot,

            '#' if self.peek(0) == Some('(') => {
                self.advance();
                Token::VecStart
            }
            ',' if self.peek(0) == Some('@') => {
                self.advance();
                Token::CommaAt
            }
            ',' => Token::Comma,

            _ => return None,
        };

        Some(tok.into())
    }

    // skips whitespace and comments that are not part of a token
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek(0) {
                // default whitespace
                Some(' ') | Some('\n') => {
                    self.advance();
                }
                // all whitespace
                Some(c) if self.config.extended_whitespace && c.is_whitespace() => {
                    self.advance();
                }
                // comments
                Some(';') => {
                    self.advance();
                    while self.peek(0) != Some('\n') {
                        self.advance();
                    }
                }
                _ => return,
            }
        }
    }

    /// is a character a valid letter for the start of an identifier
    fn is_initial(&self, c: char) -> bool {
        let res = if self.config.unicode_identifiers {
            c.is_alphabetic()
        } else {
            c.is_ascii_alphabetic()
        };

        res || "!$%&*/:<=>?^_~".contains(c)
    }

    /// is a character a valid delimiter between tokens
    fn is_delimiter(&self, val: Option<char>) -> bool {
        if let Some(val) = val {
            matches!(val, '(' | ')' | '"' | ';' | ' ' | '\n')
                || (self.config.extended_whitespace && val.is_whitespace())
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
    fn advance(&mut self) -> Option<char> {
        let res = self.peek(0);

        // don't change positions if at EOF
        if res.is_some() {
            self.current += 1;
            self.column += 1;

            if res == Some('\n') {
                self.line += 1;
                self.column = 1;
            }
        }

        res
    }

    /// Try to get the next character from the input without consuming it.
    /// count is the number of characters ahead to look, if count == 0 => peek
    /// count == 1 => peekNext
    fn peek(&self, count: usize) -> Option<char> {
        self.source.get(self.current + count).copied()
    }

    /// Convenience method to clone the path rc if required
    fn get_path(&self) -> Option<Rc<String>> {
        self.path.as_ref().map(|str| Rc::clone(str))
    }
}
