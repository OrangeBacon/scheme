mod identifiers;
mod location;
mod numbers;
mod test;

pub use self::location::*;

use std::{fmt, ops::Range};

use lasso::Spur;
use thiserror::Error;

use crate::{environment::Environment, numerics::NumericLiteralString, unicode};

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

    /// Was the last consumed character an end of line character?
    is_eol: bool,
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
            is_eol: false,
        }
    }

    /// Get the next token and its source location
    pub fn get_token_loc(&mut self, env: &mut Environment) -> WithLocation<Token> {
        // need to skip whitespace before recording line and column positions
        // otherwise the whitespace will be included in the token's source
        // location
        self.skip_whitespace();

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
        self.skip_whitespace();
        self.start = self.current;

        if self.peek(0) == None {
            return Token::Eof.into();
        }

        if let Some(tok) = self.parse_identifier(env) {
            return tok;
        }
        if let Some(tok) = self.parse_boolean() {
            return tok;
        }
        if let Some(tok) = self.parse_number() {
            return tok;
        }
        if let Some(tok) = self.parse_character() {
            return tok;
        }
        if let Some(tok) = self.parse_string() {
            return tok;
        }
        if let Some(tok) = self.parse_symbol() {
            return tok;
        }

        Token::Error {
            error: LexerError::UnexpectedChars {
                chars: self.advance().map(|c| c.to_string()).unwrap_or_default(),
            },
        }
    }

    /// Parse a boolean true or false
    fn parse_boolean(&mut self) -> Option<Token> {
        if self.peek_is(0, "#") && self.peek_is(1, "tfTF") {
            self.advance();
            self.advance();
            return Some(
                Token::Boolean {
                    value: self.peek_is(0, "tT"),
                }
                .into(),
            );
        }

        None
    }

    /// Parse a single character literal
    fn parse_character(&mut self) -> Option<Token> {
        if self.peek(0) != Some('#') || self.peek(1) != Some('\\') {
            return None;
        }

        self.advance();
        self.advance();

        let mut content = String::new();
        while let Some(char) = self.peek(0) {
            if self.is_delimiter(Some(char)) {
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
    fn parse_string(&mut self) -> Option<Token> {
        if self.peek(0) != Some('"') {
            return None;
        }
        self.advance();

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

        let error = if self.advance() != Some('"') {
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

    fn parse_symbol(&mut self) -> Option<Token> {
        let peek = match self.peek(0) {
            Some(c) => c,
            None => return None,
        };

        let tok = match peek {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '\'' => Token::Quote,
            '`' => Token::BackQuote,
            '.' => Token::Dot,

            '#' if self.peek(1) == Some('(') => {
                self.advance();
                Token::VecStart
            }
            ',' if self.peek(1) == Some('@') => {
                self.advance();
                Token::CommaAt
            }
            ',' => Token::Comma,

            _ => return None,
        };

        self.advance();

        Some(tok.into())
    }

    // skips whitespace and comments that are not part of a token
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek(0) {
                // whitespace
                Some(c) if Self::is_whitespace(c) => {
                    self.advance();
                }
                // comments
                Some(';') => {
                    self.advance();
                    loop {
                        self.advance();
                        if self.is_eol {
                            break;
                        }
                    }
                }
                _ => return,
            }
        }
    }

    /// is a character a valid letter for the start of an identifier
    fn is_initial(&self, c: char) -> bool {
        c.is_alphabetic() || "!$%&*/:<=>?^_~".contains(c)
    }

    /// is a character a valid delimiter between tokens
    fn is_delimiter(&self, val: Option<char>) -> bool {
        if let Some(val) = val {
            matches!(val, '|' | '(' | ')' | '"' | ';') || Self::is_whitespace(val)
        } else {
            true
        }
    }

    fn is_whitespace(val: char) -> bool {
        unicode::WHITESPACE.contains(&val)
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
        self.is_eol = false;

        let res = self.peek(0);

        let is_eol = res
            .map(|c| "\u{B}\u{C}\r\n\u{85}\u{2028}\u{2029}".contains(c))
            .unwrap_or(false);

        // check for crlf being a single line terminator, not one so the line numbering
        // is extended to include the \n, not register it as a new line
        if is_eol && !(res == Some('\r') && self.peek(1) == Some('\n')) {
            let start = if let Some(prev) = self.line_numbering.last() {
                prev.end
            } else {
                1
            };

            // current is a consumed character, the ranges exclude the last character
            let end = self.current + 1;

            self.line_numbering.push(start..end);
            self.is_eol = true;
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
