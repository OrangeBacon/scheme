mod identifiers;
mod location;
mod numbers;

pub use self::location::*;

use std::{fmt, ops::Range};

use lasso::Spur;
use thiserror::Error;

use crate::{environment::Environment, numerics::NumericLiteralString};

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
