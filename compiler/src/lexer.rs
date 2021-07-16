mod display;
mod escape;
mod identifiers;
mod location;
mod numbers;

pub use self::display::{LexerDisplay, TokenDisplay};
pub use self::identifiers::W_UNICODE_IDENTIFIERS;
pub use self::location::*;

use std::num::ParseIntError;
use std::ops::Range;

use lasso::Spur;
use thiserror::Error;

use crate::{environment::Environment, numerics::NumericLiteralString};

#[derive(Debug, Error, Clone, PartialEq, Eq)]
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

    #[error("Invalid character in identifier: {character:?}")]
    InvalidIdentifier { character: WithLocation<char> },

    #[error("Invalid character in decimal exponential suffix")]
    InvalidExponential,

    #[error("Invalid character following numeric literal")]
    InvalidNumericTerminator,

    #[error("Non-ascii unicode character contained within identifier: {character:?}")]
    UnicodeIdentifier { character: WithLocation<char> },

    #[error("Non-terminated `|` delimited identifier: expected `|`, found end of file")]
    IdentifierEOF,

    #[error("Unexpected end of file while parsing escape sequence")]
    UnexpectedEOFEscapeSequence,

    #[error("Expected whitespace or end of line, got {character:?}")]
    NoWhitespaceEscapeSequenceEndOfLine { character: WithLocation<char> },

    #[error("Error parsing hexadecimal unicode escape sequence: {error:?}")]
    HexUnicodeEscape { error: WithLocation<ParseIntError> },

    #[error("Number invalid unicode code point")]
    InvalidUnicode { error: WithLocation<()> },

    #[error("Could not find semi-colon after unicode escape sequence starting at {loc:?}")]
    UnicodeEscapeSemicolon { loc: WithLocation<()> },
}

/// Individual units of source code
#[derive(Clone, PartialEq, Eq)]
pub enum Token {
    LeftParen,
    RightParen,
    VecStart,
    Quote,
    BackQuote,
    Comma,
    CommaAt,
    Dot,
    DatumComment,
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

    /// Is the lexer in a case sensitive mode? (default = true)
    case_sensitive: bool,
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
            case_sensitive: true,
        }
    }

    /// Get the next token and its source location
    pub fn get_token_loc(&mut self, env: &mut Environment) -> WithLocation<Token> {
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
            return Token::Eof;
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

        // assume that character with an error will be followed by more errors
        // collect them all into one error token
        let unexpected = self.peek_until_delimiter();
        self.advance_n(unexpected.chars().count());

        Token::Error {
            error: LexerError::UnexpectedChars { chars: unexpected },
        }
    }

    /// Parse a boolean true or false
    fn parse_boolean(&mut self) -> Option<Token> {
        if self.peek_is(0, "#") && self.peek_is(1, "tfTF") && self.is_delimiter(self.peek(2)) {
            self.advance();
            let ch = self.advance();
            return Some(Token::Boolean {
                value: ch == Some('t') || ch == Some('T'),
            });
        }

        if self.consume_ascii_insensitive("#true") {
            return Some(Token::Boolean { value: true });
        } else if self.consume_ascii_insensitive("#false") {
            return Some(Token::Boolean { value: false });
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

        let mut content = self.consume_until_delimiter();
        if content.chars().count() == 0 {
            if let Some(next) = self.advance() {
                content.push(next);
            }
        }
        let content = content;

        if content.chars().count() == 1 {
            return Some(Token::Character {
                value: content.chars().next().unwrap_or_default(),
                error: None,
            });
        }

        let lower = content.to_lowercase();
        let named = match lower.as_str() {
            "alarm" => Some('\u{7}'),
            "backspace" => Some('\u{8}'),
            "delete" => Some('\u{7f}'),
            "escape" => Some('\u{1b}'),
            "newline" => Some('\n'),
            "null" => Some('\0'),
            "return" => Some('\r'),
            "space" => Some(' '),
            "tab" => Some('\t'),
            _ => None,
        };

        if let Some(ch) = named {
            return Some(Token::Character {
                value: ch,
                error: None,
            });
        }

        let next = content.chars().next();
        if next == Some('x') || next == Some('X') {
            // hexadecimal unicode character codes
            if let Ok(ch) = u32::from_str_radix(&content[1..], 16) {
                if let Some(ch) = char::from_u32(ch) {
                    return Some(Token::Character {
                        value: ch,
                        error: None,
                    });
                };
            };
        }

        // unicode character names
        let content = content.split(&['-', '_'][..]).collect::<Vec<_>>();
        let content = content.join(" ");

        if let Some(ch) = unicode_names2::character(&content) {
            return Some(Token::Character {
                value: ch,
                error: None,
            });
        }

        let case = if content[0..1] == content[0..1].to_lowercase() {
            "small"
        } else {
            "capital"
        };

        {
            let content = format!("latin {} letter {}", case, content);

            if let Some(ch) = unicode_names2::character(&content) {
                return Some(Token::Character {
                    value: ch,
                    error: None,
                });
            }
        }

        Some(Token::Character {
            value: content.chars().next().unwrap_or_default(),
            error: Some(LexerError::BadCharacterLiteral { chars: content }),
        })
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

        Some(Token::String {
            value: literal,
            error,
        })
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

            '#' if self.peek_is(1, "(") => {
                self.advance();
                Token::VecStart
            }
            '#' if self.peek_is(1, ";") => {
                self.advance();
                Token::DatumComment
            }
            ',' if self.peek_is(1, "@") => {
                self.advance();
                Token::CommaAt
            }
            ',' => Token::Comma,

            _ => return None,
        };

        self.advance();

        Some(tok)
    }

    // skips whitespace and comments that are not part of a token
    fn skip_whitespace(&mut self) {
        loop {
            if self.consume_ascii_insensitive("#!fold-case") {
                self.case_sensitive = false;
            }

            if self.consume_ascii_insensitive("#!no-fold-case") {
                self.case_sensitive = true;
            }

            match self.peek(0) {
                // whitespace
                Some(c) if c.is_whitespace() => {
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
                Some('#') if self.peek_is(1, "|") => self.nested_comment(),
                _ => return,
            }
        }
    }

    fn nested_comment(&mut self) {
        self.advance_n(2);

        let mut count = 0;

        loop {
            if self.peek_is(0, "#") && self.peek_is(1, "|") {
                self.advance_n(2);
                count += 1;
            } else if self.peek_is(0, "|") && self.peek_is(1, "#") {
                self.advance_n(2);
                if count == 0 {
                    break;
                } else {
                    count -= 1;
                }
            } else {
                self.advance();
            }
        }
    }

    /// Try to consume a case insensitive ascii string from the source,
    /// returns true if the string was found.  Assumes that a delimiter is
    /// required after the string finishes, does not consume the delimiter.
    fn consume_ascii_insensitive(&mut self, expected: &str) -> bool {
        // calculate len in for loop to avoid having to iterate chars
        // twice to count unicode code points in expected
        let mut len = 0;
        for (idx, val) in expected.chars().enumerate() {
            let got = match self.peek(idx) {
                Some(ch) => ch,
                None => return false,
            };

            if !got.eq_ignore_ascii_case(&val) {
                return false;
            }
            len += 1;
        }

        let ret = self.is_delimiter(self.peek(len));

        self.advance_n(len);

        ret
    }

    /// Peek the entire contents of what is presumed to be a single token
    /// and return it as a string
    fn peek_until_delimiter(&self) -> String {
        let mut result = String::new();

        let mut i = 0;
        while let Some(ch) = self.peek(i) {
            if self.is_delimiter(Some(ch)) {
                break;
            }
            i += 1;
            result.push(ch);
        }

        result
    }

    /// Consume characters until a delimiter is reached and return as a single
    /// string
    fn consume_until_delimiter(&mut self) -> String {
        let mut result = String::new();

        while let Some(ch) = self.peek(0) {
            if self.is_delimiter(Some(ch)) {
                break;
            }
            result.push(ch);
            self.advance();
        }

        result
    }

    /// is a character a valid delimiter between tokens
    fn is_delimiter(&self, val: Option<char>) -> bool {
        if let Some(val) = val {
            matches!(val, '|' | '(' | ')' | '"' | ';') || val.is_whitespace()
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

    /// advances n characters and discards the result
    fn advance_n(&mut self, count: usize) {
        for _ in 0..count {
            self.advance();
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
                0
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
