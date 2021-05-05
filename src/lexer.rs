use std::{fmt, rc::Rc};

use anyhow::Result;
use thiserror::Error;

use crate::run::{RuntimeConfig, SourceFile};

#[derive(Debug, Error, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum LexerError {
    #[error("Unexpected characters: {chars:?}")]
    UnexpectedChars { chars: String },

    #[error("Unexpected value in character literal: {chars:?}")]
    BadCharacterLiteral { chars: String },
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
    Number,
    Character { value: char },
    String,
    EOF,
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
            Token::Number => {}
            Token::Character { value } => write!(f, "Character {:?}", value)?,
            Token::String => (),
            Token::EOF => write!(f, "EOF")?,
        }

        Ok(())
    }
}

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
        let path = match source.path {
            Some(s) => Some(Rc::new(s)),
            None => None,
        };

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
            if token.content == ErrorToken::Token(Token::EOF) {
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
            return Ok(Token::EOF.into());
        };

        // all parsers to try in order
        const PARSERS: [fn(&mut Lexer, char) -> Option<ErrorToken>; 3] = [
            Lexer::parse_identifier,
            Lexer::parse_boolean,
            Lexer::parse_character,
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
        if next == '+' || next == '-' {
            return Some(
                Token::Identifier {
                    value: next.to_string(),
                }
                .into(),
            );
        }

        if next == '.' && self.peek(0) == Some('.') && self.peek(1) == Some('.') {
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

            return Some(Token::Identifier { value: ident }.into());
        }

        None
    }

    /// Parse a boolean true or false
    fn parse_boolean(&mut self, next: char) -> Option<ErrorToken> {
        // booleans
        let peek = self.peek(0);
        if next == '#' && (peek == Some('t') || peek == Some('f')) {
            self.advance();
            return Some(
                Token::Boolean {
                    value: peek == Some('t'),
                }
                .into(),
            );
        }

        None
    }

    /// Parse a single character literal
    fn parse_character(&mut self, next: char) -> Option<ErrorToken> {
        if next != '#' || self.peek(0) != Some('\\') {
            return None;
        }

        self.advance();

        let mut content = String::new();
        while let Some(char) = self.peek(0) {
            if char.is_ascii_control() || char == ' ' {
                break;
            } else {
                content.push(char);
                self.advance();
            }
        }

        if !content.is_ascii() {
            return Some(LexerError::UnexpectedChars { chars: content }.into());
        }

        if content == "space" {
            content.clear();
            content.push(' ')
        }
        if content == "newline" {
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
    fn is_initial(&mut self, c: char) -> bool {
        let res = if self.config.unicode_identifiers {
            c.is_alphabetic()
        } else {
            c.is_ascii_alphabetic()
        };

        res || "!$%&*/:<=>?^_\"".contains(c)
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
        match &self.path {
            Some(str) => Some(Rc::clone(str)),
            None => None,
        }
    }
}
