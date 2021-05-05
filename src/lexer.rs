use std::{fmt, rc::Rc};

use anyhow::Result;
use thiserror::Error;

use crate::run::{RuntimeConfig, SourceFile};

#[derive(Debug, Error)]
enum LexerError {
    #[error("Unexpected characters: {chars:?}")]
    UnexpectedChars { chars: String },
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
    Boolean,
    Number,
    Character,
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
            Token::Boolean => {}
            Token::Number => {}
            Token::Character => {}
            Token::String => (),
            Token::EOF => write!(f, "EOF")?,
        }

        Ok(())
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
            if token.content == Token::EOF {
                break;
            }
            println!("{}", token);
        }

        Ok(())
    }

    /// Get the next token and its source location
    fn get_token_loc(&mut self) -> Result<WithLocation<Token>, LexerError> {
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
    fn get_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        self.start = self.current;

        let next = if let Some(next) = self.advance() {
            next
        } else {
            return Ok(Token::EOF);
        };

        // peculiar identifiers
        if next == '+' || next == '-' {
            return Ok(Token::Identifier {
                value: next.to_string(),
            });
        }

        if next == '.' && self.peek(1) == Some('.') && self.peek(2) == Some('.') {
            return Ok(Token::Identifier {
                value: "...".to_string(),
            });
        }

        Err(LexerError::UnexpectedChars {
            chars: next.to_string(),
        })
    }

    // skips whitespace and comments that are not part of a token
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek(1) {
                Some(' ') | Some('\n') => {
                    self.advance();
                }
                Some(c) if !self.config.strict_mode && c.is_whitespace() => {

                }
                _ => return,
            }
        }
    }

    /// Consume and return one character from the input
    fn advance(&mut self) -> Option<char> {
        let res = self.source.get(self.current).copied();
        self.current += 1;
        self.column += 1;

        if res == Some('\n') {
            self.line += 1;
            self.column = 1;
        }

        res
    }

    /// Try to get the next character from the input without consuming it.
    /// count is the number of characters ahead to look, if count == 0, returns
    /// the current character, 1 => next character, etc.
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
