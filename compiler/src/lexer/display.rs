use std::{cell::RefCell, fmt};

use crate::environment::Environment;

use super::{Lexer, Token};

impl fmt::Debug for Token {
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

/// Wrapper to consume and display all tokens from a lexer
pub struct LexerDisplay<'a> {
    lexer: RefCell<Lexer>,
    env: RefCell<&'a mut Environment>,
}

impl<'a> LexerDisplay<'a> {
    pub fn new(lexer: Lexer, env: &'a mut Environment) -> Self {
        Self {
            lexer: RefCell::new(lexer),
            env: RefCell::new(env),
        }
    }
}

impl<'a> fmt::Display for LexerDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut lexer = self.lexer.borrow_mut();
        let mut env = self.env.borrow_mut();

        let mut tokens = vec![];

        loop {
            let tok = lexer.get_token_loc(&mut env);

            if tok.content() == &Token::Eof {
                break;
            }

            tokens.push(tok);
        }

        env.file_mut(lexer.file_idx())
            .set_line_numbering(lexer.line_numbering());

        for tok in tokens {
            let (line, col) = env
                .file(lexer.file_idx())
                .line_col(tok.extract().start_offset);

            writeln!(
                f,
                "{}:{}-{} | {}",
                line,
                col,
                col + tok.extract().length,
                TokenDisplay::new(tok.content(), &env)
            )?;
        }

        for err in env.errors() {
            writeln!(f, "error: {}", err)?;
        }

        for warn in env.warnings() {
            writeln!(f, "warn: {}", warn)?;
        }

        Ok(())
    }
}

pub struct TokenDisplay<'a> {
    tok: &'a Token,
    env: &'a Environment,
}

impl<'a> TokenDisplay<'a> {
    pub fn new(tok: &'a Token, env: &'a Environment) -> Self {
        Self { tok, env }
    }
}

impl<'a> fmt::Display for TokenDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Token::Identifier { value, ref error } = self.tok {
            let symbol = self.env.symbols().resolve(&value);

            write!(f, "Identifier {:?}", symbol)?;
            if let Some(error) = error {
                write!(f, " with error `{}`", error)?;
            }
            Ok(())
        } else {
            write!(f, "{:?}", self.tok)
        }
    }
}
