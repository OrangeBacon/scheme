use crate::environment::Environment;

use super::{Lexer, LexerError, Token};

impl Lexer {
    /// Parse a new identifier
    pub fn parse_identifier(&mut self, next: char, env: &mut Environment) -> Option<Token> {
        // peculiar identifiers  '+', '-', '...'
        if next == '+' && self.is_delimiter(self.peek(0)) {
            return Some(
                Token::Identifier {
                    value: env.symbols_mut().get_or_intern_static("+"),
                    error: None,
                }
                .into(),
            );
        }

        if next == '-' && self.is_delimiter(self.peek(0)) {
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
            && self.is_delimiter(self.peek(2))
        {
            self.advance();
            self.advance();
            return Some(
                Token::Identifier {
                    value: env.symbols_mut().get_or_intern_static("..."),
                    error: None,
                }
                .into(),
            );
        }

        // regular identifiers
        if self.is_initial(next) {
            let mut ident = String::from(next);

            while let Some(ch) = self.peek(0) {
                if ch.is_numeric() || self.is_initial(ch) || "+-.@".contains(ch) {
                    self.advance();
                    ident.push(ch);
                } else {
                    break;
                }
            }

            let error = if !self.is_delimiter(self.peek(0)) {
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
}
