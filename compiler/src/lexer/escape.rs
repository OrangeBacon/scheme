//! Escape sequence parsing for both strings and | delimited identifiers

use crate::lexer::WithLocation;

use super::{Lexer, LexerError};

impl Lexer {
    /// Consume an escape sequence for either escaped identifiers
    /// does not consume a backslash before the escape sequence
    pub(super) fn consume_escape(&mut self) -> Result<String, LexerError> {
        let mut ch = self
            .advance()
            .ok_or(LexerError::UnexpectedEOFEscapeSequence)?;

        if ch.is_whitespace() {
            while !self.is_eol && self.peek(0).map(|ch| ch.is_whitespace()).unwrap_or(false) {
                ch = self
                    .advance()
                    .ok_or(LexerError::UnexpectedEOFEscapeSequence)?;
            }

            if !self.is_eol {
                return Err(LexerError::NoWhitespaceEscapeSequenceEndOfLine {
                    character: WithLocation {
                        file: self.file_idx,
                        length: 1,
                        start_offset: self.current,
                        content: ch,
                    },
                });
            }

            while self.peek(0).map(|ch| ch.is_whitespace()).unwrap_or(false) {
                self.advance();
                if self.is_eol {
                    break;
                }
            }
            return Ok(String::new());
        }

        let result = match ch {
            ch @ ('|' | '\\' | '"') => ch,
            'a' => '\u{7}', // alarm
            'b' => '\u{8}', // backspace
            't' => '\t',
            'n' => '\n',
            'r' => '\r',
            'x' => return self.unicode_escape(),
            _ => return Err(LexerError::StringBackslash),
        };

        Ok(String::from(result))
    }

    fn unicode_escape(&mut self) -> Result<String, LexerError> {
        let mut ident = String::new();

        while let Some(ch) = self.peek(0) {
            if "|\";".contains(ch) {
                break;
            }
            ident.push(ch);
            self.advance();
        }

        if self.peek_is(0, ";") {
            self.advance();
        } else {
            let len = ident.chars().count();
            return Err(LexerError::UnicodeEscapeSemicolon {
                loc: WithLocation {
                    file: self.file_idx,
                    length: len,
                    start_offset: self.current - len,
                    content: (),
                },
            });
        }

        let code_point = u32::from_str_radix(&ident, 16).map_err(|error| {
            let len = ident.chars().count();
            LexerError::HexUnicodeEscape {
                error: WithLocation {
                    file: self.file_idx,
                    length: len,
                    start_offset: self.current - len,
                    content: error,
                },
            }
        })?;

        let value = char::from_u32(code_point).ok_or_else(|| {
            let len = ident.chars().count();
            LexerError::InvalidUnicode {
                error: WithLocation {
                    file: self.file_idx,
                    length: len,
                    start_offset: self.current - len,
                    content: (),
                },
            }
        })?;

        Ok(String::from(value))
    }
}
