use std::{cmp::Ordering, ops::Range};

use crate::{
    config::{ConfigurationCategory, Flag},
    environment::Environment,
    lexer::WithLocation,
    unicode,
};

use super::{Lexer, LexerError, Token};

const SPECIAL_INITIAL: &str = "!$%&*/:<=>?@^_~";
const SPECIAL_SUBSEQUENT: &str = "+-.@";

impl Lexer {
    /// Parse a new identifier
    pub fn parse_identifier(&mut self, env: &mut Environment) -> Option<Token> {
        if self.peek_is(0, "|") {
            todo!()
        }

        let identifier = self.peek_until_delimiter();

        // if this fails no identifier was parsed, it went straight into a delimiter
        let first = identifier.chars().next()?;

        if first.is_ascii_alphabetic()
            || SPECIAL_INITIAL.contains(first)
            || unicode_range(unicode::ID_START, first)
        {
            self.normal_identifier(identifier, env)
        } else if "+-.".contains(first) {
            self.peculiar_identifier(identifier, env)
        } else {
            None
        }
    }

    /// Parse a regular identifier
    ///〈identifier〉−→〈initial〉〈subsequent〉*
    ///〈initial〉−→〈letter〉|〈special initial〉
    ///〈subsequent〉−→〈initial〉|〈digit〉|〈special subsequent〉
    fn normal_identifier(&mut self, identifier: String, env: &mut Environment) -> Option<Token> {
        let error = self.check_subsequent(&identifier, 0, env);

        self.advance_n(identifier.len());

        let ident = env.symbols_mut().get_or_intern(identifier);

        Some(Token::Identifier {
            value: ident,
            error,
        })
    }

    fn peculiar_identifier(&mut self, identifier: String, env: &mut Environment) -> Option<Token> {
        // check for exceptions to the peculiar identifier rule
        if identifier.is_ascii() {
            match &identifier.to_ascii_lowercase()[..] {
                "+i" => todo!(),
                "-i" => todo!(),
                "+inf.0" => todo!(),
                "-inf.0" => todo!(),
                "+nan.0" => todo!(),
                "-nan.0" => todo!(),
                _ => (),
            }
        }

        //〈peculiar identifier〉−→〈explicit sign〉
        if identifier.len() == 1 && matches!(identifier.chars().next(), Some('+' | '-')) {
            self.advance_n(identifier.len());
            let ident = env.symbols_mut().get_or_intern(identifier);

            return Some(Token::Identifier {
                value: ident,
                error: None,
            });
        } else if identifier.len() == 1 {
            return None;
        }

        // identifier will always have a length >= 1 when this is called, as
        // length == 1 has been checked, below here length is always >= 2
        let mut iter = identifier.chars();
        let first = iter.next()?;
        let second = iter.next()?;

        //〈peculiar identifier〉−→〈explicit sign〉〈sign subsequent〉〈subsequent〉*
        //〈peculiar identifier〉−→ .〈dot subsequent〉〈subsequent〉*
        if ("+-".contains(first)
            && (second.is_ascii_alphabetic()
                || SPECIAL_INITIAL.contains(second)
                || "+-@".contains(second)))
            || (first == '.'
                && (second.is_ascii_alphabetic()
                    || SPECIAL_INITIAL.contains(second)
                    || "+-@.".contains(second)))
        {
            let error = self.check_subsequent(&identifier[2..], 2, env);

            self.advance_n(identifier.len());

            let ident = env.symbols_mut().get_or_intern(identifier);

            return Some(Token::Identifier {
                value: ident,
                error,
            });
        } else if identifier.len() == 2 {
            return None;
        }

        // length == 1 or 2 has been checked, below here length is always >= 3
        let third = iter.next()?;

        //〈peculiar identifier〉−→〈explicit sign〉.〈dot subsequent〉〈subsequent〉*
        if "+-".contains(first)
            && second == '.'
            && (third.is_ascii_alphabetic()
                || SPECIAL_INITIAL.contains(third)
                || "+-@.".contains(third))
        {
            let error = self.check_subsequent(&identifier[3..], 3, env);

            self.advance_n(identifier.len());

            let ident = env.symbols_mut().get_or_intern(identifier);

            return Some(Token::Identifier {
                value: ident,
                error,
            });
        }

        None
    }

    /// check if all characters in a string are valid 〈subsequent〉
    fn check_subsequent(
        &self,
        identifier: &str,
        position_offset: usize,
        env: &mut Environment,
    ) -> Option<LexerError> {
        let mut error = None;

        for (idx, ch) in identifier.chars().enumerate() {
            if error == None
                && !(ch.is_ascii_alphabetic()
                    || ch.is_ascii_digit()
                    || SPECIAL_INITIAL.contains(ch)
                    || SPECIAL_SUBSEQUENT.contains(ch)
                    || ch == '\u{200C}' // the zero-width non-joiner
                    || ch == '\u{200D}' // the zero-width joiner
                    || unicode_range(unicode::ID_CONTINUE, ch))
            {
                error = Some(LexerError::InvalidIdentifier {
                    character: WithLocation {
                        file: self.file_idx,
                        length: 1,
                        start_offset: self.start + idx + position_offset,
                        content: ch,
                    },
                });
            }
        }

        if error == None && !identifier.is_ascii() {
            for (idx, ch) in identifier.chars().enumerate() {
                if !ch.is_ascii() {
                    env.emit_warning(
                        W_UNICODE_IDENTIFIERS,
                        LexerError::UnicodeIdentifier {
                            character: WithLocation {
                                file: self.file_idx,
                                length: 1,
                                start_offset: self.start + idx + position_offset,
                                content: ch,
                            },
                        },
                    );
                    return None;
                }
            }
        }

        error
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
}

/// lookup if character is in a table
fn unicode_range(data: &[Range<char>], ch: char) -> bool {
    data.binary_search_by(|range| {
        if range.contains(&ch) {
            Ordering::Equal
        } else if ch < range.start {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    })
    .is_ok()
}

pub static W_UNICODE_IDENTIFIERS: Flag =
    Flag::new(ConfigurationCategory::Warning, "unicode_identifiers")
        .warning(crate::config::WarningLevel::Allow)
        .help("Should identifiers containing non ascii unicode characters be allowed");
