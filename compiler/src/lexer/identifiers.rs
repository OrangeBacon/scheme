use std::{cmp::Ordering, iter::FromIterator, ops::Range};

use lasso::Spur;
use unicode_normalization::UnicodeNormalization;

use crate::{
    config::{ConfigurationCategory, Flag, WarningLevel},
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
            return self.escaped_identifier(env);
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

        self.advance_n(identifier.chars().count());

        Some(Token::Identifier {
            value: self.new_ident(&identifier, env),
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
            self.advance_n(identifier.chars().count());
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
        let sign_ident = "+-".contains(first);
        let dot_ident = first == '.';

        // lambda to help avoid unicode_range lookup if not required
        let second_subsequent = || {
            second.is_ascii_alphabetic()
                || SPECIAL_INITIAL.contains(second)
                || second == '\u{200C}' // the zero-width non-joiner
                || second == '\u{200D}' // the zero-width joiner
                || unicode_range(unicode::ID_CONTINUE, second)
        };

        if (sign_ident && ("+-@".contains(second) || second_subsequent()))
            || (dot_ident && ("+-@.".contains(second) || second_subsequent()))
        {
            let error = self.check_subsequent(&identifier[2..], 2, env);

            self.advance_n(identifier.chars().count());

            return Some(Token::Identifier {
                value: self.new_ident(&identifier, env),
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

            self.advance_n(identifier.chars().count());

            return Some(Token::Identifier {
                value: self.new_ident(&identifier, env),
                error,
            });
        }

        None
    }

    /// Parse an escaped identifier (between two `|`)
    fn escaped_identifier(&mut self, env: &mut Environment) -> Option<Token> {
        // skip the starting `|`
        self.advance();

        let mut ident = String::new();

        let mut error = None;

        loop {
            let ch = if let Some(ch) = self.peek(0) {
                ch
            } else {
                break;
            };

            if ch == '|' {
                break;
            } else if ch == '\\' {
                self.advance();
                let escape = match self.consume_escape() {
                    Ok(s) => s,
                    Err(err) => {
                        if error.is_none() {
                            error = Some(err);
                        }
                        continue;
                    }
                };
                ident.push_str(&escape);
            } else {
                self.advance();
                ident.push(ch);
            }
        }

        if self.peek_is(0, "|") {
            self.advance();
        } else if error.is_none() {
            error = Some(LexerError::IdentifierEOF)
        };

        Some(Token::Identifier {
            error,
            value: self.new_ident(&ident, env),
        })
    }

    /// check if all characters in a string are valid 〈subsequent〉
    fn check_subsequent(
        &self,
        identifier: &str,
        position_offset: usize,
        env: &mut Environment,
    ) -> Option<LexerError> {
        let mut error = None;

        // check for characters being in the correct unicode ranges
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

        // check for unicode identifiers if no other error was returned
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

    /// Create a new identifier, handles identifier normalisation
    fn new_ident(&mut self, identifier: &str, env: &mut Environment) -> Spur {
        // identifier normalisation
        let identifier = if self.case_sensitive {
            identifier.nfc().collect::<String>()
        } else {
            identifier
                .chars()
                .map(|ch| match unicode::get_casefold(ch) {
                    Some(ch) => String::from_iter(ch),
                    None => String::from(ch),
                })
                .collect::<String>()
                .nfkc()
                .collect::<String>()
        };

        env.symbols_mut().get_or_intern(identifier)
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

#[cfg_attr(features = "linkme", distributed_slice(FLAGS))]
pub static W_UNICODE_IDENTIFIERS: Flag =
    Flag::new(ConfigurationCategory::Warning, "unicode_identifiers")
        .warning(WarningLevel::Allow)
        .help("Should identifiers containing non ascii unicode characters be allowed");
