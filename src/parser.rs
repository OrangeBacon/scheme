use std::{fmt, rc::Rc};

use thiserror::Error;

use crate::{
    lexer::{ErrorToken, Lexer, LexerError, Token, WithLocation},
    numerics::NumericLiteralString,
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected Token: {token}")]
    UnexpectedToken { token: WithLocation<Token> },

    #[error("Invalid token: {err}")]
    InvalidToken { err: WithLocation<LexerError> },

    #[error("Expected token: {expect}, got {got}")]
    ExpectedToken {
        expect: Token,
        got: WithLocation<Token>,
    },

    #[error("Expected expression but couldn't parse one")]
    ExpectedExpression,
}

/// Top level of a scheme file
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Program {
    file_name: Option<Rc<String>>,
    content: Vec<WithLocation<Expression>>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = f.debug_struct("Program");

        if let Some(name) = &self.file_name {
            out.field("file_name", name.as_ref());
        }

        out.field("source", &SourcePrinter(&self.content));

        out.finish()
    }
}

struct SourcePrinter<'a>(&'a Vec<WithLocation<Expression>>);

impl<'a> fmt::Debug for SourcePrinter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = f.debug_list();

        for element in self.0 {
            out.entry(element.content());
        }

        out.finish()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Expression {
    Boolean(bool),
    Number(NumericLiteralString),
    Character(char),
    String(String),
    Symbol(String),
    List {
        values: Vec<WithLocation<Expression>>,
        dot: Option<(WithLocation<()>, Box<Expression>)>,
    },
    Vector(Vec<WithLocation<Expression>>),
    Prefixed {
        prefix: Prefix,
        val: Box<WithLocation<Expression>>,
    },
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Boolean(val) => {
                if *val {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            Expression::Number(num) => write!(f, "{}", num),
            Expression::Character(ch) => match ch {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                ch => write!(f, "#\\{:?}", ch),
            },
            Expression::String(val) => write!(f, "{:?}", val),
            Expression::Symbol(val) => write!(f, "{}", val),
            Expression::List { values, dot } => {
                let mut tuple = f.debug_tuple("");
                for val in values {
                    tuple.field(val.content());
                }

                if let Some((_, val)) = dot {
                    tuple.field(val);
                }

                tuple.finish()
            }
            Expression::Vector(content) => {
                let mut tuple = f.debug_tuple("#");
                for val in content {
                    tuple.field(val.content());
                }

                tuple.finish()
            }
            Expression::Prefixed { prefix, val } => {
                write!(f, "{}{}", prefix, val.content())
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Prefix {
    Quote,
    BackQuote,
    Comma,
    CommaAt,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::Quote => write!(f, "'"),
            Prefix::BackQuote => write!(f, "`"),
            Prefix::Comma => write!(f, ","),
            Prefix::CommaAt => write!(f, ",@"),
        }
    }
}

/// State required to parse a program
pub struct Parser {
    /// The lexer for the file being parsed
    lexer: Lexer,

    /// The next token available, will be consumed by advance and read by peek
    peek: WithLocation<Token>,

    /// All errors encountered during parsing, so they can all be reported at
    /// the end
    errors: Vec<ParseError>,
}

impl Parser {
    /// Create a new parser from a lexer
    pub fn new(mut lexer: Lexer) -> Parser {
        let mut errors = Vec::with_capacity(0);

        let peek = Self::advance_lexer(&mut lexer, &mut errors);
        Parser {
            lexer,
            peek,
            errors,
        }
    }

    /// Parses a whole source code file
    pub fn parse(&mut self) -> Program {
        let mut content = vec![];

        let file_name = self.peek().file_name();

        while !self.peek().matches(&Token::Eof) {
            if let Some(element) = self.parse_datum() {
                content.push(element);
            }
        }

        Program { file_name, content }
    }

    /// Get all the errors thrown during parsing
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    /// Parses a single datum.
    /// Any valid expression will parse successfully as a datum, so this is
    /// the expression parser, however not every datum is a valid expression
    /// If unable to parse a datum, returns None and stores the error in self.
    fn parse_datum(&mut self) -> Option<WithLocation<Expression>> {
        let (tok, loc) = self.advance().split();

        match tok {
            Token::Boolean { value } => Some(WithLocation::join(Expression::Boolean(value), loc)),
            Token::Number { value } => Some(WithLocation::join(Expression::Number(value), loc)),
            Token::Character { value } => {
                Some(WithLocation::join(Expression::Character(value), loc))
            }
            Token::String { value } => Some(WithLocation::join(Expression::String(value), loc)),
            Token::Identifier { value } => Some(WithLocation::join(Expression::Symbol(value), loc)),

            Token::VecStart => Some(self.parse_vector(loc)),

            tok @ (Token::Quote | Token::BackQuote | Token::Comma | Token::CommaAt) => {
                self.parse_abbreviation(loc, tok)
            }

            Token::LeftParen => Some(self.parse_list(loc)),

            _ => {
                self.emit_error(ParseError::UnexpectedToken {
                    token: WithLocation::join(tok, loc),
                });
                None
            }
        }
    }

    /// Parses a vector #( <datum>* )
    fn parse_vector(&mut self, start_loc: WithLocation<()>) -> WithLocation<Expression> {
        let mut elements = vec![];
        loop {
            if self.peek().matches(&[Token::RightParen, Token::Eof]) {
                break;
            }

            if let Some(val) = self.parse_datum() {
                elements.push(val);
            }
        }

        let tok = self.advance();

        if !tok.matches(&Token::RightParen) {
            self.emit_error(ParseError::ExpectedToken {
                expect: Token::RightParen,
                got: tok.clone(),
            });
        }

        let (_, loc) = tok.split();
        let loc = start_loc.extend(&loc);

        WithLocation::join(Expression::Vector(elements), loc)
    }

    /// Parse an abbreviation, ('|`|,|,@) <datum>
    fn parse_abbreviation(
        &mut self,
        start_loc: WithLocation<()>,
        start: Token,
    ) -> Option<WithLocation<Expression>> {
        let prefix = match start {
            Token::Quote => Prefix::Quote,
            Token::BackQuote => Prefix::BackQuote,
            Token::Comma => Prefix::Comma,
            Token::CommaAt => Prefix::CommaAt,
            _ => unreachable!(),
        };

        let content = self.parse_datum();

        if let Some(content) = content {
            let (content, end) = content.split();
            let loc = start_loc.extend(&end);

            Some(WithLocation::join(
                Expression::Prefixed {
                    prefix: prefix,
                    val: Box::new(WithLocation::join(content, end)),
                },
                loc,
            ))
        } else {
            None
        }
    }

    /// parses lists, syntax: (<datum>*) | (<datum>+ . <datum>)
    fn parse_list(&mut self, start_loc: WithLocation<()>) -> WithLocation<Expression> {
        let mut values = vec![];

        let mut started = false;
        loop {
            if self.peek().matches(&[Token::RightParen, Token::Eof]) {
                break;
            }

            // a dot is only valid as an element if it is not first
            if started && self.peek().matches(&Token::Dot) {
                break;
            }

            if let Some(val) = self.parse_datum() {
                values.push(val);
            }

            started = true;
        }

        let mut tok = self.advance();

        let dot;

        if tok.matches(&Token::Dot) {
            // try to get the value after the dot
            dot = if let Some(after) = self.parse_datum() {
                let (value, loc) = after.split();
                Some((loc, Box::new(value)))
            } else {
                self.emit_error(ParseError::ExpectedExpression);
                None
            };

            tok = self.advance();
        } else {
            dot = None
        }

        if !tok.matches(&Token::RightParen) {
            self.emit_error(ParseError::ExpectedToken {
                expect: Token::RightParen,
                got: tok.clone(),
            });
        }

        let (_, loc) = tok.split();
        let loc = start_loc.extend(&loc);

        WithLocation::join(Expression::List { values, dot }, loc)
    }

    /// get the next token without consuming it
    fn peek(&self) -> &WithLocation<Token> {
        &self.peek
    }

    /// consumes and returns the next token
    fn advance(&mut self) -> WithLocation<Token> {
        let new = Self::advance_lexer(&mut self.lexer, &mut self.errors);
        std::mem::replace(&mut self.peek, new)
    }

    /// gets the next non-error token from the lexer
    /// reports the errors if relevant
    fn advance_lexer(lexer: &mut Lexer, errors: &mut Vec<ParseError>) -> WithLocation<Token> {
        loop {
            let tok = lexer.get_token_loc();
            match tok.split() {
                (ErrorToken::Token(t), loc) => return WithLocation::join(t, loc),
                (ErrorToken::Error(err), loc) => errors.push(ParseError::InvalidToken {
                    err: WithLocation::join(err, loc),
                }),
            }
        }
    }

    fn emit_error(&mut self, err: ParseError) {
        self.errors.push(err);
    }
}

/// Trait for things that can be pattern matched against tokens
trait TokenPattern {
    /// Contains only one token
    fn single(&self) -> Option<&Token>;

    /// Contains N tokens
    fn multiple(&self) -> Option<&[Token]>;
}

impl<const LEN: usize> TokenPattern for [Token; LEN] {
    fn single(&self) -> Option<&Token> {
        None
    }

    fn multiple(&self) -> Option<&[Token]> {
        Some(self)
    }
}

impl TokenPattern for Token {
    fn single(&self) -> Option<&Token> {
        Some(self)
    }

    fn multiple(&self) -> Option<&[Token]> {
        None
    }
}

impl WithLocation<Token> {
    /// Do two tokens have the same kind, ignores their contents
    fn matches<T: TokenPattern>(&self, tok: &T) -> bool {
        use std::mem::discriminant;

        if let Some(tok) = tok.single() {
            discriminant(tok) == discriminant(self.content())
        } else if let Some(val) = tok.multiple() {
            val.iter()
                .any(|tok| discriminant(tok) == discriminant(self.content()))
        } else {
            false
        }
    }
}
