use std::{
    cell::RefCell,
    fmt::{self, Display},
};

use lasso::{Key, Spur};
use thiserror::Error;

use crate::{
    environment::{Environment, File},
    lexer::{Lexer, LexerError, Token, WithLocation},
    value::{Value, ValuePrinter},
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected Token: {token:?}")]
    UnexpectedToken { token: WithLocation<Token> },

    #[error("Invalid token: {err:?}")]
    InvalidToken { err: WithLocation<LexerError> },

    #[error("Expected token: {expect:?}, got {got:?}")]
    ExpectedToken {
        expect: Token,
        got: WithLocation<Token>,
    },

    #[error("Expected expression but couldn't parse one, instead found {got:?}")]
    ExpectedExpression { got: Token },
}

/// Top level of a scheme file
#[derive(Debug)]
pub struct Program {
    file_idx: usize,
    content: Vec<WithLocation<Datum>>,
}

impl Program {
    pub fn contents(&self) -> &[WithLocation<Datum>] {
        &self.content
    }
}

#[derive(Debug)]
pub enum Datum {
    Boolean(bool),
    Number(Value),
    Character(char),
    String(String),
    Symbol(Spur),
    List {
        values: Vec<WithLocation<Datum>>,
        dot: Option<(WithLocation<()>, Box<WithLocation<Datum>>)>,
    },
    Vector(Vec<WithLocation<Datum>>),
    Error(anyhow::Error),
    LexerError(Token),
}

impl fmt::Display for Datum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

/// State required to parse a program
pub struct Parser<'a> {
    /// The lexer for the file being parsed
    lexer: Lexer,

    /// The next token available, will be consumed by advance and read by peek
    peek: WithLocation<Token>,

    /// The environment used for parsing
    env: &'a mut Environment,
}

impl<'a> Parser<'a> {
    /// Create a new parser from a lexer
    pub fn new(file_idx: usize, env: &'a mut Environment) -> Self {
        let mut lexer = Lexer::new(file_idx, env);

        let peek = Self::advance_lexer(&mut lexer, env);
        Parser { lexer, peek, env }
    }

    /// Parses a whole source code file
    pub fn parse(&mut self) -> Program {
        let mut content = vec![];

        while !self.peek().matches(&Token::Eof) {
            content.push(self.parse_datum());
        }

        self.env
            .file_mut(self.lexer.file_idx())
            .set_line_numbering(self.lexer.line_numbering());

        Program {
            content,
            file_idx: self.lexer.file_idx(),
        }
    }

    /// Parses a single datum.
    /// Any valid expression will parse successfully as a datum, so this is
    /// the expression parser, however not every datum is a valid expression
    /// If unable to parse a datum, returns an error datum showing the error
    /// kind and location.  Might emit an error to the environment in some cases
    /// when a datum can still be constructed, but is wrong in some way
    fn parse_datum(&mut self) -> WithLocation<Datum> {
        let (tok, loc) = self.advance().split();

        match tok {
            Token::Number { error: Some(_), .. }
            | Token::Character { error: Some(_), .. }
            | Token::String { error: Some(_), .. }
            | Token::Identifier { error: Some(_), .. } => {
                WithLocation::join(Datum::LexerError(tok), &loc)
            }

            Token::Boolean { value } => WithLocation::join(Datum::Boolean(value), &loc),
            Token::Number { value, .. } => {
                let value = value.into_value(self.env.heap_mut());
                let datum = match value {
                    Ok(value) => Datum::Number(value),
                    Err(err) => Datum::Error(err.into()),
                };

                WithLocation::join(datum, &loc)
            }
            Token::Character { value, .. } => WithLocation::join(Datum::Character(value), &loc),
            Token::String { value, .. } => WithLocation::join(Datum::String(value), &loc),
            Token::Identifier { value, .. } => WithLocation::join(Datum::Symbol(value), &loc),

            Token::VecStart => self.parse_vector(loc),

            Token::DatumComment => {
                let _ = self.parse_datum();
                self.parse_datum()
            }

            tok @ (Token::Quote | Token::BackQuote | Token::Comma | Token::CommaAt) => {
                self.parse_abbreviation(loc, tok)
            }

            Token::LeftParen => self.parse_list(loc),

            Token::RightParen | Token::Dot | Token::Eof => WithLocation::join(
                Datum::Error(ParseError::ExpectedExpression { got: tok }.into()),
                &loc,
            ),
            Token::Error { .. } => WithLocation::join(Datum::LexerError(tok), &loc),
        }
    }

    /// Parses a vector #( <datum>* )
    fn parse_vector(&mut self, start_loc: WithLocation<()>) -> WithLocation<Datum> {
        let mut elements = vec![];
        loop {
            if self.peek().matches(&[Token::RightParen, Token::Eof]) {
                break;
            }

            elements.push(self.parse_datum());
        }

        let tok = self.advance();

        if !tok.matches(&Token::RightParen) {
            self.env.emit_error(ParseError::ExpectedToken {
                expect: Token::RightParen,
                got: tok.clone(),
            });
        }

        let (_, loc) = tok.split();
        let loc = start_loc.extend(&loc);

        WithLocation::join(Datum::Vector(elements), &loc)
    }

    /// Parse an abbreviation, ('|`|,|,@) <datum>
    fn parse_abbreviation(
        &mut self,
        start_loc: WithLocation<()>,
        start: Token,
    ) -> WithLocation<Datum> {
        let prefix = match start {
            Token::Quote => self.env.symbols_mut().get_or_intern("quote"),
            Token::BackQuote => self.env.symbols_mut().get_or_intern("quasiquote"),
            Token::Comma => self.env.symbols_mut().get_or_intern("unquote"),
            Token::CommaAt => self.env.symbols_mut().get_or_intern("unquote-splicing"),
            _ => unreachable!(),
        };

        let content = self.parse_datum();

        let (content, end) = content.split();
        let loc = start_loc.extend(&end);

        WithLocation::join(
            Datum::List {
                values: vec![
                    WithLocation::join(Datum::Symbol(prefix), &start_loc),
                    WithLocation::join(content, &loc),
                ],
                dot: None,
            },
            &loc,
        )
    }

    /// parses lists, syntax: (<datum>*) | (<datum>+ . <datum>)
    fn parse_list(&mut self, start_loc: WithLocation<()>) -> WithLocation<Datum> {
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

            values.push(self.parse_datum());

            started = true;
        }

        let mut tok = self.advance();

        let dot;

        if tok.matches(&Token::Dot) {
            // try to get the value after the dot
            let (_, loc) = tok.split();
            dot = Some((loc, Box::new(self.parse_datum())));

            tok = self.advance();
        } else {
            dot = None
        }

        if !tok.matches(&Token::RightParen) {
            self.env.emit_error(ParseError::ExpectedToken {
                expect: Token::RightParen,
                got: tok.clone(),
            });
        }

        let (_, loc) = tok.split();
        let loc = start_loc.extend(&loc);

        WithLocation::join(Datum::List { values, dot }, &loc)
    }

    /// get the next token without consuming it
    fn peek(&self) -> &WithLocation<Token> {
        &self.peek
    }

    /// consumes and returns the next token
    fn advance(&mut self) -> WithLocation<Token> {
        let new = Self::advance_lexer(&mut self.lexer, self.env);
        std::mem::replace(&mut self.peek, new)
    }

    /// gets the next non-error token from the lexer
    /// reports the errors if relevant
    fn advance_lexer(lexer: &mut Lexer, env: &mut Environment) -> WithLocation<Token> {
        loop {
            let tok = lexer.get_token_loc(env);
            match tok.content() {
                Token::Error { .. } => {
                    if let (Token::Error { error }, loc) = tok.split() {
                        env.emit_error(ParseError::InvalidToken {
                            err: WithLocation::join(error, &loc),
                        });
                    }
                }
                _ => return tok,
            }
        }
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

/// Pretty printer for a program (Datum) in context of its environment
pub struct ProgramPrinter<'a, 'b> {
    program: &'a Program,
    env: &'b Environment,
}

impl<'a, 'b> ProgramPrinter<'a, 'b> {
    /// Construct a new datum pretty printer
    pub fn new(program: &'a Program, env: &'b Environment) -> Self {
        Self { program, env }
    }
}

impl<'a, 'b> Display for ProgramPrinter<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let depth = RefCell::new(vec![]);

        writeln!(f, "file `{}`:", self.env.file(self.program.file_idx).name())?;
        for datum in &self.program.content {
            let printer = DatumPrintWrapper {
                program: self,
                datum,
                depth: &depth,
            };
            write!(f, "{}", printer)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum DepthInfo {
    Continue,
    End,
    None,
}

struct DatumPrintWrapper<'a, 'b, 'c, 'd, 'e> {
    program: &'c ProgramPrinter<'a, 'b>,
    datum: &'d WithLocation<Datum>,
    depth: &'e RefCell<Vec<DepthInfo>>,
}

impl<'a, 'b, 'c, 'd, 'e> DatumPrintWrapper<'a, 'b, 'c, 'd, 'e> {
    fn print_many(
        &self,
        f: &mut fmt::Formatter,
        values: &[WithLocation<Datum>],
        at_end: bool,
    ) -> fmt::Result {
        for (idx, item) in values.iter().enumerate() {
            if at_end && idx == values.len() - 1 {
                let mut depth = self.depth.borrow_mut();
                depth.pop();
                depth.push(DepthInfo::End);
            }

            let printer = DatumPrintWrapper {
                program: self.program,
                datum: item,
                depth: &self.depth,
            };
            write!(f, "{}", printer)?;
        }

        Ok(())
    }
}

impl<'a, 'b, 'c, 'd, 'e> Display for DatumPrintWrapper<'a, 'b, 'c, 'd, 'e> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let file = self.program.env.file(self.program.program.file_idx);

        print_depth(f, &self.depth)?;
        match self.datum.content() {
            Datum::Boolean(val) => {
                write!(f, "boolean ")?;
                if *val {
                    write!(f, "#t")?;
                } else {
                    write!(f, "#f")?;
                }
                print_location(f, self.datum.extract(), file)?;
            }
            Datum::Number(val) => {
                write!(f, "number: `{}`", ValuePrinter::new(*val, self.program.env))?;
                print_location(f, self.datum.extract(), file)?;
            }
            Datum::Character(val) => {
                write!(f, "character {:?}", *val)?;
                print_location(f, self.datum.extract(), file)?;
            }
            Datum::String(val) => {
                write!(f, "string {:?}", val)?;
                print_location(f, self.datum.extract(), file)?;
            }
            Datum::Symbol(val) => {
                write!(
                    f,
                    "symbol {}: {:?}",
                    val.into_usize(),
                    self.program.env.symbols().resolve(val)
                )?;
                print_location(f, self.datum.extract(), file)?;
            }
            Datum::List { values, dot } => {
                write!(f, "list")?;
                if dot.is_some() {
                    write!(f, " with dot expression")?;
                }
                print_location(f, self.datum.extract(), file)?;

                self.depth.borrow_mut().push(DepthInfo::Continue);
                self.print_many(f, values, dot.is_none())?;
                if let Some((_, dot)) = dot {
                    self.print_many(f, std::slice::from_ref(dot), true)?;
                }
                self.depth.borrow_mut().pop();
            }
            Datum::Vector(values) => {
                write!(f, "vector")?;
                print_location(f, self.datum.extract(), file)?;

                self.depth.borrow_mut().push(DepthInfo::Continue);
                self.print_many(f, values, true)?;
                self.depth.borrow_mut().pop();
            }
            Datum::Error(err) => {
                write!(f, "error: {}", err)?;
                print_location(f, self.datum.extract(), file)?;
            }
            Datum::LexerError(err) => {
                write!(f, "{:?}", err)?;
                print_location(f, self.datum.extract(), file)?;
            }
        }
        Ok(())
    }
}

fn print_depth(f: &mut fmt::Formatter, depth_data: &RefCell<Vec<DepthInfo>>) -> fmt::Result {
    if depth_data.borrow().is_empty() {
        return Ok(());
    }

    let end_idx = depth_data.borrow().len() - 1;
    let mut depth_data = depth_data.borrow_mut();

    for (idx, depth) in depth_data.iter_mut().enumerate() {
        if idx == end_idx && *depth == DepthInfo::Continue {
            write!(f, "|- ")?;
            continue;
        }
        match *depth {
            DepthInfo::Continue => write!(f, "|  ")?,
            DepthInfo::End => write!(f, r"\- ")?,
            DepthInfo::None => write!(f, "   ")?,
        }
        if *depth == DepthInfo::End {
            *depth = DepthInfo::None;
        }
    }
    Ok(())
}

fn print_location(f: &mut fmt::Formatter, loc: WithLocation<()>, file: &File) -> fmt::Result {
    let range = loc.source_range();

    let start = file.line_col(range.start);
    let end = file.line_col(range.end);

    write!(f, " {}:{}", start.0, start.1)?;

    if start.0 == end.0 {
        writeln!(f, "-{}", end.1)
    } else {
        writeln!(f, "-{}:{}", end.0, end.1)
    }
}
