use lasso::Spur;
use thiserror::Error;

use crate::{
    environment::Environment,
    lexer::WithLocation,
    numerics::NumericError,
    parser::{Datum, Program},
    value::Value,
};

#[derive(Debug, Error)]
pub enum IrParseError {
    #[error("Cannot run empty source")]
    EmptySource,

    #[error("Number parsing error: {source}")]
    NumberParse {
        #[from]
        source: NumericError,
    },
}

// A more detailed ir that can be optimised and have codegen performed

#[derive(Debug)]
pub struct Ir {
    content: Expression,
}

#[derive(Debug)]
enum Expression {
    ConstantValue(WithLocation<Value>),
    VariableReference(WithLocation<Spur>),
    Call(WithLocation<Vec<WithLocation<Expression>>>),
    Block(WithLocation<Vec<WithLocation<Expression>>>),
}

pub struct IrBuilder<'a> {
    env: &'a mut Environment,
    locals: Vec<(Spur, usize)>,
    errors: Vec<IrParseError>,
    scope_depth: usize,
}

impl<'a> IrBuilder<'a> {
    /// Create a new Ir from a datum from the parser
    /// requires an environment and a bytecode block for allocation
    /// requires that the datum was created in the same environment
    /// that is passed to this function.
    pub fn build(datum: &Program, env: &'a mut Environment) -> Result<Ir, IrParseError> {
        println!("{:#}", datum);

        if datum.contents().is_empty() {
            return Err(IrParseError::EmptySource);
        }

        let mut builder = Self {
            env,
            locals: vec![],
            errors: vec![],
            scope_depth: 0,
        };

        let mut content = vec![];
        let mut loc = datum.contents()[0].extract();
        for item in datum.contents() {
            loc = loc.extend(item);
            if let Some(val) = builder.expression(item) {
                content.push(val);
            }
        }

        Ok(Ir {
            content: Expression::Block(WithLocation::join(content, &loc)),
        })
    }

    fn expression(&mut self, item: &WithLocation<Datum>) -> Option<WithLocation<Expression>> {
        let loc = item.extract();
        let item = item.content();

        None
    }
}
