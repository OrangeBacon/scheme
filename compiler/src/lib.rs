pub mod bytecode;
pub mod config;
pub mod environment;
pub mod ir;
pub mod lexer;
pub mod memory;
pub mod numerics;
pub mod parser;
pub mod run;
pub mod value;
pub mod vm;

mod unicode;

pub mod flags {
    pub use crate::environment::W_ERROR;
    pub use crate::lexer::W_UNICODE_IDENTIFIERS;
}
