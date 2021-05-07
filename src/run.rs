use anyhow::Result;
use thiserror::Error;

use crate::{lexer::Lexer, parser::Parser};

#[derive(Debug, Error)]
enum RunError {
    #[error("Unknown feature '{s}'")]
    UnknownFeature { s: String },
}

/// A single source file unit description
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct SourceFile {
    pub path: Option<String>,
    pub content: String,
}

/// Global configuration options
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct RuntimeConfig {
    pub extended_whitespace: bool,
    pub unicode_identifiers: bool,
}

impl RuntimeConfig {
    pub fn new_strict() -> Self {
        Self {
            extended_whitespace: false,
            unicode_identifiers: false,
        }
    }

    pub fn new_extended() -> Self {
        Self {
            extended_whitespace: true,
            unicode_identifiers: true,
        }
    }

    pub fn set_feature(&mut self, feature: &str) -> Result<()> {
        let (value, feature) = if let Some(feature) = feature.strip_prefix("no-") {
            (false, feature)
        } else {
            (true, feature)
        };

        match feature {
            "extended-whitespace" => self.extended_whitespace = value,
            "unicode-identifiers" => self.unicode_identifiers = value,
            s => return Err(RunError::UnknownFeature { s: s.to_string() }.into()),
        }

        Ok(())
    }
}

pub fn run(sources: Vec<SourceFile>, config: RuntimeConfig) -> Result<()> {
    for source in sources {
        let mut parser = Parser::new(Lexer::new(source, config));

        println!("{:#}", parser.parse());

        if !parser.errors().is_empty() {
            println!("errors: [");
            for err in parser.errors() {
                println!("    {}", err);
            }
            println!("]");
        }
    }

    Ok(())
}
