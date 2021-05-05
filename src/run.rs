use anyhow::Result;

use crate::lexer::Lexer;

/// A single source file unit description
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct SourceFile {
    pub path: Option<String>,
    pub content: String,
}

/// Global configuration options
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct RuntimeConfig {
    pub strict_mode: bool,
}

pub fn run(sources: Vec<SourceFile>, config: RuntimeConfig) -> Result<()> {
    for source in sources {
        Lexer::new(source, config).lex()?;
    }

    Ok(())
}
