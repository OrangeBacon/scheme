use anyhow::Result;
use lasso::Key;
use thiserror::Error;

use crate::environment::Environment;

#[derive(Debug, Error)]
enum RunError {}

/// A single source file unit description
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct SourceFile {
    pub path: Option<String>,
    pub content: String,
}

/// Global configuration options
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct RuntimeConfig {}

impl RuntimeConfig {
    pub fn new() -> Self {
        Self {}
    }
}

pub fn run(sources: Vec<SourceFile>, config: RuntimeConfig) -> Result<()> {
    for source in sources {
        let mut env = Environment::null(config);
        let main = env.add_file(
            source.path.unwrap_or_else(|| String::from("unknown_file")),
            source.content,
        );
        env.run(main);

        if !env.symbols_mut().is_empty() {
            println!("strings: [");
            for (idx, string) in env.symbols_mut().iter() {
                println!("    {}: {}", idx.into_usize(), string);
            }
            println!("]");
        }

        if !env.errors().is_empty() {
            println!("errors: [");
            for err in env.errors() {
                println!("    {}", err);
            }
            println!("]");
        }
    }

    Ok(())
}
