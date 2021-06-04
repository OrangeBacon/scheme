use anyhow::Result;
use thiserror::Error;

use crate::{config::Configuration, environment::Environment};

#[derive(Debug, Error)]
enum RunError {}

/// A single source file unit description
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct SourceFile {
    pub path: Option<String>,
    pub content: String,
}

pub fn run(sources: Vec<SourceFile>, config: Configuration) -> Result<()> {
    for source in sources {
        let mut env = Environment::null(config.clone());
        let main = env.add_file(
            source.path.unwrap_or_else(|| String::from("unknown_file")),
            source.content,
        );
        env.run(main);

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
