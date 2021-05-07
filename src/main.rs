use std::{
    fmt::{self, Display},
    fs,
    io::{self, Read},
};

use anyhow::Result;
use clap::{clap_app, crate_authors, crate_description, crate_version};
use thiserror::Error;

use crate::run::{RuntimeConfig, SourceFile};

mod info;
mod lexer;
mod parser;
mod run;

/// Errors encountered while interpreting the input arguments
#[derive(Debug, Error)]
enum InputError {
    #[error("Encountered errors while reading files:\n{files}")]
    FileError { files: IoErrorVec },

    #[error("No input was provided")]
    NoInput,

    #[error("All sources provided were empty")]
    AllEmpty,
}

/// New type wrapper to provide display impl
#[derive(Debug)]
struct IoErrorVec(Vec<io::Error>);

impl Display for IoErrorVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.0 {
            writeln!(f, "{}", err)?;
        }

        Ok(())
    }
}

fn main() {
    if let Err(e) = run() {
        println!("Error:\n{}", e);
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let matches = clap_app!(scheme =>
        (version: crate_version!())
        (author: crate_authors!())
        (about: crate_description!())
        (@arg input: +multiple "Input files to parse.  If not present uses stdin.")
        (@arg eval: -e --eval "Interpret the input as source code instead of file names")
        (@arg strict: -s --strict r#"Disable strict mode"#)
        (@arg feature: -f --feature +takes_value +multiple "Enable or disable a feature")
        (@subcommand info =>
            (about: "Print internal documentation messages")
            (@arg value: +takes_value +multiple "The documentation message to get"))
    )
    .get_matches();

    // print documentation if requested
    if let Some(matches) = matches.subcommand_matches("info") {
        match matches.values_of("value") {
            Some(values) => {
                // if multiple inputs are entered assume they are words in a
                // space separated string
                info::info(Some(&values.collect::<Vec<_>>().join(" ")));
            }
            None => info::info(None),
        }
        return Ok(());
    }

    let mut sources = vec![];

    // try to work out the meaning of the inputs
    if let Some(file_names) = matches.values_of("input") {
        // the input is source code
        if matches.is_present("eval") {
            sources.push(SourceFile {
                path: None,
                content: file_names.fold(String::new(), |a, b| a + b),
            });
        } else {
            // the input is file names

            // any file read errors
            let mut errors = vec![];

            for file in file_names {
                // file name "-" == read from stdin
                if file == "-" {
                    match get_stdin() {
                        Ok(input) => sources.push(input),
                        Err(err) => errors.push(err),
                    }
                } else {
                    // try to interpret as a source file name
                    match fs::read_to_string(file) {
                        Ok(input) => sources.push(SourceFile {
                            path: Some(file.to_string()),
                            content: input,
                        }),
                        Err(err) => errors.push(err),
                    }
                };
            }

            if !errors.is_empty() {
                return Err(InputError::FileError {
                    files: IoErrorVec(errors),
                }
                .into());
            }
        }
    } else {
        sources.push(get_stdin()?);
    }

    // no input provided, so nothing to do
    if sources.is_empty() {
        return Err(InputError::NoInput.into());
    }

    if sources.iter().all(|s| s.content.is_empty()) {
        return Err(InputError::AllEmpty.into());
    }

    // get the base configuration
    let mut config = if matches.is_present("strict") {
        RuntimeConfig::new_extended()
    } else {
        RuntimeConfig::new_strict()
    };

    if let Some(features) = matches.values_of("feature") {
        for value in features {
            config.set_feature(value)?;
        }
    }

    run::run(sources, config)?;

    Ok(())
}

/// read source code from stdin
fn get_stdin() -> Result<SourceFile, io::Error> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    Ok(SourceFile {
        path: Some("stdin".to_string()),
        content: input,
    })
}
