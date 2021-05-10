use lasso::{Capacity, Rodeo};

use crate::{memory::Heap, parser::Parser, run::RuntimeConfig};

#[derive(Debug)]
pub struct Environment {
    config: RuntimeConfig,
    files: Vec<(String, String)>,

    symbols: Rodeo,
    errors: Vec<anyhow::Error>,
    heap: Heap,
}

// Environment kinds
// - null => only keywords (quote, lambda, if, set!, begin, cond, and, or, case
//                          let, let*, letrec, do, delay, quasiquote, else, =>,
//                          define, unquote, unquote-splicing)
// - scheme-report-environment 5 = r5rs => null + standard library
// - interaction-environment => r5rs + sfri's + user supplied extra procedures

impl Environment {
    pub fn null(config: impl Into<RuntimeConfig>) -> Self {
        Self {
            files: Vec::with_capacity(0),
            config: config.into(),
            symbols: Rodeo::with_capacity(Capacity::minimal()),
            errors: Vec::with_capacity(0),
            heap: Heap::new(),
        }
    }

    pub fn add_file(&mut self, name: String, content: String) -> usize {
        self.files.push((name, content));
        self.files.len() - 1
    }

    pub fn run(&mut self, _main_file: usize) {
        for file_idx in 0..self.files.len() {
            let mut parser = Parser::new(file_idx, self);

            println!("{:#}", parser.parse());
        }
    }

    pub fn config(&self) -> &RuntimeConfig {
        &self.config
    }

    pub fn files(&self) -> &[(String, String)] {
        &self.files
    }

    pub fn symbols(&mut self) -> &mut Rodeo {
        &mut self.symbols
    }

    pub fn errors(&self) -> &[anyhow::Error] {
        &self.errors
    }

    pub fn emit_error(&mut self, err: impl Into<anyhow::Error>) {
        self.errors.push(err.into());
    }
}
