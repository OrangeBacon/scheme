use std::collections::HashMap;

use lasso::{Capacity, Rodeo, Spur};

use crate::{ir::IrBuilder, memory::Heap, parser::Parser, run::RuntimeConfig, value::Value};

#[derive(Debug)]
pub struct Environment {
    config: RuntimeConfig,
    files: Vec<(String, String)>,

    symbols: Rodeo,
    errors: Vec<anyhow::Error>,
    heap: Heap,

    globals: Vec<Value>,
    global_names: HashMap<Spur, usize>,
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
            globals: Vec::with_capacity(0),
            global_names: HashMap::with_capacity(0),
        }
    }

    pub fn add_file(&mut self, name: String, content: String) -> usize {
        self.files.push((name, content));
        self.files.len() - 1
    }

    pub fn run(&mut self, _main_file: usize) {
        for file_idx in 0..self.files.len() {
            let mut parser = Parser::new(file_idx, self);
            let datum = parser.parse();
            println!("{:#?}", IrBuilder::build(&datum, self));
        }
    }

    pub fn config(&self) -> &RuntimeConfig {
        &self.config
    }

    pub fn files(&self) -> &[(String, String)] {
        &self.files
    }

    pub fn symbols(&self) -> &Rodeo {
        &self.symbols
    }

    pub fn symbols_mut(&mut self) -> &mut Rodeo {
        &mut self.symbols
    }

    pub fn errors(&self) -> &[anyhow::Error] {
        &self.errors
    }

    pub fn emit_error(&mut self, err: impl Into<anyhow::Error>) {
        self.errors.push(err.into());
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }
}
