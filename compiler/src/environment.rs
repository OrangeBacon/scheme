use std::{collections::HashMap, ops::Range};

use lasso::{Capacity, Rodeo, Spur};

use crate::{
    config::{Configuration, ConfigurationCategory, Flag, WarningLevel},
    ir::IrBuilder,
    memory::Heap,
    parser::Parser,
    value::Value,
};

#[derive(Debug, Clone)]
pub struct File {
    name: String,
    content: String,
    line_numbering: Vec<Range<usize>>,
}

impl File {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn line_numbering(&self) -> &[Range<usize>] {
        &self.line_numbering
    }

    pub(crate) fn set_line_numbering(&mut self, numbers: &[Range<usize>]) {
        self.line_numbering = numbers.to_vec();
    }
}

#[derive(Debug)]
pub struct Environment {
    config: Configuration,
    files: Vec<File>,

    symbols: Rodeo,
    heap: Heap,

    errors: Vec<anyhow::Error>,
    warnings: Vec<anyhow::Error>,

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
    pub fn null(config: impl Into<Configuration>) -> Self {
        Self {
            files: Vec::with_capacity(0),
            config: config.into(),
            symbols: Rodeo::with_capacity(Capacity::minimal()),
            heap: Heap::new(),
            errors: Vec::with_capacity(0),
            warnings: Vec::with_capacity(0),
            globals: Vec::with_capacity(0),
            global_names: HashMap::with_capacity(0),
        }
    }

    pub fn add_file(&mut self, name: String, content: String) -> usize {
        self.files.push(File {
            name,
            content,
            line_numbering: vec![],
        });
        self.files.len() - 1
    }

    pub fn run(&mut self, _main_file: usize) {
        for file_idx in 0..self.files.len() {
            let mut parser = Parser::new(file_idx, self);
            let datum = parser.parse();
            println!("{:#?}", IrBuilder::build(&datum, self));
        }
    }

    pub fn config(&self) -> &Configuration {
        &self.config
    }

    pub fn file(&self, idx: usize) -> &File {
        &self.files[idx]
    }

    pub fn file_mut(&mut self, idx: usize) -> &mut File {
        &mut self.files[idx]
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

    pub fn emit_warning(&mut self, flag: Flag, warning: impl Into<anyhow::Error>) {
        let flag = self
            .config()
            .warning_level(flag)
            .unwrap_or(WarningLevel::Warn);

        let warning = warning.into();

        eprintln!("{:?}: {}", flag, warning);

        if flag == WarningLevel::Allow {
            return;
        }

        if flag == WarningLevel::Deny || self.config().bool(W_ERROR).unwrap_or(false) {
            self.emit_error(warning);
        } else {
            self.warnings.push(warning);
        }
    }

    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }
}

pub static W_ERROR: Flag = Flag::new(ConfigurationCategory::Warning, "error")
    .bool(false)
    .help("Convert all compile time warnings emitted into fatal errors");
