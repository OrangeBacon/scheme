use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    environment::Environment,
    numerics::{ComplexNumber, Number},
};

/// Wrapper to allow value to be an opaque type, so value goes through
/// a single allocator, allowing changing allocation strategy.
/// Some methods for manipulating values require a reference to a heap,
/// this must be the heap the value was allocated using
#[derive(Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Value(pub(crate) ValueContents);

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum ValueContents {
    Boolean(bool),
    Character(char),
    String(usize),
    Number(Number),
    Complex(ComplexNumber),
    List(usize),
    Vector(usize),
    NativeFunction(u32),
}

impl fmt::Debug for ValueContents {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueContents::Boolean(val) => {
                if *val {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }

            ValueContents::Character(val) => {
                if *val == ' ' {
                    write!(f, r"#\space")
                } else if *val == '\n' {
                    write!(f, r"#\newline")
                } else {
                    write!(f, r"#\{}", *val)
                }
            }

            ValueContents::Vector(val) => write!(f, "#(vector {})", *val),
            ValueContents::String(val) => write!(f, "{:?}", val),
            ValueContents::Number(val) => write!(f, "{:?}", val),
            ValueContents::Complex(val) => write!(f, "{:?}", val),
            ValueContents::List(val) => write!(f, "(list {})", *val),
            ValueContents::NativeFunction(val) => write!(f, "{{native fn {}}}", val),
        }
    }
}

/// Print out a human readable version of a value, based on interpreting it in
/// an environment.  Takes into account that values can be recursively defined.
pub struct ValuePrinter<'a> {
    value: Value,
    env: &'a Environment,
    vector_ids: Rc<RefCell<Vec<usize>>>,
}

impl<'a> ValuePrinter<'a> {
    pub fn new(value: Value, env: &'a Environment) -> Self {
        Self {
            value,
            env,
            vector_ids: Rc::new(RefCell::new(vec![])),
        }
    }

    fn print_nested(&self, f: &mut fmt::Formatter, id: usize, start: &'static str) -> fmt::Result {
        if self.vector_ids.borrow().contains(&id) {
            write!(f, "`...recursive`")?;
            return Ok(());
        }

        self.vector_ids.borrow_mut().push(id);

        let list = self.env.heap().get_vec(id);
        write!(f, "{}", start)?;
        for (id, value) in list.iter().enumerate() {
            if id != 0 {
                write!(f, " ")?;
            }

            write!(
                f,
                "{}",
                ValuePrinter {
                    value: *value,
                    env: self.env,
                    vector_ids: Rc::clone(&self.vector_ids),
                }
            )?;
        }

        self.vector_ids.borrow_mut().pop();
        write!(f, ")")
    }
}

impl<'a> fmt::Display for ValuePrinter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value.0 {
            ValueContents::Boolean(_)
            | ValueContents::Character(_)
            | ValueContents::NativeFunction(_) => write!(f, "{:?}", self.value.0),

            ValueContents::String(id) => write!(f, "{:?}", self.env.heap().get_string(id)),
            ValueContents::List(id) => self.print_nested(f, id, "("),
            ValueContents::Vector(id) => self.print_nested(f, id, "#("),
            ValueContents::Number(val) => val.print(f, self.env.heap()),
            ValueContents::Complex(val) => val.print(f, self.env.heap()),
        }
    }
}
