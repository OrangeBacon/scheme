use std::collections::HashMap;

#[cfg(features = "linkme")]
#[linkme::distributed_slice]
pub(crate) static FLAGS: [Flag] = [..];

#[cfg(not(features = "linkme"))]
pub(crate) static FLAGS: &[Flag] = &[];

/// Global configuration options
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Configuration {
    options: HashMap<Flag, FlagKind>,
}

impl Configuration {
    /// Create the default configuration settings
    pub fn new() -> Self {
        Self {
            options: HashMap::new(),
        }
    }

    /// Get a boolean configuration option
    pub fn bool(&self, value: Flag) -> Option<bool> {
        self.options
            .get(&value)
            .and_then(|kind| match kind {
                FlagKind::Boolean(val) => Some(*val),
                _ => None,
            })
            .or_else(|| match value.default {
                StaticFlagKind::Boolean(default) => Some(default),
                _ => None,
            })
    }

    /// Get a string configuration option
    pub fn string(&self, value: Flag) -> Option<&str> {
        self.options
            .get(&value)
            .and_then(|kind| match kind {
                FlagKind::String(val) => Some(val.as_str()),
                _ => None,
            })
            .or_else(|| match value.default {
                StaticFlagKind::String(default) => Some(default),
                _ => None,
            })
    }

    /// Get a list of string configuration options
    pub fn string_list(&self, value: Flag) -> Option<&[String]> {
        self.options
            .get(&value)
            .and_then(|kind| match kind {
                FlagKind::StringList(val) => Some(val.as_slice()),
                _ => None,
            })
            .or_else(|| Some(&[]))
    }

    /// Gets a warning level flag value
    pub fn warning_level(&self, value: Flag) -> Option<WarningLevel> {
        self.options
            .get(&value)
            .and_then(|kind| match kind {
                FlagKind::WarningLevel(val) => Some(*val),
                _ => None,
            })
            .or_else(|| match value.default {
                StaticFlagKind::WarningLevel(default) => Some(default),
                _ => None,
            })
    }

    /// Sets a flag's boolean value.  If the flag takes a string then it is set
    /// to either "true" or "false" depending on the boolean value.  If it takes
    /// a list of strings, the string value is appended to the list.  If it takes
    /// a single boolean value, that value is set.  This is repeated for all
    /// values referenced by an alias flag.  True and false are mapped to warn
    /// and allow respectively for warning level options.
    pub fn set_bool(&mut self, flag: Flag, value: bool) {
        let string = if value { "true" } else { "false" };
        let warn = if value {
            WarningLevel::Warn
        } else {
            WarningLevel::Allow
        };

        self.generic_setter(flag, string.to_string(), value, warn);
    }

    /// Sets a flag's string value.  If it takes a single string then the old one
    /// is replaced.  If it takes multiple strings, the new one is added to the
    /// list, if it is a boolean option, it is set to true.  This is repeated for
    /// all values referenced by an alias flag.  If the flag is a warning level,
    /// it is set to warn.
    pub fn set_string(&mut self, flag: Flag, value: String) {
        self.generic_setter(flag, value, true, WarningLevel::Warn);
    }

    /// Sets a flag's warning level value.  If it takes a string, then the warning
    /// level is converted to a string, if it takes a boolean true => not-fatal,
    /// false => fatal error, This is repeated for all values referenced by an
    /// alias flag.
    pub fn set_warning_level(&mut self, flag: Flag, value: WarningLevel) {
        let bool = match value {
            WarningLevel::Allow => true,
            WarningLevel::Warn => true,
            WarningLevel::Deny => false,
        };
        self.generic_setter(flag, format!("{:?}", value), bool, value)
    }

    /// Sets the value of a flag to either the string or boolean value passed
    /// depending on the flag's type.  Resolves alias flags to their components.
    /// Should handle the edge case of alias flags recursively referring to each
    /// other and not repeatedly set the values, however to make a flag easier
    /// to understand recursive aliases should be avoided, this is only here
    /// in case of a likely mistake.
    fn generic_setter(&mut self, flag: Flag, string: String, bool: bool, warning: WarningLevel) {
        let mut checked = vec![];
        let mut todo = vec![flag];

        while let Some(flag) = todo.pop() {
            // this flag has already been set in this call of the setter, so
            // do not set it again
            if checked.contains(&flag) {
                continue;
            }

            if let Some(val) = self.options.get_mut(&flag) {
                // the value already has been set before, overwrite it
                match val {
                    FlagKind::Boolean(val) => *val = bool,
                    FlagKind::String(val) => *val = string.clone(),
                    FlagKind::StringList(val) => val.push(string.clone()),
                    FlagKind::WarningLevel(val) => *val = warning,
                }
            } else {
                // The value has not been set, so it should be added
                match flag.default {
                    StaticFlagKind::Alias(flags) => {
                        // do not add an alias, instead make all of its components
                        // still todo
                        for flag in flags {
                            todo.push(**flag);
                        }
                    }
                    StaticFlagKind::String(_) => {
                        self.options.insert(flag, FlagKind::String(string.clone()));
                    }
                    StaticFlagKind::Boolean(_) => {
                        self.options.insert(flag, FlagKind::Boolean(bool));
                    }
                    StaticFlagKind::StringList => {
                        self.options
                            .insert(flag, FlagKind::StringList(vec![string.clone()]));
                    }
                    StaticFlagKind::WarningLevel(_) => {
                        self.options.insert(flag, FlagKind::WarningLevel(warning));
                    }
                };
            }

            checked.push(flag);
        }
    }

    /// Get all of the flags detected currently.  Will include any flags added
    /// to the configuration, regardless of if they are valid or not.
    /// Returns in an unspecified order.  If the "linkme" feature is enabled
    /// All registered options will be returned, otherwise only the ones that
    /// have had a value set.
    pub fn iter(&self) -> impl Iterator<Item = &Flag> {
        FLAGS
            .iter()
            .filter(move |f| !self.options.contains_key(f))
            .chain(self.options.keys())
    }

    /// Attempts to lookup a flag based on its name and category, useful
    /// to convert a string option into something that can be passed to a
    /// setter or getter.  The lookup has the same limitations as the
    /// [`Self::iter`] method, in that if the "linkme" feature is disabled, this
    /// will probably not work as expected.
    pub fn lookup(&self, category: ConfigurationCategory, name: &str) -> Option<Flag> {
        self.iter()
            .find(|flag| flag.category == category && flag.name == name)
            .copied()
    }
}

impl Default for Configuration {
    fn default() -> Self {
        Self::new()
    }
}

/// The type of a configuration option, options with different categories
/// can have the same name, so this is needed to differentiate between them
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum ConfigurationCategory {
    Warning,
}

/// An optional configuration option that can be passed to the language
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Flag {
    category: ConfigurationCategory,
    name: &'static str,
    default: StaticFlagKind,
    help: &'static str,
}

impl Flag {
    /// Create a new configuration option
    pub const fn new(category: ConfigurationCategory, name: &'static str) -> Self {
        Self {
            category,
            name,
            default: StaticFlagKind::Boolean(false),
            help: "",
        }
    }

    /// Set an option to be an alias to other options, treats all options referenced
    /// as boolean values.  To set or unset an alias, use the boolean setters.
    /// It is not possible to get the value of an alias.  Recursive aliases
    /// should be handled correctly but it is advisable to try to not create
    /// them as it will be confusing for a user.
    pub const fn alias(self, flags: &'static [&'static Flag]) -> Self {
        Self {
            default: StaticFlagKind::Alias(flags),
            ..self
        }
    }

    /// Set an option to take a boolean value that has a default value
    pub const fn bool(self, value: bool) -> Self {
        Self {
            default: StaticFlagKind::Boolean(value),
            ..self
        }
    }

    /// Set an option to take a single string value
    pub const fn string(self, value: &'static str) -> Self {
        Self {
            default: StaticFlagKind::String(value),
            ..self
        }
    }

    /// Set an option to take multiple strings
    pub const fn list(self) -> Self {
        Self {
            default: StaticFlagKind::StringList,
            ..self
        }
    }

    /// Set an option to take a warning level
    pub const fn warning(self, value: WarningLevel) -> Self {
        Self {
            default: StaticFlagKind::WarningLevel(value),
            ..self
        }
    }

    /// Set the help text for a configuration option
    pub const fn help(self, help: &'static str) -> Self {
        Self { help, ..self }
    }
}

/// The type of data that can be received in an option
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum StaticFlagKind {
    Alias(&'static [&'static Flag]),
    String(&'static str),
    Boolean(bool),
    WarningLevel(WarningLevel),
    StringList,
}

/// The data stored inside a flag value
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
enum FlagKind {
    String(String),
    Boolean(bool),
    StringList(Vec<String>),
    WarningLevel(WarningLevel),
}

/// The severity of an individual warning message
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum WarningLevel {
    Allow,
    Warn,
    Deny,
}
