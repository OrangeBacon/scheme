use compiler::{
    config::Configuration,
    environment::Environment,
    lexer::{Lexer, LexerDisplay, W_UNICODE_IDENTIFIERS},
};

fn driver(config: Configuration, source: impl Into<String>) -> String {
    let mut env = Environment::null(config);
    let file = env.add_file("test source".to_owned(), source.into());

    let lexer = Lexer::new(file, &env);

    LexerDisplay::new(lexer, &mut env).to_string()
}

const IDENTIFIERS: &str = "ident ... + +soup+ <=? ->string a34kTMNs lambda
list->vector q V17a the-word-recursion-has-many-meanings \u{1f9cb} 🙉🟠🙉";
const RESULT: &str = r##"1:1-6 | Identifier "ident"
1:7-10 | Identifier "..."
1:11-12 | Identifier "+"
1:13-19 | Identifier "+soup+"
1:20-23 | Identifier "<=?"
1:24-32 | Identifier "->string"
1:33-41 | Identifier "a34kTMNs"
1:42-48 | Identifier "lambda"
2:1-13 | Identifier "list->vector"
2:14-15 | Identifier "q"
2:16-20 | Identifier "V17a"
2:21-57 | Identifier "the-word-recursion-has-many-meanings"
2:58-59 | Identifier "🧋"
2:60-63 | Identifier "🙉🟠🙉"
"##;

#[test]
fn identifiers() {
    let config = Configuration::new();

    let result = driver(config, IDENTIFIERS);

    assert_eq!(result, RESULT);
}

#[test]
fn deny_unicode() {
    let mut config = Configuration::new();
    config.set_warning_level(W_UNICODE_IDENTIFIERS, compiler::config::WarningLevel::Deny);

    let result = driver(config, IDENTIFIERS);

    let expected = format!(
        "{}{}",
        RESULT,
        r##"error: Non-ascii unicode character contained within identifier: WithLocation { file: 0, length: 1, start_offset: 105, content: '🧋' }
error: Non-ascii unicode character contained within identifier: WithLocation { file: 0, length: 1, start_offset: 107, content: '🙉' }
"##
    );

    assert_eq!(result, expected);
}

#[test]
fn warn_unicode() {
    let mut config = Configuration::new();
    config.set_warning_level(W_UNICODE_IDENTIFIERS, compiler::config::WarningLevel::Warn);

    let result = driver(config, IDENTIFIERS);

    let expected = format!(
        "{}{}",
        RESULT,
        r##"warn: Non-ascii unicode character contained within identifier: WithLocation { file: 0, length: 1, start_offset: 105, content: '🧋' }
warn: Non-ascii unicode character contained within identifier: WithLocation { file: 0, length: 1, start_offset: 107, content: '🙉' }
"##
    );

    assert_eq!(result, expected);
}

#[test]
fn dot_identifiers() {
    let config = Configuration::new();

    let result = driver(config, ". .. ... ....");

    assert_eq!(
        result,
        r#"1:1-2 | dot '.'
1:3-5 | Identifier ".."
1:6-9 | Identifier "..."
1:10-14 | Identifier "...."
"#
    );
}

#[test]
fn casefold() {
    let config = Configuration::new();

    let result = driver(config, "AA aa #!fold-case AA aa #!no-fold-case AA aa");

    assert_eq!(
        result,
        r#"1:1-3 | Identifier "AA"
1:4-6 | Identifier "aa"
1:19-21 | Identifier "aa"
1:22-24 | Identifier "aa"
1:40-42 | Identifier "AA"
1:43-45 | Identifier "aa"
"#
    );
}

#[test]
fn error_directive() {
    let config = Configuration::new();

    let result = driver(config, "#!this-is-an-error");

    assert_eq!(
        result,
        r##"1:1-19 | Unexpected characters: "#!this-is-an-error"
"##
    );
}
