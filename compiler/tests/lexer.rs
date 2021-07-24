use pretty_assertions::assert_eq;

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

#[test]
fn escaped_identifier() {
    let config = Configuration::new();
    let result = driver(
        config,
        r#"|two words| |two\x20;words| |H\x65;llo| |\x3BB;|
        |\x9;\x9;| |\t\t| |||no-space-either-side||hello \
        world| |\a\b\t\n\r| |\xhello;| |\x20| |non terminated identifier"#,
    );

    assert_eq!(
        result,
        r#"1:1-12 | Identifier "two words"
1:13-28 | Identifier "two words"
1:29-40 | Identifier "Hello"
1:41-49 | Identifier "λ"
2:9-19 | Identifier "\t\t"
2:20-26 | Identifier "\t\t"
2:27-29 | Identifier ""
2:29-51 | Identifier "no-space-either-side"
2:51-74 | Identifier "hello world"
3:16-28 | Identifier "\u{7}\u{8}\t\n\r"
3:29-39 | Identifier "" with error `Error parsing hexadecimal unicode escape sequence: WithLocation { file: 0, length: 5, start_offset: 140, content: ParseIntError { kind: InvalidDigit } }`
3:40-46 | Identifier "" with error `Could not find semi-colon after unicode escape sequence starting at WithLocation { file: 0, length: 2, start_offset: 150, content: () }`
3:47-73 | Identifier "non terminated identifier" with error `Non-terminated `|` delimited identifier: expected `|`, found end of file`
"#
    );
}

#[test]
fn comments() {
    let config = Configuration::new();
    let result = driver(
        config,
        r##"#|hi|# hello #| nested #| comment |#|# #; (datum comment)"##,
    );

    assert_eq!(
        result,
        r#"1:8-13 | Identifier "hello"
1:40-42 | datum comment #;
1:43-44 | left paren '('
1:44-49 | Identifier "datum"
1:50-57 | Identifier "comment"
1:57-58 | right paren ')'
"#
    );
}

#[test]
fn booleans() {
    let config = Configuration::new();
    let result = driver(
        config,
        "#t #f #T other #F #true stuff #false #TrUe () #FaLsE",
    );

    assert_eq!(
        result,
        r#"1:1-3 | boolean #t
1:4-6 | boolean #f
1:7-9 | boolean #t
1:10-15 | Identifier "other"
1:16-18 | boolean #f
1:19-24 | boolean #t
1:25-30 | Identifier "stuff"
1:31-37 | boolean #f
1:38-43 | boolean #t
1:44-45 | left paren '('
1:45-46 | right paren ')'
1:47-53 | boolean #f
"#
    );
}

#[test]
fn characters() {
    let config = Configuration::new();
    let result = driver(
        config,
        r#"#\alarm #\a #\backspace #\ backspace #\iota #\Iota #\x03BB"#,
    );

    assert_eq!(
        result,
        r#"1:1-8 | character '\u{7}'
1:9-12 | character 'a'
1:13-24 | character '\u{8}'
1:25-28 | character ' '
1:28-37 | Identifier "backspace"
1:38-44 | character 'ɩ'
1:45-51 | character 'Ɩ'
1:52-59 | character 'λ'
"#
    );
}

#[test]
fn numbers() {
    let config = Configuration::new();
    let result = driver(
        config,
        "1 1.1 #x1ae7 #i5 #e5 #o137 #o9 #i#b010110 #b5
                5e7 #e5e7 #i5e7 #o5e7 #x5e7 10#.# 5f3 3.1@7.8
                3+4i -9-8.2i #x3e-4fFi",
    );

    assert_eq!(
        result,
        r#"1:1-2 | NumericLiteralString { radix: None, exact: None, polar_form: false, real: Some(Integer("1")), imaginary: None }
1:3-6 | NumericLiteralString { radix: None, exact: None, polar_form: false, real: Some(Decimal("1.1", None)), imaginary: None }
1:7-13 | NumericLiteralString { radix: Some(Hexadecimal), exact: None, polar_form: false, real: Some(Integer("ae7")), imaginary: None }
1:14-17 | Unexpected characters: ""
1:18-21 | Unexpected characters: ""
1:22-27 | NumericLiteralString { radix: Some(Octal), exact: None, polar_form: false, real: Some(Integer("37")), imaginary: None }
1:28-31 | Unexpected characters: ""
1:32-42 | NumericLiteralString { radix: Some(Binary), exact: Some(false), polar_form: false, real: Some(Integer("010110")), imaginary: None }
1:43-46 | Unexpected characters: ""
2:17-20 | NumericLiteralString { radix: None, exact: None, polar_form: false, real: Some(Decimal("5", Some("7"))), imaginary: None }
2:21-26 | Unexpected characters: "e7"
2:27-32 | Unexpected characters: "e7"
2:33-38 | Unexpected characters: "e7"
2:39-44 | NumericLiteralString { radix: Some(Hexadecimal), exact: None, polar_form: false, real: Some(Integer("e7")), imaginary: None }
2:45-50 | NumericLiteralString { radix: None, exact: None, polar_form: false, real: Some(Decimal("10#.#", None)), imaginary: None }
2:51-54 | NumericLiteralString { radix: None, exact: None, polar_form: false, real: Some(Decimal("5", Some("3"))), imaginary: None }
2:55-62 | NumericLiteralString { radix: None, exact: None, polar_form: true, real: Some(Decimal("3.1", None)), imaginary: Some(Decimal("7.8", None)) }
3:17-21 | NumericLiteralString { radix: None, exact: None, polar_form: false, real: Some(Integer("3")), imaginary: Some(Integer("+4")) } with error `Non-terminated numeric literal`
3:22-29 | NumericLiteralString { radix: None, exact: None, polar_form: false, real: None, imaginary: Some(Decimal("-98.2", None)) }
3:30-39 | NumericLiteralString { radix: Some(Hexadecimal), exact: None, polar_form: false, real: Some(Integer("e")), imaginary: Some(Integer("-4F")) }
warn: Use of `#` in number literal
warn: Use of deprecated decimal exponent marker
"#
    );
}
