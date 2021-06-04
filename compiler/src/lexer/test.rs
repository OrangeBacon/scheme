#![cfg(test)]

use crate::{config::Configuration, environment::Environment};

use super::{Lexer, Token, WithLocation};

fn driver(source: impl Into<String>) -> (Environment, Vec<WithLocation<Token>>) {
    let mut env = Environment::null(Configuration::new());
    let file = env.add_file("test source".to_owned(), source.into());

    let mut lexer = Lexer::new(file, &env);

    let mut tokens = vec![];

    loop {
        let tok = lexer.get_token_loc(&mut env);
        if *tok.content() == Token::Eof {
            break;
        }

        tokens.push(tok);
    }

    (env, tokens)
}

fn identifier_value(source: &str, result: &str) {
    let (env, tokens) = driver(source);

    if tokens.len() != 1 {
        println!("{:?} => {:?}", source, tokens);
    }
    assert_eq!(tokens.len(), 1);
    assert_eq!(
        tokens[0].extract(),
        WithLocation {
            file: 0,
            length: result.len(),
            start_offset: 0,
            content: ()
        }
    );
    if let Token::Identifier { value, error } = tokens[0].content() {
        assert_eq!(env.symbols().resolve(value), result);
        assert!(error.is_none());
    } else {
        panic!("Didn't get identifier from test");
    }
}

#[test]
fn regular_identifiers() {
    const IDENTIFIERS: &[(&str, &str)] = &[
        ("ident", "ident"),
        ("ident\n   ", "ident"),
        ("...", "..."),
        ("+", "+"),
        ("+soup+", "+soup+"),
        ("<=?", "<=?"),
        ("->string", "->string"),
        ("a34kTMNs", "a34kTMNs"),
        ("lambda", "lambda"),
        ("list->vector", "list->vector"),
        ("q", "q"),
        ("V17a", "V17a"),
        (
            "the-word-recursion-has-many-meanings",
            "the-word-recursion-has-many-meanings",
        ),
    ];

    for (source, result) in IDENTIFIERS {
        identifier_value(source, result)
    }
}

#[test]
fn extended_identifiers() {
    const IDENTIFIERS: &[(&str, &str)] = &[
        ("|two words|", "two words"),
        ("|two\\x20;words|", "two words"),
        ("|H\\x65;llo", "Hello"),
        ("|\\x3BB;|", "λ"),
        ("|\\x9;\\x9;|", "\t\t"),
        ("|\\t\\t|", "\t\t"),
        ("||", ""),
    ];

    for (source, result) in IDENTIFIERS {
        identifier_value(source, result)
    }
}
