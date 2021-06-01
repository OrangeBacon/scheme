#![cfg(test)]

use crate::{environment::Environment, run::RuntimeConfig};

use super::{Lexer, Token, WithLocation};

fn driver(source: impl Into<String>) -> (Environment, Vec<WithLocation<Token>>) {
    let mut env = Environment::null(RuntimeConfig::new());
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
    identifier_value("ident", "ident");
    identifier_value("ident\n   ", "ident");
}
