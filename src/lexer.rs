use simple_error::SimpleError;

// Listed in reverse order of precedence.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Token {
    True,               // 'T'
    False,              // 'F'
    Identifier(String), // Alphanumeric chars
    Str(String),        // Literal string
    OpenParen,          // '('
    ClosedParen,        // ')'
    Not,                // '!'
    And,                // '&'
    Or,                 // '|'
    Implies,            // '=>'
    Biconditional,      // '<=>'
}

pub fn tokenize(expr: &str) -> Result<Vec<Token>, SimpleError> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut rest: &str = expr;

    while !rest.is_empty() {
        // Ignore whitespace
        if let " " | "\t" | "\n" = &rest[0..1] {
            rest = &rest[1..];
            continue;
        }

        // Do all the single-character operators first
        // If we've found a single-character token, advance.
        if let Some(tok) = match &rest[0..1] {
            "&" => Some(Token::And),
            "|" => Some(Token::Or),
            "!" => Some(Token::Not),
            "(" => Some(Token::OpenParen),
            ")" => Some(Token::ClosedParen),
            _ => None,
        } {
            tokens.push(tok);
            rest = &rest[1..];
            continue;
        }

        if rest.starts_with("=>") {
            rest = &rest[2..];
            tokens.push(Token::Implies);
            continue;
        }

        if rest.starts_with("<=>") {
            rest = &rest[3..];
            tokens.push(Token::Biconditional);
            continue;
        }

        // Scan strings
        if rest.starts_with("\"") {
            let contents: String = rest.chars().skip(1).take_while(|c| *c != '"').collect();
            rest = &rest[contents.len() + 2..];
            tokens.push(Token::Str(contents));
            continue;
        }

        // Finally, look for possible identifiers.
        let ident: String = rest.chars().take_while(|c| c.is_alphanumeric()).collect();

        tokens.push(match ident.as_str() {
            "T" => Token::True,
            "F" => Token::False,
            "" => bail!("unable to tokenize, rest: {}", rest),
            _ => Token::Identifier(String::from(&ident)),
        });

        rest = &rest[ident.len()..]
    }

    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize() {
        let test_str = "What a&|!=><=>T F( )";
        let tokens = tokenize(test_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("What".into()),
                Token::Identifier("a".into()),
                Token::And,
                Token::Or,
                Token::Not,
                Token::Implies,
                Token::Biconditional,
                Token::True,
                Token::False,
                Token::OpenParen,
                Token::ClosedParen,
            ]
        );
    }

    #[test]
    fn test_tokenize_fail() {
        let fail = tokenize("$");
        println!("{:?}", fail);
        match fail {
            Err(_) => (),
            _ => panic!("invalid expression tokenized without error"),
        }
    }

    #[test]
    fn test_ordering() {
        assert!(Token::Or < Token::Implies);
        assert!(Token::And < Token::Or);
        assert!(Token::Identifier("".into()) < Token::And);
    }
}
