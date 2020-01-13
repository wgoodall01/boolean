use super::expr;
use super::expr::Expr;
use super::lexer::Token;
use simple_error::SimpleError;

// Implementation of parser combinators in Rust adapted from
// https://bodil.lol/parser-combinators/

pub fn parse(input: Vec<Token>) -> Result<Expr, SimpleError> {
    match expr(input.as_slice()) {
        Ok((expr, &[])) => Ok(expr),
        Ok((_expr, _rest)) => Err(SimpleError::new("could not fully parse")),
        Err(_rest) => Err(SimpleError::new("failed to parse.")),
    }
}

/// ParseResult is the result of every parse operation.
///   If the input can be successfully parsed, it returs the Output type, along with the rest of
///   the input.
///   If the input cannot be parsed, the entire input is returned.
type ParseResult<'a, Output> = Result<(Output, &'a [Token]), &'a [Token]>;

/// Parser is a trait representing anything that can produce a ParseResult from an input slice
/// of Tokens.
trait Parser<'a, Output> {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output>;
}

/// Implement Parser automatically for any function which satisfies it.
impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a [Token]) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        self(input)
    }
}

/// map() returns a Parser whose successful output is run through `map_fn` before being returned.
fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input: &'a [Token]| {
        parser
            .parse(input)
            .map(|(result, rest)| (map_fn(result), rest))
    }
}

/// pair(P1, P2) returns a Parser which matches the concatenation of P1 and P2, returning (P1's
/// result, P2's result)
fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input: &'a [Token]| -> ParseResult<'a, (R1, R2)> {
        let (result_1, intermediate1) = parser1.parse(input)?;
        let (result_2, rest) = parser2.parse(intermediate1)?;
        Ok(((result_1, result_2), rest))
    }
}

/// infix(P1, P2, P3) returns a Parser matching the concatenation of P1, P2, and P3, returning
/// (R1, R3).
fn infix<'a, P1, P2, P3, R1, R2, R3>(
    parser1: P1,
    parser2: P2,
    parser3: P3,
) -> impl Parser<'a, (R1, R3)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
    P3: Parser<'a, R3>,
{
    pair(left(parser1, parser2), parser3)
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right)| right)
}

fn either<'a, P1, P2, R>(parser1: P1, parser2: P2) -> impl Parser<'a, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    move |input: &'a [Token]| -> ParseResult<'a, R> {
        match parser1.parse(input) {
            ok @ Ok(_) => ok,
            Err(_) => parser2.parse(input),
        }
    }
}

macro_rules! first_of {
        // Base case:
        ($x:expr) => ($x);
        // `$x` followed by at least one `$y,`
        ($x:expr, $($y:expr),+) => (
            // Call `find_min!` on the tail `$y`
            either($x, first_of!($($y),+))
        )
    }

fn literal<'a>(literal: &'a Token) -> impl Parser<'a, ()> {
    move |input: &'a [Token]| -> ParseResult<'a, ()> {
        match input.iter().next() {
            Some(tok) if tok == literal => Ok(((), &input[1..])),
            _ => Err(input),
        }
    }
}

fn literal_true<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    map(literal(&Token::True), |_| expr::t()).parse(input)
}

fn literal_false<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    map(literal(&Token::False), |_| expr::f()).parse(input)
}

fn identifier<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    match input.iter().next() {
        Some(Token::Identifier(name)) => Ok((expr::var(name), &input[1..])),
        _ => Err(input),
    }
}

fn atom<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    let bools = either(literal_true, literal_false);
    let atoms = either(bools, identifier);
    return atoms.parse(input);
}

fn paren<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    left(
        right(literal(&Token::OpenParen), expr),
        literal(&Token::ClosedParen),
    )
    .parse(input)
}

fn negation<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    map(right(literal(&Token::Not), expr), |e| expr::not(e)).parse(input)
}

fn term<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    first_of!(paren, negation, atom).parse(input)
}

fn conjunction<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    let and = map(infix(term, literal(&Token::And), conjunction), |(l, r)| {
        expr::and(l, r)
    });

    either(and, term).parse(input)
}

fn disjunction<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    let or = map(
        infix(conjunction, literal(&Token::Or), disjunction),
        |(l, r)| expr::or(l, r),
    );

    either(or, conjunction).parse(input)
}

fn implication<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    let implies = map(
        infix(disjunction, literal(&Token::Implies), implication),
        |(l, r)| expr::implies(l, r),
    );

    either(implies, disjunction).parse(input)
}

fn biconditional<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    let bicond = map(
        infix(implication, literal(&Token::Biconditional), biconditional),
        |(l, r)| expr::biconditional(l, r),
    );

    either(bicond, implication).parse(input)
}

pub fn expr<'a>(input: &'a [Token]) -> ParseResult<'a, Expr> {
    biconditional(input)
}

#[cfg(test)]
mod test {
    use super::super::lexer::tokenize;
    use super::*;

    #[test]
    fn test_parse_literal_true() {
        assert_eq!(
            Ok((expr::t(), &vec![][..])),
            literal_true(&tokenize("T").unwrap())
        );
    }

    #[test]
    fn test_parse_literal_false() {
        assert_eq!(
            Ok((expr::f(), &vec![][..])),
            literal_false(&tokenize("F").unwrap())
        );
    }

    #[test]
    fn test_parse_identifier() {
        assert_eq!(
            Ok((expr::var("Test".into()), &vec![][..])),
            identifier(&tokenize("Test").unwrap())
        );
    }

    #[test]
    fn test_parse_pair() {
        let input = &tokenize("T F").unwrap();
        let parser = pair(literal_true, literal_false);
        let ((r1, r2), rest) = parser.parse(input).unwrap();
        assert_eq!(r1, expr::t());
        assert_eq!(r2, expr::f());
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_infix() {
        let input = &tokenize("T & F").unwrap();
        let parser = infix(atom, literal(&Token::And), atom);
        let ((r1, r2), rest) = parser.parse(input).unwrap();
        assert_eq!(r1, expr::t());
        assert_eq!(r2, expr::f());
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_either() {
        let input = &tokenize("F").unwrap();
        let parser = either(literal_true, literal_false);
        let (result, rest) = parser.parse(input).unwrap();
        assert_eq!(result, expr::f());
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_atom() {
        let input = &tokenize("F Whatever").unwrap();
        let parser = pair(atom, atom);
        let ((r1, r2), rest) = parser.parse(input).unwrap();
        assert_eq!(r1, expr::f());
        assert_eq!(r2, expr::var("Whatever"));
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_negation() {
        let input = &tokenize("!F").unwrap();
        let (result, rest) = negation(input).unwrap();
        assert_eq!(result, expr::not(expr::f()));
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_paren() {
        let input = &tokenize("(T)").unwrap();
        let (result, rest) = paren(input).unwrap();
        assert_eq!(result, expr::t());
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_expr_paren() {
        let input = &tokenize("(T)").unwrap();
        let (result, rest) = expr(input).unwrap();
        assert_eq!(result, expr::t());
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_and() {
        let input = &tokenize("T & F").unwrap();
        let (result, rest) = conjunction(input).unwrap();
        assert_eq!(result, expr::and(expr::t(), expr::f()));
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_expr_and() {
        let input = &tokenize("T & F").unwrap();
        let (result, rest) = expr(input).unwrap();
        assert_eq!(result, expr::and(expr::t(), expr::f()));
        assert_eq!(rest.len(), 0);
    }

    #[test]
    fn test_parse_and_or() {
        let input = &tokenize("a | b & c").unwrap();
        let (result, rest) = expr(input).unwrap();
        assert_eq!(
            result,
            expr::or(expr::var("a"), expr::and(expr::var("b"), expr::var("c")))
        );
        assert_eq!(rest.len(), 0);
    }
}
