use super::expr::Expr;
use simple_error::SimpleError;
use std::collections::HashMap;

#[derive(PartialEq, Debug)]
pub struct TruthTable<'a> {
    // The expression we're evaluating
    expr: &'a Expr,

    // The ordered list of symbols we're using to evaluate the expression.
    symbols: Vec<String>,

    // Bits representing the next pattern of values
    next_pattern: u64,

    // The last possible index, plus one.
    max_pattern: u64,
}

impl<'a> TruthTable<'a> {
    pub fn new(expr: &'a Expr, symbols: Vec<String>) -> Result<TruthTable<'a>, SimpleError> {
        if symbols.len() > 63 {
            // Don't want to deal with overflows, the MSB is used to check if we're done.
            bail!("cannot create a truth table with more than 63 variables");
        }

        Ok(TruthTable {
            next_pattern: 0b0,               // start with all false
            max_pattern: 1 << symbols.len(), // 2^len
            expr,
            symbols,
        })
    }
}

impl<'a> Iterator for TruthTable<'a> {
    type Item = (HashMap<String, Expr>, Expr);

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_pattern == self.max_pattern {
            return None;
        }

        // Evaluate the expression here, to see if it can be simplified.
        let mut expr = self.expr.eval();

        // Iterate through the symbols back-to-front, shifting out the LSB of `pattern` each time.
        let mut pattern = self.next_pattern;
        let mut value_map: HashMap<String, Expr> = HashMap::new();
        for symbol in self.symbols.iter().rev() {
            let value = match pattern & 0b1 {
                0b0 => Expr::False,
                0b1 => Expr::True,
                _ => panic!("impossible arithmetic occurred"),
            };

            // Substitute in the value, and set it in the map
            expr = expr.substitute(symbol, &value);
            value_map.insert(symbol.clone(), value);

            // Shift over our pattern 1 bit to the right
            pattern = pattern >> 1
        }

        // Evaluate the modified expression
        expr = expr.eval();

        // Advance the pattern by 1.
        self.next_pattern += 1;

        Some((value_map, expr))
    }
}

#[cfg(test)]
mod test {
    use super::super::expr::*;
    use super::TruthTable;

    #[test]
    fn test_truth_table_and() {
        let expr = and(var("P"), var("Q"));
        let truth_table = TruthTable::new(&expr, vec!["P".into(), "Q".into()]).unwrap();
        let values: Vec<Expr> = truth_table.map(|(_, val)| val).collect();
        assert_eq!(values, vec![f(), f(), f(), t()]);
    }

    #[test]
    fn test_truth_table_implies() {
        let expr = implies(var("P"), var("Q"));
        let values: Vec<Expr> = TruthTable::new(&expr, vec!["P".into(), "Q".into()])
            .unwrap()
            .map(|(_, val)| val)
            .collect();
        assert_eq!(values, vec![t(), t(), f(), t()]);
    }

    #[test]
    fn test_big_truth_table_works() {
        let expr = implies(var("d"), and(var("a"), or(var("b"), var("c"))));
        let mut symbols: Vec<String> = expr.symbols().into_iter().collect();
        symbols.sort();
        let sym_count = symbols.len();
        assert_eq!(symbols, vec!["a", "b", "c", "d"]);

        let result: Vec<Expr> = expr
            .truth_table(symbols)
            .unwrap()
            .map(|(_, val)| val)
            .collect();
        assert_eq!(result.len(), 1 << sym_count);
    }
}
