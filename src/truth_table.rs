use super::expr;
use super::expr::Expr;
use itertools::Itertools;
use simple_error::SimpleError;
use std::collections::HashMap;
use std::mem;

#[derive(PartialEq, Debug)]
pub struct TruthTable<'a> {
    // The expression we're evaluating
    expr: &'a Expr,

    // The ordered list of symbols we're using to evaluate the expression.
    symbols: Vec<String>,

    // Bits representing the next pattern of values
    min_pattern: usize, // inclusive
    max_pattern: usize, // exclusive
}

impl<'a> TruthTable<'a> {
    pub fn new(expr: &'a Expr, symbols: Vec<String>) -> Result<TruthTable<'a>, SimpleError> {
        let n = symbols.len();

        let max_bits = 8 * mem::size_of::<usize>();
        if n > max_bits - 1 {
            // Don't want to deal with overflows, the MSB is used to check if we're done.
            bail!(
                "cannot create a truth table with more than {} variables",
                max_bits
            );
        }

        Ok(TruthTable {
            min_pattern: 0b0,    // start with all false
            max_pattern: 1 << n, // End with all true
            expr,
            symbols,
        })
    }

    fn produce(&self, index: usize) -> (HashMap<String, bool>, Expr) {
        // Evaluate the expression here, to see if it can be simplified.
        let mut expr = self.expr.eval();

        // Iterate through the symbols back-to-front, shifting out the LSB of `pattern` each time.
        let mut value_map: HashMap<String, bool> = HashMap::new();
        let mut pattern = index;
        for symbol in self.symbols.iter().rev() {
            let value = match pattern & 0b1 {
                0b0 => false,
                0b1 => true,
                _ => panic!("impossible arithmetic occurred"),
            };

            // Substitute in the value, and set it in the map
            expr = expr.substitute(symbol, &value.into());
            value_map.insert(symbol.clone(), value);

            // Shift over our pattern 1 bit to the right
            pattern = pattern >> 1;
        }

        // Evaluate the modified expression
        expr = expr.eval();

        (value_map, expr)
    }
}

pub fn hash_to_expr(params: HashMap<String, bool>) -> Expr {
    params
        .into_iter()
        .map(|(id, val)| match val {
            true => expr::var(&id),
            false => expr::not(expr::var(&id)),
        })
        .fold1(expr::and)
        .unwrap()
}

impl<'a> Iterator for TruthTable<'a> {
    type Item = (HashMap<String, bool>, Expr);

    fn next(&mut self) -> Option<Self::Item> {
        if self.min_pattern == self.max_pattern {
            return None;
        }

        let val = self.produce(self.min_pattern);

        // Advance the pattern by 1.
        self.min_pattern += 1;

        Some(val)
    }
}

impl<'a> DoubleEndedIterator for TruthTable<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.min_pattern == self.max_pattern {
            return None;
        }

        let val = self.produce(self.max_pattern - 1);

        // Move the end back by 1.
        self.max_pattern -= 1;

        Some(val)
    }
}

impl<'a> ExactSizeIterator for TruthTable<'a> {
    fn len(&self) -> usize {
        self.max_pattern - self.min_pattern
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

    #[test]
    fn test_truth_table_double_ended() {
        let expr = implies(var("P"), var("Q"));
        let mut values = TruthTable::new(&expr, vec!["P".into(), "Q".into()])
            .unwrap()
            .map(|(_, val)| val);
        assert_eq!(values.len(), 4);
        assert_eq!(values.next(), Some(t()));
        assert_eq!(values.len(), 3);
        assert_eq!(values.next_back(), Some(t()));
        assert_eq!(values.next(), Some(t()));
        assert_eq!(values.len(), 1);
        assert_eq!(values.next_back(), Some(f()));
        assert_eq!(values.next(), None);
        assert_eq!(values.len(), 0);
        assert_eq!(values.next_back(), None);
        assert_eq!(values.len(), 0);
    }

    #[test]
    fn test_truth_table_double_ended_2() {
        let expr = implies(var("P"), var("Q"));
        let mut values = TruthTable::new(&expr, vec!["P".into(), "Q".into()])
            .unwrap()
            .map(|(_, val)| val);
        assert_eq!(values.next_back(), Some(t()));
        assert_eq!(values.next_back(), Some(f()));
        assert_eq!(values.next(), Some(t()));
        assert_eq!(values.next(), Some(t()));
        assert_eq!(values.next(), None);
        assert_eq!(values.next_back(), None);
    }
}
