use std::cmp;
use std::collections::HashSet;
use std::rc;
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    True,
    False,
    Var(String),
    Not(Rc<Expr>),
    And(Rc<Expr>, Rc<Expr>),
    Or(Rc<Expr>, Rc<Expr>),
    Implies(Rc<Expr>, Rc<Expr>),
    Biconditional(Rc<Expr>, Rc<Expr>),
}

impl cmp::PartialEq<rc::Rc<Expr>> for Expr {
    fn eq(&self, other: &rc::Rc<Expr>) -> bool {
        *self == **other
    }
}

pub fn and(a: Expr, b: Expr) -> Expr {
    Expr::And(Rc::new(a), Rc::new(b))
}

pub fn or(a: Expr, b: Expr) -> Expr {
    Expr::Or(Rc::new(a), Rc::new(b))
}

pub fn not(a: Expr) -> Expr {
    Expr::Not(Rc::new(a))
}

pub fn implies(a: Expr, b: Expr) -> Expr {
    Expr::Implies(Rc::new(a), Rc::new(b))
}

pub fn biconditional(a: Expr, b: Expr) -> Expr {
    Expr::Biconditional(Rc::new(a), Rc::new(b))
}

pub fn var(name: &str) -> Expr {
    Expr::Var(String::from(name))
}

pub fn t() -> Expr {
    Expr::True
}

pub fn f() -> Expr {
    Expr::False
}

impl Expr {
    pub fn eval(&self) -> Expr {
        match self {
            // Terminal expressions
            Expr::True => t(),
            Expr::False => f(),
            Expr::Var(name) => var(name),

            Expr::Not(ex) => match ex.eval() {
                // Domination laws
                Expr::True => f(),
                Expr::False => t(),

                // Double negation
                Expr::Not(val) => (*val).clone(),

                // Unevaluated
                held => not(held),
            },

            Expr::And(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                // Domination Laws
                (Expr::False, _) => f(),
                (_, Expr::False) => f(),

                // Identity Laws
                (Expr::True, val) => val,
                (val, Expr::True) => val,

                // Idempotent Law
                (ref l, ref r) if l == r => l.clone(),

                // Negation law
                (ref l, ref r) if l == &not(r.clone()).eval() => f(),
                (ref l, ref r) if r == &not(l.clone()).eval() => f(),

                // Absorbtion Law
                (ref a, Expr::Or(ref b, _)) if a == b => a.clone(),
                (ref a, Expr::Or(_, ref b)) if a == b => a.clone(),
                (Expr::Or(ref b, _), ref a) if a == b => a.clone(),
                (Expr::Or(_, ref b), ref a) if a == b => a.clone(),

                // Unevaluated
                (l, r) => and(l, r),
            },

            Expr::Or(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                // Domination laws
                (Expr::True, _) => t(),
                (_, Expr::True) => t(),

                // Identity laws
                (Expr::False, val) => val,
                (val, Expr::False) => val,

                // Idempotent Law
                (ref l, ref r) if l == r => l.clone(),

                // Negation law
                (ref l, ref r) if l == &not(r.clone()).eval() => f(),
                (ref l, ref r) if r == &not(l.clone()).eval() => f(),

                // Absorbtion Law
                (ref a, Expr::And(ref b, _)) if a == b => a.clone(),
                (ref a, Expr::And(_, ref b)) if a == b => a.clone(),
                (Expr::And(ref b, _), ref a) if a == b => a.clone(),
                (Expr::And(_, ref b), ref a) if a == b => a.clone(),

                // Unevaluated
                (l, r) => or(l, r),
            },

            Expr::Implies(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                // Vacuous case
                (Expr::False, _) => t(),

                // Non-vacuous cases
                (Expr::True, Expr::True) => t(),
                (Expr::True, Expr::False) => f(),

                // Unevaluated
                (p, q) => implies(p, q),
            },

            Expr::Biconditional(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                // Domination laws
                (Expr::True, Expr::True) => t(),
                (Expr::False, Expr::False) => t(),
                (Expr::True, Expr::False) => f(),
                (Expr::False, Expr::True) => f(),

                // Unevaluated
                (p, q) => biconditional(p, q),
            },
        }
    }

    /// Apply a function to this Expr's direct children.
    ///
    /// Note: this isn't necessarily mathematically valid.
    pub fn map<F>(&self, map_fn: &mut F) -> Expr
    where
        F: FnMut(&Expr) -> Expr,
    {
        match self {
            val @ Expr::True | val @ Expr::False | val @ Expr::Var(_) => val.clone(),
            Expr::Not(ex) => not(map_fn(ex)),
            Expr::And(ref lhs, ref rhs) => and(map_fn(lhs), map_fn(rhs)),
            Expr::Or(ref lhs, ref rhs) => or(map_fn(lhs), map_fn(rhs)),
            Expr::Implies(ref lhs, ref rhs) => implies(map_fn(lhs), map_fn(rhs)),
            Expr::Biconditional(ref lhs, ref rhs) => biconditional(map_fn(lhs), map_fn(rhs)),
        }
    }

    /// Replace Expr::Var(var_name) with clones of `value`, recursively.
    pub fn substitute(&self, var_name: &str, value: &Expr) -> Expr {
        match self {
            Expr::Var(name) if *name == var_name => value.clone(),
            val => val.map(&mut move |ex: &Expr| ex.substitute(var_name, value)),
        }
    }

    /// Get a HashSet of all symbols used in the expression
    pub fn symbols(&self) -> HashSet<String> {
        let mut symbols: HashSet<String> = HashSet::new();
        self.insert_symbols(&mut symbols);
        symbols
    }

    fn insert_symbols(&self, set: &mut HashSet<String>) {
        match self {
            Expr::Var(name) => {
                set.insert(name.into());
            }
            other => {
                other.map(&mut move |ex| {
                    ex.insert_symbols(set);
                    ex.clone()
                });
            }
        }
    }

    pub fn truth_table(&self, symbols: Vec<String>) -> TruthTable {
        let mut index: usize = 0;
        let max = 1 << symbols.len();
        let mut values: Vec<Expr> = Vec::with_capacity(max);
        println!("max: {:b}", max);
        println!("index: {:b}", index);
        while index < max {
            println!("index:{:?}", index);
            let mut expr = self.clone();
            let mut shifted = index;

            for symbol in symbols.iter().rev() {
                let value = match shifted & 0b1 {
                    0b0 => Expr::False,
                    0b1 => Expr::True,
                    _ => panic!("impossible arithmetic occured"),
                };

                println!("shifted:{:b} sym:{:?} val:{:?}", shifted, symbol, value);
                expr = expr.substitute(symbol, &value);
                shifted = shifted >> 1;
            }

            println!("expr: {:?}", expr.eval());
            values.push(expr.eval());
            index += 1;
        }

        TruthTable {
            symbols: symbols.iter().map(|s| s.to_string()).collect(),
            values,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct TruthTable {
    // e.g. "a", "b"
    symbols: Vec<String>,

    // e.g. result of (!a, !b), (!a, b), (a, !b), (a, b)
    values: Vec<Expr>,
}

#[cfg(test)]
mod test {
    use super::super::parser;
    use super::*;

    #[test]
    fn test_calculate_expr() {
        let expr = parser::parse_str("a & (b | c) & !d").unwrap();
        let result = expr
            .substitute("a", &t())
            .substitute("b", &t())
            .substitute("c", &f())
            .substitute("d", &f())
            .eval();
        assert_eq!(result, t());
    }

    #[test]
    fn test_truth_table() {
        let expr = parser::parse_str("a & b").unwrap();
        let symbols = expr.symbols().into_iter().collect();
        assert_eq!(symbols, vec!["a", "b"]);
        let result = expr.truth_table(symbols);
        assert_eq!(result.values, vec![f(), f(), f(), t()]);
    }

    #[test]
    fn test_big_truth_table_works() {
        let expr = parser::parse_str("d => a & (b | c)").unwrap();
        let mut symbols: Vec<String> = expr.symbols().into_iter().collect();
        symbols.sort();
        let sym_count = symbols.len();
        assert_eq!(symbols, vec!["a", "b", "c", "d"]);

        let result = expr.truth_table(symbols);
        assert_eq!(result.values.len(), 1 << sym_count);
        assert_eq!(result.symbols.len(), sym_count);
    }
}
