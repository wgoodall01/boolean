use super::truth_table::{hash_to_expr, TruthTable};
use itertools::Itertools;
use simple_error::SimpleError;
use std::cmp;
use std::collections::HashSet;
use std::convert::From;
use std::fmt;
use std::rc;
use std::rc::Rc;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Expr {
    True,
    False,
    Var(String),
    Not(Rc<Expr>),
    And(Rc<Expr>, Rc<Expr>),
    Or(Rc<Expr>, Rc<Expr>),
    Implies(Rc<Expr>, Rc<Expr>),
    Biconditional(Rc<Expr>, Rc<Expr>),
    Command(String, Vec<Rc<Expr>>),
    Str(String),
}

impl From<bool> for Expr {
    fn from(val: bool) -> Expr {
        if val {
            t()
        } else {
            f()
        }
    }
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

pub fn command(name: String, args: Vec<Expr>) -> Expr {
    Expr::Command(name, args.into_iter().map(Rc::new).collect())
}

pub fn error(args: Vec<&str>) -> Expr {
    command("Error".into(), args.into_iter().map(str).collect())
}

pub fn var(name: &str) -> Expr {
    Expr::Var(String::from(name))
}

pub fn str(name: &str) -> Expr {
    Expr::Str(String::from(name))
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
            Expr::Str(name) => str(name),

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

            Expr::Command(name, args) => match name.as_str() {
                "Table" => {
                    if args.len() < 1 {
                        return error(vec!["BadUsage", "InvalidArgCount"]);
                    }

                    let expr: &Expr = Rc::as_ref(args.iter().next().unwrap());

                    let param_names: Vec<String> = if args.len() == 1 {
                        expr.symbols()
                    } else {
                        let mut names: Vec<String> = Vec::new();
                        for arg in &args[1..] {
                            match &**arg {
                                Expr::Var(name) => names.push(name.clone()),
                                _ => return error(vec!["BadUsage", "ParameterNotVariable"]),
                            }
                        }
                        names
                    };

                    if param_names.is_empty() {
                        // If there are no parameters, we can't make a truth table.
                        return expr.clone();
                    }

                    expr.truth_table(param_names)
                        .unwrap()
                        .filter_map(|(params, ex)| match ex {
                            Expr::True => Some(hash_to_expr(params)),
                            _ => None,
                        })
                        .fold1(or)
                        .unwrap_or_else(|| expr.clone())
                }
                "With" => {
                    if args.len() != 2 {
                        return error(vec!["BadUsage", "InvalidArgCount"]);
                    }

                    let mut expr: Expr = args[0].eval();

                    let mut replacements: Vec<(String, bool)> = Vec::new();
                    let mut assumptions: Vec<Rc<Expr>> = vec![args[1].clone()];

                    // Convert the expressions of `assumptions` into a list of concrete values
                    while let Some(truth) = assumptions.pop() {
                        match truth.as_ref() {
                            Expr::Var(name) => {
                                replacements.push((name.clone(), true));
                            }
                            Expr::And(a, b) => {
                                assumptions.push(a.clone());
                                assumptions.push(b.clone());
                            }
                            Expr::Not(negated) => match negated.as_ref() {
                                Expr::Var(name) => {
                                    replacements.push((name.clone(), false));
                                }
                                _ => return error(vec!["BadUsage", "MalformedAssumptionNegated"]),
                            },
                            _ => return error(vec!["BadUsage", "MalformedAssumption"]),
                        }
                    }

                    // Apply the replacements
                    for repl in replacements {
                        expr = expr.substitute(&repl.0, &repl.1.into());
                    }

                    expr.eval()
                }
                "Satisfy" => {
                    if args.len() != 1 {
                        return error(vec!["BadUsage"]);
                    }

                    let expr = args.iter().next().unwrap();
                    let sat = expr
                        .truth_table(expr.symbols())
                        .unwrap()
                        .filter(|(_, result)| result == &Expr::True)
                        .map(|(vars, _)| vars)
                        .next()
                        .map(hash_to_expr);

                    match sat {
                        Some(expr) => expr,
                        None => error(vec!["Unsatisfiable"]),
                    }
                }
                "Debug" => {
                    return str(format!("{:?}", args).as_str());
                }
                "Inert" | "Error" | _ => Expr::Command(name.clone(), args.clone()),
            },
        }
    }

    pub fn precedence(&self) -> u32 {
        match self {
            Expr::True => !0,
            Expr::False => !0,
            Expr::Var(_) => (!0) - 10,
            Expr::Str(_) => (!0) - 10,
            Expr::Not(_) => 60,
            Expr::And(_, _) => 50,
            Expr::Or(_, _) => 40,
            Expr::Implies(_, _) => 35,
            Expr::Biconditional(_, _) => 30,
            Expr::Command(_, _) => 20,
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
            val @ Expr::True | val @ Expr::False | val @ Expr::Var(_) | val @ Expr::Str(_) => {
                val.clone()
            }
            Expr::Not(ex) => not(map_fn(ex)),
            Expr::And(ref lhs, ref rhs) => and(map_fn(lhs), map_fn(rhs)),
            Expr::Or(ref lhs, ref rhs) => or(map_fn(lhs), map_fn(rhs)),
            Expr::Implies(ref lhs, ref rhs) => implies(map_fn(lhs), map_fn(rhs)),
            Expr::Biconditional(ref lhs, ref rhs) => biconditional(map_fn(lhs), map_fn(rhs)),
            Expr::Command(name, args) => {
                command(name.clone(), args.iter().map(|e| map_fn(e)).collect())
            }
        }
    }

    /// Replace Expr::Var(var_name) with clones of `value`, recursively.
    pub fn substitute(&self, var_name: &str, value: &Expr) -> Expr {
        match self {
            Expr::Var(name) if *name == var_name => value.clone(),
            val => val.map(&mut move |ex: &Expr| ex.substitute(var_name, value)),
        }
    }

    /// Get a list of all symbols used in the expression
    pub fn symbols(&self) -> Vec<String> {
        let mut symbols: HashSet<String> = HashSet::new();
        self.insert_symbols(&mut symbols);
        symbols.into_iter().collect()
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

    pub fn truth_table(&self, symbols: Vec<String>) -> Result<TruthTable, SimpleError> {
        TruthTable::new(self, symbols)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::True => write!(f, "T"),
            Expr::False => write!(f, "F"),
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Str(name) => write!(f, "\"{}\"", name),
            Expr::Not(ex) => write!(f, "!{}", paren(self, ex)),
            Expr::And(lhs, rhs) => write!(f, "{} & {}", paren(self, lhs), paren(self, rhs)),
            Expr::Or(lhs, rhs) => write!(f, "{} | {}", paren(self, lhs), paren(self, rhs)),
            Expr::Implies(lhs, rhs) => write!(f, "{} => {}", paren(self, lhs), paren(self, rhs)),
            Expr::Biconditional(lhs, rhs) => {
                write!(f, "{} <=> {}", paren(self, lhs), paren(self, rhs))
            }
            Expr::Command(cmd, args) => write!(
                f,
                "{} {}",
                cmd,
                args.iter().map(|ex| paren(&not(t()), ex)).format(" ")
            ),
        }
    }
}

fn paren(outer: &Expr, inner: &Expr) -> String {
    if outer.precedence() <= inner.precedence() {
        format!("{}", inner)
    } else {
        format!("({})", inner)
    }
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
    fn test_paren_inner_or() {
        let lhs = or(t(), f());
        let rhs = f();
        let expr = and(lhs.clone(), rhs.clone());
        assert_eq!(paren(&expr, &lhs), "(T | F)");
    }

    #[test]
    fn test_paren_inner_and() {
        let lhs = and(t(), f());
        let rhs = f();
        let expr = or(lhs.clone(), rhs.clone());
        assert_eq!(paren(&expr, &lhs), "T & F");
    }

    #[test]
    fn test_display_parse() {
        let exprs = vec![
            t(),
            f(),
            not(t()),
            and(or(t(), f()), t()),
            implies(and(or(t(), f()), t()), f()),
            implies(biconditional(t(), f()), f()),
            command(
                "Inert".into(),
                vec![
                    and(or(t(), f()), t()),
                    f(),
                    biconditional(implies(t(), f()), implies(t(), biconditional(t(), f()))),
                ],
            ),
            or(t(), or(t(), or(t(), t()))),
        ];

        for expr in exprs {
            let repr = format!("{}", expr);
            let parsed = parser::parse_str(&repr).unwrap();
            println!("{}", repr);
            assert_eq!(parsed, expr);
        }
    }
}
