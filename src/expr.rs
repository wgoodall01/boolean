use std::borrow::ToOwned;
use std::cmp;
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
                // Domination laws
                (Expr::False, _) => t(),
                (Expr::True, Expr::True) => t(),

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
}
