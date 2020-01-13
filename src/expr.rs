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
                Expr::True => f(),
                Expr::False => t(),
                held @ _ => held,
            },

            Expr::And(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                (Expr::False, _) => f(),
                (_, Expr::False) => f(),
                (Expr::True, Expr::True) => t(),
                (l, r) => and(l, r),
            },

            Expr::Or(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                (Expr::True, _) => t(),
                (_, Expr::True) => t(),
                (Expr::False, Expr::False) => f(),
                (l, r) => or(l, r),
            },

            Expr::Implies(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                (Expr::False, _) => t(),
                (Expr::True, Expr::True) => t(),
                (p, q) => implies(p, q),
            },

            Expr::Biconditional(lhs, rhs) => match (lhs.eval(), rhs.eval()) {
                (Expr::True, Expr::True) => t(),
                (Expr::False, Expr::False) => t(),
                (Expr::True, Expr::False) => f(),
                (Expr::False, Expr::True) => f(),
                (p, q) => biconditional(p, q),
            },
        }
    }
}
