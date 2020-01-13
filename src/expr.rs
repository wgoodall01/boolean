#[derive(PartialEq, Debug)]
pub enum Expr {
    True,
    False,
    Identifier(String),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Implies(Box<Expr>, Box<Expr>),
    Biconditional(Box<Expr>, Box<Expr>),
}

pub fn and(a: Expr, b: Expr) -> Expr {
    Expr::And(Box::new(a), Box::new(b))
}

pub fn or(a: Expr, b: Expr) -> Expr {
    Expr::Or(Box::new(a), Box::new(b))
}

pub fn not(a: Expr) -> Expr {
    Expr::Not(Box::new(a))
}

pub fn implies(a: Expr, b: Expr) -> Expr {
    Expr::Implies(Box::new(a), Box::new(b))
}

pub fn biconditional(a: Expr, b: Expr) -> Expr {
    Expr::Biconditional(Box::new(a), Box::new(b))
}

pub fn id(name: &str) -> Expr {
    Expr::Identifier(String::from(name))
}

pub fn t() -> Expr {
    Expr::True
}

pub fn f() -> Expr {
    Expr::False
}
