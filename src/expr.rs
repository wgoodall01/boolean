pub type Expr = Box<ExprValue>;

#[derive(PartialEq, Debug)]
pub enum ExprValue {
    True,
    False,
    Identifier(String),
    Not(Expr),
    And(Expr, Expr),
    Or(Expr, Expr),
    Implies(Expr, Expr),
    Biconditional(Expr, Expr),
}

// Some convenience functions, basically because Box::new() is so long and annoying.

pub fn and(a: Expr, b: Expr) -> Expr {
    Box::new(ExprValue::And(a, b))
}

pub fn or(a: Expr, b: Expr) -> Expr {
    Box::new(ExprValue::Or(a, b))
}

pub fn not(a: Expr) -> Expr {
    Box::new(ExprValue::Not(a))
}

pub fn implies(a: Expr, b: Expr) -> Expr {
    Box::new(ExprValue::Implies(a, b))
}

pub fn biconditional(a: Expr, b: Expr) -> Expr {
    Box::new(ExprValue::Biconditional(a, b))
}

pub fn id(name: &str) -> Expr {
    Box::new(ExprValue::Identifier(name.into()))
}

pub fn t() -> Expr {
    Box::new(ExprValue::True)
}

pub fn f() -> Expr {
    Box::new(ExprValue::False)
}
