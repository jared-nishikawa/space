use std::fmt;

#[derive(Debug, Clone)]
pub enum Add {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum Mul {
    Times,
    Divide,
}

#[derive(Debug, Clone)]
pub enum Primary {
    Identifier(String),
    Integer(u64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Unit {
    Primary(Primary),
    Group(Box<Group>),
}

#[derive(Debug, Clone)]
pub struct Term {
    pub unit: Unit,
    pub more: Option<(Mul, Box<Term>)>,
}

#[derive(Debug, Clone)]
pub struct Factor {
    pub term: Term,
    pub more: Option<(Add, Box<Factor>)>,
}

impl Factor {
    pub fn from_primary(p: Primary) -> Factor {
        Factor {
            term: Term {
                unit: Unit::Primary(p),
                more: None,
            },
            more: None,
        }
    }

    pub fn to_primary(&mut self) -> Option<Primary> {
        match self.more {
            None => {
                match self.term.more {
                    None => {
                        let u = self.term.unit.clone();
                        match u {
                            Unit::Primary(p) => Some(p),
                            Unit::Group(_) => None,
                        }
                    }
                    Some(_) => None,
                }
            }
            Some(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Group {
    pub factor: Factor,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub id: String,
    pub factor: Factor,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assignment(Assignment),
    Factor(Factor),
    Eof,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Eof => write!(f, ""),
            Expr::Assignment(a) => write!(f, "{} = {}", a.id, a.factor),
            Expr::Factor(factor) => {
                match &factor.more {
                    Some((tok, t2)) => write!(f, "{} {} {}", factor.term, tok, t2),
                    None => write!(f, "{}", factor.term),
                }
            }
        }
    }
}

impl fmt::Display for Primary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primary::Identifier(id) => write!(f, "{}", id),
            Primary::Integer(i) => write!(f, "{}", i),
            Primary::String(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unit::Primary(p) => write!(f, "{}", p),
            Unit::Group(g) => write!(f, "{}", g),
        }
    }
}

impl fmt::Display for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.factor)
    }
}

impl fmt::Display for Factor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.more {
            Some((tok, t2)) => write!(f, "{} {} {}", self.term, tok, t2),
            None => write!(f, "{}", self.term),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.more {
            Some((tok, u2)) => write!(f, "{} {} {}", self.unit, tok, u2),
            None => write!(f, "{}", self.unit),
        }
    }
}

impl fmt::Display for Add {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Add::Plus => write!(f, "+"),
            Add::Minus => write!(f, "-"),
        }
    }
}

impl fmt::Display for Mul {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mul::Times => write!(f, "*"),
            Mul::Divide => write!(f, "/"),
        }
    }
}



