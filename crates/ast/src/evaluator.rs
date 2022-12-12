use super::parser;
use super::parser::Parser;
use super::node::{Expr, Factor, Term, Unit, Primary, Group, Add, Mul, Assignment};
use std::fmt;

use std::collections::HashMap;

#[derive(Debug)]
pub enum Error {
    EvalError(String),
    Eof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t = match self {
            Error::EvalError(s) => s,
            Error::Eof => "EOF",
        };
        write!(f, "{}", t)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::EvalError(err.to_string())
    }
}

pub struct Namespace {
    names: HashMap<String, Primary>,
    _subspaces: HashMap<String, Box<Namespace>>,
}

impl Namespace {
    fn new() -> Self {
        Namespace{
            names: HashMap::new(),
            _subspaces: HashMap::new(),
        }
    }

    fn get_int(&mut self, id: String) -> Option<u64> {
        match self.names.get(&id) {
            Some(p) => match p {
                Primary::Integer(i) => Some(*i),
                _ => None,
            }
            None => None,
        }
    }
}

pub struct Evaluator {
    pub exprs: Vec<Expr>,
    namespace: Namespace,
}

impl Evaluator {
    pub fn new(s: &str) -> Result<Self> {
        let mut p = Parser::new(s)?;
        let exprs = p.parse()?;
        let e = Evaluator {
            exprs: exprs,
            namespace: Namespace::new(),
        };
        Ok(e)
    }

    pub fn new_interactive() -> Self {
        Evaluator {
            exprs: Vec::new(),
            namespace: Namespace::new(),
        }
    }

    pub fn evaluate_interactive(&mut self, s: &str) -> Result<Expr> {
        let mut p = Parser::new(s)?;
        let exprs = p.parse()?;
        self.evaluate_expr(exprs[0].clone())
    }

    pub fn evaluate(&mut self) -> Result<Vec<Expr>> {
        let mut eval_exprs: Vec<Expr> = Vec::new();
        let e = self.exprs.clone();
        for expr in e {
            eval_exprs.push(self.evaluate_expr(expr)?);
        }

        Ok(eval_exprs)
    }

    pub fn evaluate_expr(&mut self, expr: Expr) -> Result<Expr> {
        match expr {
            Expr::Assignment(a) => {
                let p = self.reduce_factor(a.factor)?;
                let f = Factor::from_primary(p.clone());
                self.namespace.names.insert(a.id.clone(), p.clone());
                Ok(Expr::Assignment(Assignment{id: a.id, factor: f}))
            }
            Expr::Factor(f) => {
                let p = self.reduce_factor(f)?;
                let g = Factor::from_primary(p);
                Ok(Expr::Factor(g))
            }
            Expr::Eof => return Err(self.create_error("no".to_string())),
        }
    }


    pub fn add(&mut self, op: Add, p1: Primary, p2: Primary) -> Result<Primary> {
        match (p1, p2) {
            (Primary::Integer(i1), Primary::Integer(i2)) => {
                match op {
                    Add::Plus => Ok(Primary::Integer(i1+i2)),
                    Add::Minus => Ok(Primary::Integer(i1-i2)),
                }
            }
            (Primary::Integer(i1), Primary::Identifier(t)) => {
                match self.namespace.get_int(t) {
                    Some(i2) => match op {
                        Add::Plus => Ok(Primary::Integer(i1+i2)),
                        Add::Minus => Ok(Primary::Integer(i1-i2)),
                    }
                    None => Err(self.create_error("type mismatch".to_string())),
                }
            }
            (Primary::Identifier(t), Primary::Integer(i2)) => {
                match self.namespace.get_int(t) {
                    Some(i1) => match op {
                        Add::Plus => Ok(Primary::Integer(i1+i2)),
                        Add::Minus => Ok(Primary::Integer(i1-i2)),
                    }
                    None => Err(self.create_error("type mismatch".to_string())),
                }
            }
            (Primary::Identifier(t1), Primary::Identifier(t2)) => {
                match (self.namespace.get_int(t1), self.namespace.get_int(t2)) {
                    (Some(i1), Some(i2)) => match op {
                        Add::Plus => Ok(Primary::Integer(i1+i2)),
                        Add::Minus => Ok(Primary::Integer(i1-i2)),
                    }
                    _ => Err(self.create_error("type mismatch".to_string())),
                }
            }

            (_,_) => Err(self.create_error("type mismatch".to_string())),
        }
    }

    pub fn mul(&mut self, op: Mul, p1: Primary, p2: Primary) -> Result<Primary> {
        match (p1, p2) {
            (Primary::Integer(i1), Primary::Integer(i2)) => {
                match op {
                    Mul::Times => Ok(Primary::Integer(i1*i2)),
                    Mul::Divide => Ok(Primary::Integer(i1/i2)),
                }
            }
            (Primary::Integer(i1), Primary::Identifier(t)) => {
                match self.namespace.get_int(t) {
                    Some(i2) => match op {
                        Mul::Times => Ok(Primary::Integer(i1*i2)),
                        Mul::Divide => Ok(Primary::Integer(i1/i2)),
                    }
                    None => Err(self.create_error("type mismatch".to_string())),
                }
            }
            (Primary::Identifier(t), Primary::Integer(i2)) => {
                match self.namespace.get_int(t) {
                    Some(i1) => match op {
                        Mul::Times => Ok(Primary::Integer(i1*i2)),
                        Mul::Divide => Ok(Primary::Integer(i1/i2)),
                    }
                    None => Err(self.create_error("type mismatch".to_string())),
                }
            }
            (Primary::Identifier(t1), Primary::Identifier(t2)) => {
                match (self.namespace.get_int(t1), self.namespace.get_int(t2)) {
                    (Some(i1), Some(i2)) => match op {
                        Mul::Times => Ok(Primary::Integer(i1*i2)),
                        Mul::Divide => Ok(Primary::Integer(i1/i2)),
                    }
                    _ => Err(self.create_error("type mismatch".to_string())),
                }
            }

            (_,_) => Err(self.create_error("type mismatch".to_string())),
        }
    }

    pub fn reduce_factor(&mut self, f: Factor) -> Result<Primary> {
        let t1 = self.reduce_term(f.term)?;

        match f.more {
            Some((op, f2)) => {
                let t2 = self.reduce_factor(*f2)?;
                self.add(op, t1, t2)
            }
            None => Ok(t1),
        }
    }

    pub fn reduce_term(&mut self, t: Term) -> Result<Primary> {
        let u1 = self.reduce_unit(t.unit)?;

        match t.more {
            Some((op, t2)) => {
                let u2 = self.reduce_term(*t2)?;
                self.mul(op, u1, u2)
            }
            None => Ok(u1),
        }
    }

    pub fn reduce_unit(&mut self, u: Unit) -> Result<Primary> {
        match u {
            Unit::Primary(p) => {
                match p {
                    Primary::Identifier(id) => {
                        match self.namespace.get_int(id) {
                            Some(i) => Ok(Primary::Integer(i)),
                            None => Err(self.create_error("unknown var".to_string())),
                        }
                    }
                    other => Ok(other),
                }
            },
            Unit::Group(g) => self.reduce_group(*g),
        }
    }

    pub fn reduce_group(&mut self, g: Group) -> Result<Primary> {
        self.reduce_factor(g.factor)
    }

    pub fn create_error(&mut self, msg: String) -> Error {
        Error::EvalError(format!("eval_error: {}", msg))
    }
}
