use std::fmt;

use super::scanner;
use super::scanner::{Scanner, Token};
use super::node::{Group, Primary, Unit, Term, Assignment, Factor, Expr, Add, Mul};

#[derive(Debug)]
pub enum Error {
    ParseError(String),
    Eof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t = match self {
            Error::ParseError(s) => s,
            Error::Eof => "EOF",
        };
        write!(f, "{}", t)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<scanner::Error> for Error {
    fn from(err: scanner::Error) -> Error {
        Error::ParseError(err.to_string())
    }
}

pub struct Parser {
    exprs: Vec<Expr>,
    tokens: Vec<Token>,
    cur: usize,
}

impl Parser {
    pub fn new(s: &str) -> Result<Self> {
        let tokens = Scanner::new(s).scan()?;
        let p = Parser {
            exprs: Vec::new(),
            tokens: tokens,
            cur: 0,
        };
        Ok(p)
    }

    pub fn peek(&mut self, n: usize) -> Token {
        if self.cur < self.tokens.len() - n {
            self.tokens[self.cur + n].clone()
        } else {
            Token::Eof
        }
    }

    pub fn read(&mut self) -> Token {
        let ret = self.cur;

        if ret >= self.tokens.len() {
            self.cur += 1;
            return Token::Eof;
        }

        self.cur += 1;
        self.tokens[ret].clone()
    }

    pub fn create_error(&mut self, msg: String) -> Error {
        Error::ParseError(format!("parse_error at {}: {}", self.tokens[self.cur].position(), msg))
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>> {
        loop {
            match self.parse_expr() {
                Ok(Expr::Eof) => break,
                Ok(expr) => {
                    self.exprs.push(expr);
                    match self.peek(0) {
                        Token::Eof => break,
                        Token::Newline(_) => {
                            self.read();
                            continue;
                        }
                        t => return Err(self.create_error(format!("unexpected: {}", t))),
                    }
                }
                Err(e) => return Err(self.create_error(e.to_string())),
            }
        }
        Ok(self.exprs.clone())
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        // <expr> ::= <assignment> | <factor>
        match self.peek(0) {
            Token::Eof => return Ok(Expr::Eof),
            Token::Identifier(..) => {
                match self.peek(1) {
                    // if identifier followed by "=", then assignment
                    Token::Equals(_) => {
                        return Ok(Expr::Assignment(self.parse_assignment()?));
                    }
                    _ => (),
                }
            }
            _ => (),
        }
        // otherwise factor
        Ok(Expr::Factor(self.parse_factor()?))
    }

    pub fn parse_factor(&mut self) -> Result<Factor> {
        // <factor> ::= <term> | <term> <add> <factor>
        let t1 = self.parse_term()?;
        match self.peek(0) {
            Token::Plus(_) | Token::Minus(_) => {
                let op = match self.read() {
                    Token::Plus(_) => Add::Plus,
                    Token::Minus(_) => Add::Minus,
                    t => return Err(self.create_error(format!("invalid op: {}", t))),
                };
                let t2 = self.parse_factor()?;
                return Ok(Factor{term: t1, more: Some((op, Box::new(t2)))});
            }
            _ => (),
        }
        Ok(Factor{term: t1, more: None})
    }

    pub fn parse_assignment(&mut self) -> Result<Assignment> {
        // <assignment> ::= <identifier> "=" <factor>
        let id = match self.read() {
            Token::Identifier(_,id) => id,
            _ => return Err(self.create_error("parse error on assignment".to_string())),
        };
        match self.read() {
            Token::Equals(_) => (),
            _ => return Err(self.create_error("parse error on assignment".to_string())),
        }

        let f = self.parse_factor()?;

        Ok(Assignment{id: id, factor: f})
    }

    pub fn parse_term(&mut self) -> Result<Term> {
        // <term> ::= <unit> | <unit> <mul> <term>
        let u1 = self.parse_unit()?;
        match self.peek(0) {
            Token::Asterisk(_) | Token::ForwardSlash(_) => {
                let op = match self.read(){
                    Token::Asterisk(_) => Mul::Times,
                    Token::ForwardSlash(_) => Mul::Divide,
                    t => return Err(self.create_error(format!("invalid op: {}", t))),
                };
                let u2 = self.parse_term()?;
                return Ok(Term{unit: u1, more: Some((op, Box::new(u2)))});
            }
            _ => (),
        }
        Ok(Term{unit: u1, more: None})
    }

    pub fn parse_unit(&mut self) -> Result<Unit> {
        // <unit> ::= <primary> | <group>
        match self.peek(0) {
            Token::Identifier(..) |
                Token::Integer(..) |
                Token::String(..) => Ok(Unit::Primary(self.parse_primary()?)),
            Token::OpenParen(_) => Ok(Unit::Group(Box::new(self.parse_group()?))),
            t => Err(self.create_error(format!("error parsing unit: unexpected {}", t))),
        }
    }

    pub fn parse_primary(&mut self) -> Result<Primary> {
        match self.read() {
            Token::Identifier(_,id) => Ok(Primary::Identifier(id)),
            Token::Integer(_,i) => Ok(Primary::Integer(i)),
            Token::String(_,s) => Ok(Primary::String(s)),
            _t => Err(self.create_error("error parsing primary".to_string())),
        }
    }

    pub fn parse_group(&mut self) -> Result<Group> {
        match self.read() {
            Token::OpenParen(..) => (),
            _ => return Err(self.create_error("error parsing group".to_string())),
        };
        let f = self.parse_factor()?;
        match self.read() {
            Token::CloseParen(..) => (),
            _ => return Err(self.create_error("error parsing group".to_string())),
        };
        Ok(Group{factor: f})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_equals1() {
        let text = "a = 0";
        let exprs = Parser::new(text).unwrap().parse().unwrap();
        assert_eq!(exprs.len(), 1);
        match &exprs[0] {
            Expr::Assignment(_) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_equals2() {
        let text = "a = \"some string\"";
        let exprs = Parser::new(text).unwrap().parse().unwrap();
        assert_eq!(exprs.len(), 1);
        match &exprs[0] {
            Expr::Assignment(a) => match a.clone().factor.to_primary() {
                Some(p) => match p {
                    Primary::String(s) => assert_eq!(s, "some string"),
                    _ => assert!(false),
                }
                None => assert!(false),
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_equals3() {
        let text = "a = \"some string with a \\\"quote\\\" in the middle\"";
        let exprs = Parser::new(text).unwrap().parse().unwrap();
        assert_eq!(exprs.len(), 1);
        match &exprs[0] {
            Expr::Assignment(a) => match a.clone().factor.to_primary() {
                Some(p) => match p {
                    Primary::String(s) => assert_eq!(s, "some string with a \"quote\" in the middle"),
                    _ => assert!(false),
                }
                None => assert!(false),
            }
            _ => assert!(false),
        }
    }

}
