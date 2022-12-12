use std::fmt;

#[derive(Debug)]
pub enum Error {
    ScanError(String),
    Eof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t = match self {
            Error::ScanError(s) => s,
            Error::Eof => "EOF",
        };
        write!(f, "{}", t)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<std::num::ParseIntError> for Error {
    fn from(err: std::num::ParseIntError) -> Error {
        Error::ScanError(err.to_string())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub row: u32,
    pub col: u32,
}

impl PartialEq for Position {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}:{})", self.row, self.col)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    Whitespace,
    Newline(Position),

    // keywords
    Let(Position),

    // identifiers
    Identifier(Position, String),
    
    // symbols
    Equals(Position),
    Plus(Position),
    Minus(Position),
    Asterisk(Position),
    ForwardSlash(Position),

    OpenParen(Position),
    CloseParen(Position),

    // number types
    Float(Position, f64),
    Integer(Position, u64),

    // strings
    String(Position, String),
    Char(Position, char),
}

impl Token {
    pub fn position(&self) -> Position {
        match self {
            Token::Eof => Position {row: 0, col: 0},
            Token::Whitespace => Position {row: 0, col: 0},
            Token::Newline(pos) => *pos,
            Token::Let(pos) => *pos,
            Token::Identifier(pos, _) => *pos,
            Token::Equals(pos) => *pos,
            Token::Plus(pos) => *pos,
            Token::Minus(pos) => *pos,
            Token::Asterisk(pos) => *pos,
            Token::ForwardSlash(pos) => *pos,
            Token::OpenParen(pos) => *pos,
            Token::CloseParen(pos) => *pos,
            Token::Float(pos, _) => *pos,
            Token::Integer(pos, _) => *pos,
            Token::String(pos, _) => *pos,
            Token::Char(pos, _) => *pos,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Eof => write!(f, ""),
            Token::Whitespace => write!(f, ""),
            Token::Newline(_) => writeln!(f),
            Token::Let(_) => write!(f, "let"),
            Token::Identifier(_, s) => write!(f, "{}", s),
            Token::Equals(_) => write!(f, "="),
            Token::Plus(_) => write!(f, "+"),
            Token::Minus(_) => write!(f, "-"),
            Token::Asterisk(_) => write!(f, "*"),
            Token::ForwardSlash(_) => write!(f, "/"),
            Token::OpenParen(_) => write!(f, "("),
            Token::CloseParen(_) => write!(f, ")"),
            Token::Float(_, d) => write!(f, "{}", d),
            Token::Integer(_, i) => write!(f, "{}", i),
            Token::String(_, s) => write!(f, "\"{}\"", s),
            Token::Char(_, c) => write!(f, "\'{}\'", c),
        }
    }
}

pub struct Scanner {
    input: Vec<char>,
    cur: usize,
    pos: Position,
}

impl Scanner {
    pub fn new(s: &str) -> Self {
        Scanner {
            input: s.chars().collect(),
            cur: 0,
            pos: Position {row: 1, col: 0},
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        loop {
            match self.scan_token() {
                Ok(Token::Eof) => break,
                Ok(Token::Whitespace) => continue,
                Ok(tok) => tokens.push(tok),
                Err(e) => return Err(self.create_error(e.to_string())),
            }
        }
        Ok(tokens)
    }

    pub fn scan_token(&mut self) -> Result<Token> {
        // this should be comprehensive
        let token = match self.peek(0) {
            '\0' => Ok(Token::Eof),
            ' ' | '\t' | 'r' => self.scan_whitespace(),
            '\n' => self.scan_newlines(),
            '=' => self.create_token(Token::Equals(self.pos)),
            '+' => self.create_token(Token::Plus(self.pos)),
            '-' => self.create_token(Token::Minus(self.pos)),
            '*' => self.create_token(Token::Asterisk(self.pos)),
            '/' => self.create_token(Token::ForwardSlash(self.pos)),
            '(' => self.create_token(Token::OpenParen(self.pos)),
            ')' => self.create_token(Token::CloseParen(self.pos)),
            '"' => self.scan_string(),
            '0'..='9' => self.scan_number(),
            'a'..='z'|'A'..='Z'|'_' => self.scan_identifier(),
            c => Err(self.create_error(format!("invalid character: {}", c))),
        };
        token
    }

    pub fn create_error(&mut self, msg: String) -> Error {
        Error::ScanError(format!("scan_error at {}: {}", self.pos, msg))
    }

    pub fn create_token(&mut self, tok: Token) -> Result<Token> {
        self.read();
        Ok(tok)
    }

    pub fn peek(&mut self, n: usize) -> char {
        if self.cur < self.input.len() - n {
            self.input[self.cur + n]
        } else {
            '\0'
        }
    }

    pub fn read(&mut self) -> char {
        let ret = self.cur;

        if ret >= self.input.len() {
            self.cur += 1;
            return '\0';
        }

        if self.input[ret] == '\n' {
            self.pos.row += 1;
            self.pos.col = 0;
        } else {
            self.pos.col += 1;
        }

        self.cur += 1;
        self.input[ret]
    }

    pub fn scan_whitespace(&mut self) -> Result<Token> {
        loop {
            match self.peek(0) {
                ' ' | '\t' | '\r' => self.read(),
                _ => break,
            };
        }
        Ok(Token::Whitespace)
    }

    pub fn scan_newlines(&mut self) -> Result<Token> {
        loop {
            match self.peek(0) {
                '\n' => self.read(),
                _ => break,
            };
        };
        Ok(Token::Newline(self.pos))
    }

    pub fn scan_string(&mut self) -> Result<Token> {
        self.read();
        let mut char_vec: Vec<char> = Vec::new();
        loop {
            match self.peek(0) {
                '"' => {
                    self.read();
                    break;
                },
                '\\' => {
                    match self.peek(1) {
                        '"' => {
                            self.read();
                            char_vec.push(self.read());
                        },
                        _ => char_vec.push(self.read()),
                    };
                }
                _ => char_vec.push(self.read()),
            }
        }
        let s: String = char_vec.into_iter().collect();
        Ok(Token::String(self.pos, s))

    }

    pub fn scan_number(&mut self) -> Result<Token> {
        let mut char_vec: Vec<char> = Vec::new();
        loop {
            match self.peek(0) {
                '0'..='9' => char_vec.push(self.read()),
                _ => break,
            }
        }
        let s: String = char_vec.into_iter().collect();
        let n: u64 = s.parse()?;
        Ok(Token::Integer(self.pos, n))
    }

    pub fn scan_identifier(&mut self) -> Result<Token> {
        let mut char_vec: Vec<char> = Vec::new();
        loop {
            let c = self.peek(0);
            if is_identifier(c) {
                char_vec.push(c);
                self.read();
            } else {
                break;
            }
        }
        let s: String = char_vec.into_iter().collect();
        match s.as_str() {
            "let" => Ok(Token::Let(self.pos)),
            _ => Ok(Token::Identifier(self.pos, s)),
        }

    }
}

fn is_digit(c: char) -> bool {
    return c >= '0' && c <= '9'
}

fn is_identifier_start(c: char) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_identifier(c: char) -> bool {
    return is_digit(c) || is_identifier_start(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_let1() {
        let text = "let a = 0";
        let tokens = Scanner::new(text).scan().unwrap();
        if let Token::Let(_) = &tokens[0] {
            assert_eq!(true, true);
        } else {
            panic!("expecting \"let\"");
        }
    }

    #[test]
    fn scan_let2() {
        let text = "let A = 0";
        let tokens = Scanner::new(text).scan().unwrap();
        if let Token::Let(_) = &tokens[0] {
            assert_eq!(true, true);
        } else {
            panic!("expecting \"let\"");
        }
    }

}
