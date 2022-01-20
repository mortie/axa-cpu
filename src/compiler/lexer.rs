use super::super::isa;
use std::collections::VecDeque;
use std::fmt;
use std::io;
use std::io::{BufRead, BufReader, Read};

#[derive(Debug, Clone)]
pub struct Token {
    pub line: u32,
    pub col: u32,
    pub kind: TokKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokKind {
    Ident(String),
    Reg(isa::Reg),
    Int(i32),
    Const,
    Data,
    Func,
    If,
    Else,
    Loop,
    While,
    Return,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Equals,
    NotEq,
    EqEq,
    Plus,
    Minus,
    Asterisk,
    Slash,
    PlusEq,
    MinusEq,
    AsteriskEq,
    SlashEq,
    Lt,
    Gt,
    LtLt,
    GtGt,
    LtEq,
    GtEq,
    AmpersandEq,
    PipeEq,
    Eof,
    Error(String),
}

impl fmt::Display for TokKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TokKind::Ident(name) => write!(f, "Ident ({})", name),
            TokKind::Reg(reg) => write!(f, "Reg ({})", reg),
            TokKind::Int(num) => write!(f, "Int ({})", num),
            TokKind::Const => write!(f, "'const'"),
            TokKind::Data => write!(f, "'data'"),
            TokKind::Func => write!(f, "'func'"),
            TokKind::If => write!(f, "'if'"),
            TokKind::Else => write!(f, "'else'"),
            TokKind::Loop => write!(f, "'loop'"),
            TokKind::While => write!(f, "'while'"),
            TokKind::Return => write!(f, "'return'"),
            TokKind::OpenParen => write!(f, "'('"),
            TokKind::CloseParen => write!(f, "')'"),
            TokKind::OpenBrace => write!(f, "'{{'"),
            TokKind::CloseBrace => write!(f, "'}}'"),
            TokKind::Semicolon => write!(f, "';'"),
            TokKind::Equals => write!(f, "'='"),
            TokKind::NotEq => write!(f, "'='"),
            TokKind::EqEq => write!(f, "'=='"),
            TokKind::Plus => write!(f, "'+'"),
            TokKind::Minus => write!(f, "'-'"),
            TokKind::Asterisk => write!(f, "'*'"),
            TokKind::Slash => write!(f, "'/'"),
            TokKind::PlusEq => write!(f, "'+='"),
            TokKind::MinusEq => write!(f, "'-='"),
            TokKind::AsteriskEq => write!(f, "'*='"),
            TokKind::SlashEq => write!(f, "'/='"),
            TokKind::Lt => write!(f, "'<'"),
            TokKind::Gt => write!(f, "'>'"),
            TokKind::LtLt => write!(f, "'<<'"),
            TokKind::GtGt => write!(f, "'>>'"),
            TokKind::LtEq => write!(f, "'<='"),
            TokKind::GtEq => write!(f, "'>='"),
            TokKind::AmpersandEq => write!(f, "'&='"),
            TokKind::PipeEq => write!(f, "'|='"),
            TokKind::Eof => write!(f, "end-of-file"),
            TokKind::Error(err) => write!(f, "Error ({})", err),
        }
    }
}

pub struct Lexer {
    reader: BufReader<Box<dyn Read>>,
    line: u32,
    col: u32,

    toks: VecDeque<Token>,
}

impl Lexer {
    pub fn new(reader: Box<dyn Read>) -> Self {
        Self {
            reader: BufReader::new(reader),
            line: 1,
            col: 1,
            toks: VecDeque::new(),
        }
    }

    pub fn peek(&mut self, idx: usize) -> Result<Token, io::Error> {
        while idx >= self.toks.len() {
            let tok = self.read_tok()?;
            self.toks.push_back(tok);
        }

        Ok(self.toks[idx].clone())
    }

    pub fn consume(&mut self) -> Result<Token, io::Error> {
        match self.toks.pop_front() {
            Some(tok) => Ok(tok),
            None => self.read_tok(),
        }
    }

    fn read_tok(&mut self) -> Result<Token, io::Error> {
        self.skip_whitespace()?;

        let line = self.line;
        let col = self.col;

        let maketok = |kind| Token { line, col, kind };

        let ch = match self.peek_ch()? {
            Some(ch) => ch,
            None => return Ok(maketok(TokKind::Eof)),
        };

        let simple = |this: &mut Self, kind: TokKind| {
            this.consume_ch()?;
            Ok(maketok(kind))
        };

        let complex = |this: &mut Self, block: fn(Option<u8>) -> (bool, TokKind)| {
            this.consume_ch()?;
            let ch = this.peek_ch()?;
            let (consume, kind) = block(ch);
            if consume {
                this.consume_ch()?;
            }
            Ok(maketok(kind))
        };

        match ch {
            b'(' => simple(self, TokKind::OpenParen),
            b')' => simple(self, TokKind::CloseParen),
            b'{' => simple(self, TokKind::OpenBrace),
            b'}' => simple(self, TokKind::CloseBrace),
            b';' => simple(self, TokKind::Semicolon),
            b'!' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::NotEq),
                _ => (false, TokKind::Error("Unexpected character".to_string())),
            }),
            b'=' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::EqEq),
                _ => (false, TokKind::Equals),
            }),
            b'+' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::PlusEq),
                _ => (false, TokKind::Plus),
            }),
            b'-' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::MinusEq),
                _ => (false, TokKind::Minus),
            }),
            b'*' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::AsteriskEq),
                _ => (false, TokKind::Asterisk),
            }),
            b'/' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::SlashEq),
                _ => (false, TokKind::Slash),
            }),
            b'<' => complex(self, |ch| match ch {
                Some(b'<') => (true, TokKind::LtLt),
                Some(b'=') => (true, TokKind::LtEq),
                _ => (false, TokKind::Lt),
            }),
            b'>' => complex(self, |ch| match ch {
                Some(b'>') => (true, TokKind::GtGt),
                Some(b'=') => (true, TokKind::GtEq),
                _ => (false, TokKind::Gt),
            }),
            b'&' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::AmpersandEq),
                _ => (false, TokKind::Error("Unexpected character".to_string())),
            }),
            b'|' => complex(self, |ch| match ch {
                Some(b'=') => (true, TokKind::PipeEq),
                _ => (false, TokKind::Error("Unexpected character".to_string())),
            }),
            b'\'' => {
                self.consume_ch()?;
                let ch = match self.peek_ch()? {
                    Some(ch) => ch,
                    None => return Ok(maketok(TokKind::Error("Unexpected EOF".to_string()))),
                };

                let num;
                if ch == b'\\' {
                    self.consume_ch()?;
                    let ch = match self.peek_ch()? {
                        Some(ch) => ch,
                        None => return Ok(maketok(TokKind::Error("Unexpected EOF".to_string()))),
                    };

                    num = match ch {
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b'0' => b'\0',
                        _ => ch,
                    };
                } else {
                    num = ch;
                }

                self.consume_ch()?;
                if self.peek_ch()? != Some(b'\'') {
                    return Ok(maketok(TokKind::Error("Unexpected character".to_string())));
                }

                self.consume_ch()?;
                Ok(maketok(TokKind::Int(num as i32)))
            }
            ch => {
                if ch.is_ascii_alphabetic() || ch == b'_' {
                    let ident = match self.read_ident()? {
                        Some(ident) => ident,
                        None => {
                            return Ok(maketok(TokKind::Error("Invalid identifier".to_string())))
                        }
                    };

                    match ident.as_ref() {
                        "const" => Ok(maketok(TokKind::Const)),
                        "data" => Ok(maketok(TokKind::Data)),
                        "func" => Ok(maketok(TokKind::Func)),
                        "if" => Ok(maketok(TokKind::If)),
                        "else" => Ok(maketok(TokKind::Else)),
                        "while" => Ok(maketok(TokKind::While)),
                        "loop" => Ok(maketok(TokKind::Loop)),
                        "return" => Ok(maketok(TokKind::Return)),
                        "cs" => Ok(maketok(TokKind::Reg(isa::Reg::CS))),
                        "ds" => Ok(maketok(TokKind::Reg(isa::Reg::DS))),
                        "sp" => Ok(maketok(TokKind::Reg(isa::Reg::SP))),
                        "rv" => Ok(maketok(TokKind::Reg(isa::Reg::RV))),
                        "a1" => Ok(maketok(TokKind::Reg(isa::Reg::A1))),
                        "a2" => Ok(maketok(TokKind::Reg(isa::Reg::A2))),
                        "a3" => Ok(maketok(TokKind::Reg(isa::Reg::A3))),
                        "ra" => Ok(maketok(TokKind::Reg(isa::Reg::RA))),
                        _ => Ok(maketok(TokKind::Ident(ident))),
                    }
                } else if ch.is_ascii_digit() {
                    let num = match self.read_number()? {
                        Some(num) => num,
                        None => {
                            return Ok(maketok(TokKind::Error(
                                "Invalid number literal".to_string(),
                            )))
                        }
                    };
                    Ok(maketok(TokKind::Int(num)))
                } else {
                    Ok(maketok(TokKind::Error("Unexpected character".to_string())))
                }
            }
        }
    }

    fn read_ident(&mut self) -> Result<Option<String>, io::Error> {
        let mut buf: Vec<u8> = Vec::new();
        loop {
            let ch = match self.peek_ch()? {
                Some(ch) => ch,
                None => {
                    return match String::from_utf8(buf) {
                        Ok(s) => Ok(Some(s)),
                        Err(_) => Ok(None),
                    }
                }
            };

            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.consume_ch()?;
                buf.push(ch);
            } else {
                return match String::from_utf8(buf) {
                    Ok(s) => Ok(Some(s)),
                    Err(_) => Ok(None),
                };
            }
        }
    }

    fn read_number(&mut self) -> Result<Option<i32>, io::Error> {
        let first = self.peek_ch()?.unwrap();
        self.consume_ch()?;
        let second = self.peek_ch()?;

        let digit = |ch: u8, base: i32| -> Option<u8> {
            let num;
            if ch >= b'0' && ch <= b'9' {
                num = ch - b'0';
            } else if ch >= b'a' && ch <= b'z' {
                num = ch - b'a' + 10;
            } else if ch >= b'A' && ch <= b'Z' {
                num = ch - b'A' + 10;
            } else {
                return None;
            }

            if (num as i32) >= base {
                None
            } else {
                Some(num)
            }
        };

        let mut buf: Vec<u8> = Vec::new();
        let base: i32;
        if first == b'0' && second == Some(b'x') {
            base = 16;
            self.consume_ch()?;
        } else if first == b'0' && second == Some(b'b') {
            base = 2;
            self.consume_ch()?;
        } else if first == b'0' && second == Some(b'o') {
            base = 8;
            self.consume_ch()?;
        } else {
            base = 10;
            buf.push(digit(first, base).unwrap());
        }

        loop {
            let ch = match self.peek_ch()? {
                Some(ch) => ch,
                None => break,
            };

            let d = match digit(ch, base) {
                Some(d) => d,
                None => break,
            };

            self.consume_ch()?;
            buf.push(d);
        }

        if buf.len() == 0 {
            return Ok(None);
        }

        let mut num: i32 = 0;
        for d in buf {
            num *= base;
            num += d as i32;
        }

        Ok(Some(num))
    }

    fn skip_whitespace(&mut self) -> Result<(), io::Error> {
        loop {
            let ch = match self.peek_ch()? {
                None => return Ok(()),
                Some(ch) => ch,
            };

            if ch == b' ' || ch == b'\n' || ch == b'\t' {
                self.consume_ch()?;
                continue;
            }

            let buf = self.reader.fill_buf()?;
            if buf.get(0) == Some(&b'/') && buf.get(1) == Some(&b'/') {
                self.consume_ch()?;
                loop {
                    self.consume_ch()?;
                    let ch = self.peek_ch()?;
                    if ch.is_none() || ch == Some(b'\n') {
                        break;
                    }
                }

                continue;
            }

            break;
        }

        Ok(())
    }

    fn peek_ch(&mut self) -> Result<Option<u8>, io::Error> {
        match self.reader.fill_buf()?.get(0) {
            Some(ch) => Ok(Some(*ch)),
            None => Ok(None),
        }
    }

    fn consume_ch(&mut self) -> Result<(), io::Error> {
        match self.peek_ch() {
            Err(err) => Err(err),
            Ok(None) => Ok(()),
            Ok(Some(ch)) => {
                if ch == b'\n' {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                self.reader.consume(1);
                Ok(())
            }
        }
    }
}
