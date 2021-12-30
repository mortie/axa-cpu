use super::super::isa;
use std::collections::VecDeque;
use std::io;
use std::io::{BufRead, BufReader, Read};

#[derive(Clone)]
pub struct Token {
    line: u32,
    col: u32,
    kind: TokKind,
}

#[derive(Clone)]
pub enum TokKind {
    Ident(String),
    Reg(isa::Reg),
    Int(i32),
    Const,
    Data,
    Func,
    OpenParen,
    CloseParen,
    Semicolon,
    Equals,
    DblEquals,
    Plus,
    Minus,
    Asterisk,
    Slash,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    SlashEquals,
    Eof,
    Error(String),
}

struct Lexer<R>
where
    R: Read,
{
    reader: BufReader<R>,
    line: u32,
    col: u32,

    toks: VecDeque<Token>,
}

impl<R> Lexer<R>
where
    R: Read,
{
    pub fn new(reader: R) -> Self {
        Self {
            reader: BufReader::new(reader),
            line: 1,
            col: 1,
            toks: VecDeque::new(),
        }
    }

    pub fn peek(&mut self, idx: usize) -> Result<Token, io::Error> {
        while idx <= self.toks.len() {
            let tok = self.read_tok()?;
            self.toks.push_back(tok);
        }

        Ok(self.toks[idx].clone())
    }

    pub fn consume(&mut self) {
        let _ = self.toks.pop_front();
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

        match ch {
            b'(' => {
                self.consume_ch()?;
                Ok(maketok(TokKind::OpenParen))
            }
            b')' => {
                self.consume_ch()?;
                Ok(maketok(TokKind::CloseParen))
            }
            b';' => {
                self.consume_ch()?;
                Ok(maketok(TokKind::Semicolon))
            }
            b'=' => {
                self.consume_ch()?;
                match self.peek_ch()? {
                    Some(b'=') => {
                        self.consume_ch()?;
                        Ok(maketok(TokKind::DblEquals))
                    }
                    _ => Ok(maketok(TokKind::Equals)),
                }
            }
            b'+' => {
                self.consume_ch()?;
                match self.peek_ch()? {
                    Some(b'=') => {
                        self.consume_ch()?;
                        Ok(maketok(TokKind::PlusEquals))
                    }
                    _ => Ok(maketok(TokKind::Plus)),
                }
            }
            b'-' => {
                self.consume_ch()?;
                match self.peek_ch()? {
                    Some(b'=') => {
                        self.consume_ch()?;
                        Ok(maketok(TokKind::MinusEquals))
                    }
                    _ => Ok(maketok(TokKind::Minus)),
                }
            }
            b'*' => {
                self.consume_ch()?;
                match self.peek_ch()? {
                    Some(b'=') => {
                        self.consume_ch()?;
                        Ok(maketok(TokKind::AsteriskEquals))
                    }
                    _ => Ok(maketok(TokKind::Asterisk)),
                }
            }
            b'/' => {
                self.consume_ch()?;
                match self.peek_ch()? {
                    Some(b'=') => {
                        self.consume_ch()?;
                        Ok(maketok(TokKind::SlashEquals))
                    }
                    _ => Ok(maketok(TokKind::Slash)),
                }
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

        let digit = |ch: u8, base: u8| -> Option<u8> {
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

            if num >= base {
                None
            } else {
                Some(num)
            }
        };

        let mut buf: Vec<u8> = Vec::new();
        let base;
        if first == b'0' && second == Some(b'x') {
            base = 16;
        } else if first == b'0' && second == Some(b'b') {
            base = 2;
        } else if first == b'0' && second == Some(b'o') {
            base = 8;
        } else {
            base = 10;
            let d = match digit(first, base) {
                Some(d) => d,
                None => return Ok(None),
            };
            buf.push(d);
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

            buf.push(d);
        }

        if buf.len() == 0 {
            return Ok(None);
        }

        let mut num: i32 = 0;
        for d in buf {
            num *= 10;
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
                Ok(())
            }
        }
    }
}
