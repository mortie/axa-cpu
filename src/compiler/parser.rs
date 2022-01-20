use super::ast::{self, Program};
use super::lexer::{Lexer, TokKind, Token};
use std::error;
use std::fmt;
use std::io;

#[derive(Debug)]
pub struct ParseError {
    line: u32,
    col: u32,
    msg: String,
}

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    ParseError(ParseError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Error::IOError(err) => err.fmt(f),
            Error::ParseError(err) => {
                write!(f, "Parse error: {}:{}: {}", err.line, err.col, err.msg)
            }
        }
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::IOError(err)
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Error::ParseError(err)
    }
}

fn err(tok: &Token, msg: String) -> ParseError {
    ParseError {
        line: tok.line,
        col: tok.col,
        msg,
    }
}

fn err_unexpected_token(tok: &Token) -> ParseError {
    err(tok, format!("Unexpected token: {}", tok.kind))
}

fn err_expect_token(tok: &Token, expected: TokKind) -> ParseError {
    err(
        tok,
        format!("Unexpected token: Got {}, expected {}", tok.kind, expected),
    )
}

fn skip(lex: &mut Lexer, expected: TokKind) -> Result<(), Error> {
    let tok = lex.consume()?;
    if tok.kind != expected {
        Err(err_expect_token(&tok, expected).into())
    } else {
        Ok(())
    }
}

fn decl_exists(program: &mut Program, name: &str) -> bool {
    program.const_decls.contains_key(name)
        || program.data_decls.contains_key(name)
        || program.func_decls.contains_key(name)
}

pub fn parse_program(lex: &mut Lexer) -> Result<Program, Error> {
    let mut program = ast::Program::new();

    loop {
        if lex.peek(0)?.kind == TokKind::Eof {
            break;
        }

        parse_top_level(lex, &mut program)?;
    }

    Ok(program)
}

fn parse_top_level(lex: &mut Lexer, program: &mut Program) -> Result<(), Error> {
    let tok = lex.peek(0)?;
    match tok.kind {
        TokKind::Const => parse_const_block(lex, program),
        TokKind::Data => parse_data_block(lex, program),
        TokKind::Func => {
            let decl = parse_func_decl(lex)?;
            if decl_exists(program, &decl.name) {
                return Err(err(&tok, format!("Name '{}' already declared", decl.name)).into());
            }

            program.func_decls.insert(decl.name.clone(), decl);
            Ok(())
        }
        _ => Err(err_unexpected_token(&tok).into()),
    }
}

fn parse_const_block(lex: &mut Lexer, program: &mut Program) -> Result<(), Error> {
    skip(lex, TokKind::Const)?;
    skip(lex, TokKind::OpenBrace)?;

    loop {
        let ident_tok = lex.consume()?;
        match ident_tok.kind {
            TokKind::CloseBrace | TokKind::Eof => break,
            _ => (),
        }

        let ident = match &ident_tok.kind {
            TokKind::Ident(name) => name,
            _ => {
                return Err(err(
                    &ident_tok,
                    format!("Unexpected token: Got {}, expected Ident", ident_tok.kind),
                )
                .into());
            }
        };

        if decl_exists(program, &ident) {
            return Err(err(&ident_tok, format!("Name '{}' already declared", ident)).into());
        }

        skip(lex, TokKind::Equals)?;
        let expr = parse_const_expr(lex)?;
        program.const_decls.insert(ident.clone(), expr);
        skip(lex, TokKind::Semicolon)?;
    }

    Ok(())
}

fn parse_data_block(lex: &mut Lexer, program: &mut Program) -> Result<(), Error> {
    skip(lex, TokKind::Data)?;
    skip(lex, TokKind::OpenBrace)?;

    loop {
        let ident_tok = lex.consume()?;
        match ident_tok.kind {
            TokKind::CloseBrace | TokKind::Eof => break,
            _ => (),
        }

        let ident = match &ident_tok.kind {
            TokKind::Ident(name) => name,
            _ => {
                return Err(err(
                    &ident_tok,
                    format!("Unexpected token: Got {}, expected Ident", ident_tok.kind),
                )
                .into());
            }
        };

        if decl_exists(program, &ident) {
            return Err(err(&ident_tok, format!("Name '{}' already declared", ident)).into());
        }

        skip(lex, TokKind::Equals)?;
        let expr = parse_const_expr(lex)?;
        let index = program.data_decls.len();
        program.data_decls.insert(
            ident.clone(),
            ast::DataDecl {
                val: expr,
                index: index as u32,
            },
        );
        skip(lex, TokKind::Semicolon)?;
    }

    Ok(())
}

fn parse_func_decl(lex: &mut Lexer) -> Result<ast::FuncDecl, Error> {
    skip(lex, TokKind::Func)?;

    let name_tok = lex.consume()?;
    let name = match name_tok.kind {
        TokKind::Ident(name) => name,
        _ => {
            return Err(err(
                &name_tok,
                format!("Unexpected token: Got {}, expected Ident", name_tok.kind),
            )
            .into())
        }
    };

    skip(lex, TokKind::OpenParen)?;
    skip(lex, TokKind::CloseParen)?;

    let statms = parse_block(lex)?;
    Ok(ast::FuncDecl { name, statms })
}

fn parse_block(lex: &mut Lexer) -> Result<ast::Block, Error> {
    skip(lex, TokKind::OpenBrace)?;
    let mut block = ast::Block::new();

    loop {
        let tok = lex.peek(0)?;
        match tok.kind {
            TokKind::CloseBrace => {
                lex.consume()?;
                return Ok(block);
            }
            _ => (),
        }

        block.push(parse_statm(lex)?);
    }
}

fn parse_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    let tok = lex.peek(0)?;
    match tok.kind {
        TokKind::If => parse_if_statm(lex),
        TokKind::Loop => parse_loop_statm(lex),
        TokKind::While => parse_while_statm(lex),
        TokKind::Reg(..) => parse_reg_assign_statm(lex),
        TokKind::Asterisk => parse_store_statm(lex),
        TokKind::Ident(..) => parse_call_statm(lex),
        TokKind::Return => parse_return_statm(lex),
        _ => Err(err_unexpected_token(&tok).into()),
    }
}

fn parse_if_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    skip(lex, TokKind::If)?;
    let cond = parse_cond(lex)?;
    let if_body = parse_block(lex)?;
    let else_body = match lex.peek(0)?.kind {
        TokKind::Else => {
            lex.consume()?;
            if lex.peek(0)?.kind == TokKind::If {
                let mut block = ast::Block::new();
                block.push(parse_if_statm(lex)?);
                block
            } else {
                parse_block(lex)?
            }
        }
        _ => ast::Block::new(),
    };

    Ok(ast::Statm::If(cond, if_body, else_body))
}

fn parse_loop_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    skip(lex, TokKind::Loop)?;
    Ok(ast::Statm::Loop(parse_block(lex)?))
}

fn parse_while_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    skip(lex, TokKind::While)?;
    Ok(ast::Statm::While(parse_cond(lex)?, parse_block(lex)?))
}

fn parse_reg_assign_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    let reg_tok = lex.consume()?;
    let reg = match reg_tok.kind {
        TokKind::Reg(reg) => reg,
        _ => return Err(err_unexpected_token(&reg_tok).into()),
    };

    let op = parse_assign_op(lex)?;

    // If there's an equals followed by asterisk, this is a load statement,
    // not actually an assign statement
    if lex.peek(0)?.kind == TokKind::Asterisk {
        lex.consume()?;
        let acc = parse_accumulator_value(lex)?;
        skip(lex, TokKind::Semicolon)?;
        return Ok(ast::Statm::Load(reg, acc));
    }

    let acc = parse_accumulator_value(lex)?;
    skip(lex, TokKind::Semicolon)?;
    Ok(ast::Statm::RegAssign(reg, op, acc))
}

fn parse_store_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    skip(lex, TokKind::Asterisk)?;

    let dest = parse_accumulator_value(lex)?;
    skip(lex, TokKind::Equals)?;
    let reg_tok = lex.consume()?;
    let reg = match reg_tok.kind {
        TokKind::Reg(reg) => reg,
        _ => return Err(err_unexpected_token(&reg_tok).into()),
    };

    skip(lex, TokKind::Semicolon)?;
    Ok(ast::Statm::Store(dest, reg))
}

fn parse_call_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    let ident_tok = lex.consume()?;
    let ident = match ident_tok.kind {
        TokKind::Ident(ident) => ident,
        _ => return Err(err_unexpected_token(&ident_tok).into()),
    };

    skip(lex, TokKind::OpenParen)?;
    skip(lex, TokKind::CloseParen)?;
    skip(lex, TokKind::Semicolon)?;
    Ok(ast::Statm::Call(ident))
}

fn parse_return_statm(lex: &mut Lexer) -> Result<ast::Statm, Error> {
    skip(lex, TokKind::Return)?;

    if lex.peek(0)?.kind == TokKind::Semicolon {
        lex.consume()?;
        return Ok(ast::Statm::Return(None));
    }

    let acc = parse_accumulator_value(lex)?;
    skip(lex, TokKind::Semicolon)?;
    Ok(ast::Statm::Return(Some(acc)))
}

fn parse_assign_op(lex: &mut Lexer) -> Result<ast::AssignOp, Error> {
    let tok = lex.consume()?;
    match tok.kind {
        TokKind::Equals => Ok(ast::AssignOp::Mov),
        TokKind::PlusEq => Ok(ast::AssignOp::Add),
        TokKind::MinusEq => Ok(ast::AssignOp::Sub),
        TokKind::AmpersandEq => Ok(ast::AssignOp::And),
        TokKind::PipeEq => Ok(ast::AssignOp::Or),
        _ => Err(err_unexpected_token(&tok).into()),
    }
}

fn parse_accumulator_value(lex: &mut Lexer) -> Result<ast::Acc, Error> {
    let tok = lex.peek(0)?;
    match tok.kind {
        TokKind::Reg(reg) => {
            lex.consume()?;
            Ok(ast::Acc::Reg(reg))
        }
        _ => Ok(ast::Acc::Const(parse_const_expr(lex)?)),
    }
}

fn parse_cond(lex: &mut Lexer) -> Result<ast::Condition, Error> {
    let reg_tok = lex.consume()?;
    let reg = match reg_tok.kind {
        TokKind::Reg(reg) => reg,
        _ => {
            return Err(err(
                &reg_tok,
                format!("Unexpected token: Got {}, expected Reg", reg_tok.kind),
            )
            .into());
        }
    };

    let next_tok = lex.peek(0)?;
    match next_tok.kind {
        TokKind::EqEq => {
            lex.consume()?;
            Ok(ast::Condition::Eq(reg, parse_const_expr(lex)?))
        }
        TokKind::NotEq => {
            lex.consume()?;
            Ok(ast::Condition::Neq(reg, parse_const_expr(lex)?))
        }
        TokKind::Lt => {
            lex.consume()?;
            Ok(ast::Condition::Lt(reg, parse_const_expr(lex)?))
        }
        TokKind::Gt => {
            lex.consume()?;
            Ok(ast::Condition::Gt(reg, parse_const_expr(lex)?))
        }
        TokKind::LtEq => {
            lex.consume()?;
            Ok(ast::Condition::Le(reg, parse_const_expr(lex)?))
        }
        TokKind::GtEq => {
            lex.consume()?;
            Ok(ast::Condition::Ge(reg, parse_const_expr(lex)?))
        }
        _ => Ok(ast::Condition::Neq(reg, ast::ConstExpr::Literal(0))),
    }
}

fn parse_const_expr(lex: &mut Lexer) -> Result<ast::ConstExpr, Error> {
    let initial_tok = lex.consume()?;
    let mut expr = match initial_tok.kind {
        TokKind::Minus => ast::ConstExpr::BinExpr(
            Box::new(ast::ConstExpr::Literal(0)),
            ast::BinOp::Sub,
            Box::new(parse_const_expr(lex)?),
        ),
        TokKind::Int(num) => ast::ConstExpr::Literal(num),
        TokKind::Ident(ident) => ast::ConstExpr::Constant(ident),
        TokKind::OpenParen => {
            let expr = parse_const_expr(lex)?;
            skip(lex, TokKind::CloseParen)?;
            expr
        }
        _ => return Err(err_unexpected_token(&initial_tok).into()),
    };

    loop {
        let next_tok = lex.peek(0)?;
        match next_tok.kind {
            TokKind::Plus => {
                lex.consume()?;
                let rhs = parse_const_expr(lex)?;
                expr = ast::ConstExpr::BinExpr(Box::new(expr), ast::BinOp::Add, Box::new(rhs));
            }
            TokKind::Minus => {
                lex.consume()?;
                let rhs = parse_const_expr(lex)?;
                expr = ast::ConstExpr::BinExpr(Box::new(expr), ast::BinOp::Sub, Box::new(rhs));
            }
            TokKind::Asterisk => {
                lex.consume()?;
                let rhs = parse_const_expr(lex)?;
                expr = ast::ConstExpr::BinExpr(Box::new(expr), ast::BinOp::Mul, Box::new(rhs));
            }
            TokKind::Slash => {
                lex.consume()?;
                let rhs = parse_const_expr(lex)?;
                expr = ast::ConstExpr::BinExpr(Box::new(expr), ast::BinOp::Div, Box::new(rhs));
            }
            TokKind::LtLt => {
                lex.consume()?;
                let rhs = parse_const_expr(lex)?;
                expr = ast::ConstExpr::BinExpr(Box::new(expr), ast::BinOp::LShift, Box::new(rhs));
            }
            TokKind::GtGt => {
                lex.consume()?;
                let rhs = parse_const_expr(lex)?;
                expr = ast::ConstExpr::BinExpr(Box::new(expr), ast::BinOp::RShift, Box::new(rhs));
            }
            _ => return Ok(expr),
        }
    }
}
