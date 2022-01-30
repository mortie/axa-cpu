use super::super::isa;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub const_decls: HashMap<String, ConstExpr>,
    pub data_decls: HashMap<String, DataDecl>,
    pub func_decls: Vec<FuncDecl>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            const_decls: HashMap::new(),
            data_decls: HashMap::new(),
            func_decls: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct DataDecl {
    pub val: ConstExpr,
    pub index: u32,
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: String,
    pub statms: Block,
}

pub type Block = Vec<Statm>;

#[derive(Debug)]
pub enum Statm {
    If(Condition, Block, Block),
    Loop(Block),
    While(Condition, Block),
    RegAssign(isa::Reg, isa::RegOp, Acc),
    Load(isa::Reg, Acc),
    Store(Acc, isa::Reg),
    Call(String),
    Return(Option<Acc>),
}

#[derive(Debug, Clone)]
pub enum Acc {
    Reg(isa::Reg),
    Const(ConstExpr),
}

#[derive(Debug, Clone)]
pub enum Condition {
    Eq(isa::Reg, Acc),
    Neq(isa::Reg, Acc),
    Gt(isa::Reg, Acc),
    Ge(isa::Reg, Acc),
    Lt(isa::Reg, Acc),
    Le(isa::Reg, Acc),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    LShift,
    RShift,
}

#[derive(Debug, Clone)]
pub enum ConstExpr {
    Literal(i32),
    Constant(String),
    BinExpr(Box<ConstExpr>, BinOp, Box<ConstExpr>),
}

impl ConstExpr {
    pub fn eval(&self, prog: &Program) -> Result<i32, String> {
        match self {
            ConstExpr::Literal(val) => Ok(*val),
            ConstExpr::Constant(name) => {
                if let Some(decl) = prog.const_decls.get(name) {
                    decl.eval(prog)
                } else if let Some(decl) = prog.data_decls.get(name) {
                    Ok((decl.index + 4) as i32)
                } else {
                    Err(format!("Unknown variable: {}", name))
                }
            }
            ConstExpr::BinExpr(a, op, b) => {
                let a = a.eval(prog)?;
                let b = b.eval(prog)?;
                match op {
                    BinOp::Add => Ok(a + b),
                    BinOp::Sub => Ok(a - b),
                    BinOp::Mul => Ok(a * b),
                    BinOp::Div => Ok(a / b),
                    BinOp::LShift => Ok(a << b),
                    BinOp::RShift => Ok(a >> b),
                }
            }
        }
    }
}
