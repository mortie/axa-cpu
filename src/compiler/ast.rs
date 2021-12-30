use super::super::isa;
use std::collections::HashMap;

pub struct Program {
    const_decls: HashMap<String, ConstExpr>,
    data_decls: HashMap<String, DataDecl>,
    func_decls: HashMap<String, FuncDecl>,
}

impl Program {
    fn new() -> Self {
        Self {
            const_decls: HashMap::new(),
            data_decls: HashMap::new(),
            func_decls: HashMap::new(),
        }
    }
}

pub struct DataDecl {
    val: ConstExpr,
    addr: u32,
}

pub struct FuncDecl {
    name: String,
    statms: Block,
}

pub type Block = Vec<Statm>;

pub enum Statm {
    If(Condition, Block, Block),
    Loop(Block),
    While(Condition, Block),
    RegAssign(isa::Reg, AssignOp, Acc),
    Load(isa::Reg, Acc),
    Store(Acc, isa::Reg),
}

pub enum AssignOp {
    Mov,
    Add,
    Sub,
    And,
    Or,
}

pub enum Acc {
    Reg(isa::Reg),
    Const(ConstExpr),
}

pub enum Condition {
    True(isa::Reg),
    False(isa::Reg),
    Eq(isa::Reg, ConstExpr),
    Gt(isa::Reg, ConstExpr),
    Ge(isa::Reg, ConstExpr),
    Lt(isa::Reg, ConstExpr),
    Le(isa::Reg, ConstExpr),
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    LShift,
    RShift,
}

pub enum ConstExpr {
    Literal(i32),
    Constant(String),
    BinExpr(Box<ConstExpr>, BinOp, Box<ConstExpr>),
}

impl ConstExpr {
    fn eval(&self, prog: &Program) -> Result<i32, String> {
        match self {
            ConstExpr::Literal(val) => Ok(*val),
            ConstExpr::Constant(name) => {
                if let Some(decl) = prog.const_decls.get(name) {
                    decl.eval(prog)
                } else if let Some(decl) = prog.data_decls.get(name) {
                    Ok(decl.addr as i32)
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
