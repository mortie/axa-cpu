use super::super::assembler;
use super::super::isa;
use super::ast::*;
use std::collections::HashMap;
use std::io::Write;

pub struct Context<'a> {
    pub code: Vec<u8>,
    calls: Vec<(usize, String)>,
    program: &'a Program,
    pub annotations: HashMap<u16, String>,
    optimize: bool,
}

impl<'a> Context<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            code: Vec::new(),
            calls: Vec::new(),
            program,
            annotations: HashMap::new(),
            optimize: true,
        }
    }

    fn location(&self) -> usize {
        self.code.len()
    }

    fn patch(&mut self, loc: usize, val: u8) {
        let lo = val & 0x0f;
        let hi = (val & 0xf0) >> 4;
        self.code[loc] |= lo;
        self.code[loc + 1] |= hi;
    }

    fn asm(&mut self, s: &str) {
        let mut ctx = assembler::Context::new();
        if let Err(err) = assembler::assemble_line(s, &mut self.code, &mut ctx) {
            panic!("Assembler error: {}", err);
        }
    }

    fn instr(&mut self, instr: isa::Instr) {
        if let Err(err) = self.code.write(&[instr.format()]) {
            panic!("{}", err); // This can't really happen with Vec I think
        }
    }

    fn annotate(&mut self, text: String) {
        self.annotations.insert(self.code.len() as u16, text);
    }

    fn gen_call(&mut self, target: String) {
        self.calls.push((self.location(), target));
        self.asm("mov acc cs");
        self.asm("mov rv acc");
        self.asm("imml 0");
        self.asm("immh 0");
        self.asm("mov cs acc");
        self.asm("imml 0");
        self.asm("immh 0");
        self.asm("call");
    }

    pub fn generate(&mut self) -> Result<(), String> {
        if !self.program.func_decls.contains_key("main") {
            return Err("No main function".to_string());
        }

        // Jump past data/stack
        self.annotate("Start".to_string());
        self.asm("imml 0x01");
        self.asm("mov cs acc");
        self.asm("imml 0");
        self.asm("jmp");

        // Space for data
        self.annotate("Data".to_string());
        let data_base = self.code.len();
        for _ in 0..self.program.data_decls.len() {
            self.code.push(0);
        }

        // Initialize data
        for (_, decl) in &self.program.data_decls {
            let val = (decl.val.eval(self.program)? & 0xff) as u8;
            self.code[data_base + decl.index as usize] = val;
        }

        // Zero out stack
        self.annotate("Stack".to_string());
        let stack_base = self.code.len();
        while self.code.len() < 256 {
            self.code.push(0);
        }

        // Set up stack
        self.annotate("Initialize".to_string());
        self.asm(&format!("imml {}", stack_base));
        self.asm("mov sp acc");

        // Call main
        self.gen_call("main".to_string());

        let mut funcs: HashMap<String, usize> = HashMap::new();

        // Functions
        for (_, func) in &self.program.func_decls {
            funcs.insert(func.name.clone(), self.location());
            self.annotate(format!("Func {}", func.name));
            gen_func_decl(func, self)?;
        }

        // Fix up function calls
        for (loc, name) in &self.calls {
            if let Some(addr) = funcs.get(name) {
                /* Rust doesn't let me write this code:
                    self.patch(loc + 3, ((addr & 0xff00) >> 8) as u8);
                    self.patch(loc + 6, (addr & 0x00ff) as u8);
                */

                let val = ((addr & 0xff00) >> 8) as u8;
                let lo = val & 0x0f;
                let hi = (val & 0xf0) >> 4;
                self.code[loc + 2] |= lo;
                self.code[loc + 3] |= hi;

                let val = (addr & 0x00ff) as u8;
                let lo = val & 0x0f;
                let hi = (val & 0xf0) >> 4;
                self.code[loc + 5] |= lo;
                self.code[loc + 6] |= hi;
            }
        }

        Ok(())
    }
}

fn gen_func_decl(decl: &FuncDecl, ctx: &mut Context) -> Result<(), String> {
    gen_block(&decl.statms, ctx)
}

fn optimize(unoptimized: &[u8]) -> Vec<u8> {
    use isa::{ImmOp, Instr};

    let mut optimized: Vec<u8> = Vec::new();
    let mut acc_state: Option<u8> = None;

    optimized.reserve(unoptimized.len());
    let mut idx = 0usize;
    while idx < unoptimized.len() {
        let ibyte = unoptimized[idx];
        let instr = isa::Instr::parse(ibyte);

        let next_instr;
        if idx < unoptimized.len() - 1 {
            next_instr = Some(isa::Instr::parse(unoptimized[idx + 1]));
        } else {
            next_instr = None;
        }

        match (instr, next_instr) {
            (Instr::Imm(ImmOp::Imml, imml), Some(Instr::Imm(ImmOp::Immh, immh)))
                if Some(imml | immh) == acc_state =>
            {
                idx += 2
            }
            (Instr::Imm(ImmOp::Imml, imml), ..) if Some(imml) == acc_state => idx += 1,
            (Instr::Imm(ImmOp::Imml, imml), ..) => {
                acc_state = Some(imml);
                optimized.push(ibyte);
                idx += 1;
            }
            (Instr::Imm(ImmOp::Immh, immh), ..) if acc_state.is_some() => {
                acc_state = Some(acc_state.unwrap() | immh);
                optimized.push(ibyte);
                idx += 1;
            }
            _ => {
                optimized.push(ibyte);
                idx += 1;
            }
        }
    }

    optimized
}

fn gen_block(block: &Block, ctx: &mut Context) -> Result<(), String> {
    if !ctx.optimize {
        for statm in block {
            gen_statm(statm, ctx)?;
        }

        return Ok(());
    }

    let mut basic_block_start = ctx.location();
    for statm in block {
        let is_control_flow = match statm {
            Statm::If(..) => true,
            Statm::Loop(..) => true,
            Statm::While(..) => true,
            Statm::RegAssign(..) => false,
            Statm::Load(..) => false,
            Statm::Store(..) => false,
            Statm::Call(..) => true,
            Statm::Return(..) => true,
        };

        if is_control_flow {
            let mut optimized = optimize(&ctx.code[basic_block_start..]);
            ctx.code.truncate(basic_block_start);
            ctx.code.append(&mut optimized);
            gen_statm(statm, ctx)?;
            basic_block_start = ctx.location();
        } else {
            gen_statm(statm, ctx)?;
        }
    }

    Ok(())
}

fn gen_statm(statm: &Statm, ctx: &mut Context) -> Result<(), String> {
    match statm {
        Statm::If(cond, a, b) => {
            let branch_target_loc = gen_branch_if_not_cond(cond, ctx)?;

            let a_start = ctx.location();
            gen_block(a, ctx)?;
            let a_footer_start = ctx.location();
            let has_b = b.len() > 0;
            if has_b {
                ctx.asm("imml 0");
                ctx.asm("immh 0");
                ctx.asm("b");
            }
            let a_len = ctx.location() - a_start;

            ctx.patch(branch_target_loc, a_len as u8);

            if has_b {
                let b_start = ctx.location();
                gen_block(b, ctx)?;
                let b_len = ctx.location() - b_start;
                ctx.patch(a_footer_start, b_len as u8);
            }
        }
        Statm::Loop(block) => {
            let start = ctx.location();
            gen_block(block, ctx)?;
            let branch_target_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            let len = ctx.location() - start;
            ctx.asm("bb");
            ctx.patch(branch_target_loc, (!(len as u8)).wrapping_add(1));
        }
        Statm::While(cond, block) => {
            let start = ctx.location();
            let branch_target_loc = gen_branch_if_not_cond(cond, ctx)?;
            gen_block(block, ctx)?;
            let branch_back_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            ctx.asm("b");

            let len = start - ctx.location();
            ctx.patch(branch_target_loc, len as u8);
            ctx.patch(branch_back_loc, !(len as u8).wrapping_add(1));
        }
        Statm::RegAssign(reg, op, acc) => {
            gen_acc(acc, ctx)?;
            let regop = match op {
                AssignOp::Mov => isa::RegOp::Mov,
                AssignOp::Add => isa::RegOp::Add,
                AssignOp::Sub => isa::RegOp::Sub,
                AssignOp::And => isa::RegOp::And,
                AssignOp::Or => isa::RegOp::Or,
            };
            ctx.instr(isa::Instr::Reg(regop, false, *reg));
        }

        // TODO: These have to know whether relative or not
        Statm::Load(reg, acc) => {
            gen_acc(acc, ctx)?;
            ctx.instr(isa::Instr::Mem(isa::MemOp::Ld, true, *reg));
        }
        Statm::Store(acc, reg) => {
            gen_acc(acc, ctx)?;
            ctx.instr(isa::Instr::Mem(isa::MemOp::St, true, *reg));
        }
        Statm::Call(name) => {
            ctx.gen_call(name.clone());
        }
        Statm::Return(val) => {
            ctx.asm("mov acc rv");
            ctx.asm("mov cs acc");
            match val {
                Some(val) => {
                    gen_acc(val, ctx)?;
                    ctx.asm("mov rv acc");
                }
                _ => (),
            }

            ctx.asm("mov acc ra");
            ctx.asm("jmp");
        }
    }

    Ok(())
}

fn gen_branch_if_not_cond(cond: &Condition, ctx: &mut Context) -> Result<usize, String> {
    let val = match cond {
        Condition::Eq(_, expr) => expr,
        Condition::Neq(_, expr) => expr,
        Condition::Gt(_, expr) => expr,
        Condition::Ge(_, expr) => expr,
        Condition::Lt(_, expr) => expr,
        Condition::Le(_, expr) => expr,
    }
    .eval(ctx.program)?;

    ctx.asm(&format!("imml {}", val));
    ctx.asm(&format!("immh {}", val));

    let target_loc;
    match cond {
        Condition::Eq(reg, _) => {
            ctx.instr(isa::Instr::Reg(isa::RegOp::Cmp, false, *reg));
            ctx.asm("imml 3");
            ctx.asm("beq");
            target_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            ctx.asm("b");
        }

        Condition::Neq(reg, _) => {
            ctx.instr(isa::Instr::Reg(isa::RegOp::Cmp, false, *reg));
            target_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            ctx.asm("beq");
        }

        Condition::Gt(reg, _) => {
            ctx.instr(isa::Instr::Reg(isa::RegOp::Cmp, false, *reg));
            target_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            ctx.asm("bgt");
        }

        Condition::Ge(reg, _) => {
            ctx.instr(isa::Instr::Reg(isa::RegOp::Cmp, false, *reg));
            target_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            ctx.asm("bge");
        }

        Condition::Lt(reg, _) => {
            ctx.instr(isa::Instr::Reg(isa::RegOp::Cmp, true, *reg));
            target_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            ctx.asm("bgt");
        }

        Condition::Le(reg, _) => {
            ctx.instr(isa::Instr::Reg(isa::RegOp::Cmp, true, *reg));
            target_loc = ctx.location();
            ctx.asm("imml 0");
            ctx.asm("immh 0");
            ctx.asm("bge");
        }
    }

    Ok(target_loc)
}

fn gen_acc(acc: &Acc, ctx: &mut Context) -> Result<(), String> {
    match acc {
        Acc::Reg(reg) => {
            ctx.instr(isa::Instr::Reg(isa::RegOp::Mov, true, *reg));
        }
        Acc::Const(expr) => {
            let val = expr.eval(ctx.program)?;
            ctx.asm(&format!("imml {}", val));
            if val > 0x0f {
                ctx.asm(&format!("immh {}", val));
            }
        }
    }

    Ok(())
}
