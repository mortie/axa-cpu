use super::super::assembler;
use super::super::isa;
use super::ast::*;
use std::collections::HashMap;
use std::io::Write;

struct Context<'a> {
    code: Vec<u8>,
    calls: Vec<(usize, String)>,
    program: &'a Program,
}

impl<'a> Context<'a> {
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

    fn generate(&mut self) -> Result<(), String> {
        if !self.program.func_decls.contains_key("main") {
            return Err("No main function".to_string());
        }

        let mut instrs: Vec<u8> = Vec::new();

        // Jump past data/stack
        self.asm("imml 0xfd");
        self.asm("immh 0xfd");
        self.asm("b");

        // Space for data
        let data_base = instrs.len();
        for _ in 0..self.program.data_decls.len() {
            instrs.push(0);
        }

        // Initialize data
        for (_, decl) in &self.program.data_decls {
            let val = (decl.val.eval(self.program)? & 0xff) as u8;
            instrs[data_base + decl.index as usize] = val;
        }

        // Zero out stack
        let stack_base = instrs.len();
        while instrs.len() < 256 {
            instrs.push(0);
        }

        // Set up stack
        self.asm(&format!("imml {}", stack_base));
        self.asm("mov sp acc");

        // Call main
        self.gen_call("main".to_string());

        let mut funcs: HashMap<String, usize> = HashMap::new();

        // Functions
        for (_, func) in &self.program.func_decls {
            funcs.insert(func.name.clone(), self.location());
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
                self.code[loc + 3] |= lo;
                self.code[loc + 4] |= hi;

                let val = (addr & 0x00ff) as u8;
                let lo = val & 0x0f;
                let hi = (val & 0xf0) >> 4;
                self.code[loc + 6] |= lo;
                self.code[loc + 7] |= hi;
            }
        }

        Ok(())
    }
}

fn gen_func_decl(decl: &FuncDecl, ctx: &mut Context) -> Result<(), String> {
    gen_block(&decl.statms, ctx)
}

fn gen_block(block: &Block, ctx: &mut Context) -> Result<(), String> {
    for statm in block {
        gen_statm(statm, ctx)?;
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
            if b.len() != 0 {
                ctx.asm("imml 0");
                ctx.asm("immh 0");
                ctx.asm("b");
            }
            let a_len = ctx.location() - a_start;

            ctx.patch(branch_target_loc, a_len as u8);

            let b_start = ctx.location();
            gen_block(b, ctx)?;
            let b_len = ctx.location() - b_start;
            ctx.patch(a_footer_start, b_len as u8);
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
            ctx.asm(&format!("immh {}", val));
        }
    }

    Ok(())
}
