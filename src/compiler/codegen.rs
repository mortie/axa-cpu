use super::super::isa;
use super::ast;
use std::collections::HashMap;

pub enum Annotation {
    Indent(String),
    Dedent,
}

pub struct Code {
    pub code: Vec<u8>,
    pub annotations: Vec<(u16, Annotation)>,
    location: u16,
}

impl Code {
    fn new(location: u16) -> Self {
        Self {
            code: Vec::new(),
            annotations: Vec::new(),
            location,
        }
    }

    fn fork(&self) -> Self {
        Self {
            code: Vec::new(),
            annotations: Vec::new(),
            location: self.location,
        }
    }

    fn merge(&mut self, other: Self) {
        for ibyte in other.code {
            self.code.push(ibyte);
            self.location += 1;
        }

        for annot in other.annotations {
            self.annotations.push(annot);
        }

        assert_eq!(self.location, other.location);
    }

    fn indent(&mut self, text: String) {
        self.annotations.push((self.location, Annotation::Indent(text)));
    }

    fn dedent(&mut self) {
        self.annotations.push((self.location, Annotation::Dedent));
    }

    fn instr(&mut self, instr: isa::Instr) {
        self.code.push(instr.format());
        self.location += 1;
    }

    fn imml(&mut self, val: u8) {
        self.instr(isa::Instr::Imm(isa::ImmOp::Imml, val));
    }

    fn immh(&mut self, val: u8) {
        self.instr(isa::Instr::Imm(isa::ImmOp::Immh, val));
    }

    fn reg_op_acc(&mut self, op: isa::RegOp, reg: isa::Reg) {
        self.instr(isa::Instr::Reg(op, false, reg));
    }

    fn acc_op_reg(&mut self, op: isa::RegOp, reg: isa::Reg) {
        self.instr(isa::Instr::Reg(op, true, reg));
    }

    fn nop(&mut self) {
        self.immh(0);
    }

    fn pad(&mut self, count: usize) {
        for _ in 0..count {
            self.nop();
        }
    }
}

pub struct CpuState {
    acc: Option<u8>,
    touched_acc: bool,
    regs: [Option<u8>; 8],
    touched_regs: [bool; 8],
    returned: bool,
}

impl CpuState {
    fn new() -> Self {
        Self {
            acc: None,
            touched_acc: false,
            regs: [None; 8],
            touched_regs: [false; 8],
            returned: false,
        }
    }

    fn new_cluttered() -> Self {
        Self {
            acc: None,
            touched_acc: true,
            regs: [None; 8],
            touched_regs: [true; 8],
            returned: false,
        }
    }

    fn merge(&mut self, other: &Self) {
        if other.touched_acc {
            self.touched_acc = true;
            if self.acc != other.acc {
                self.acc = None;
            }
        }

        for reg in 0..8 {
            if other.touched_regs[reg] {
                self.touched_regs[reg] = true;
                if self.regs[reg] != other.regs[reg] {
                    self.regs[reg] = None;
                }
            }
        }

        if !other.returned {
            self.returned = false;
        }
    }

    fn fork(&self) -> Self {
        Self {
            acc: self.acc,
            touched_acc: false,
            regs: self.regs,
            touched_regs: [false; 8],
            returned: false,
        }
    }

    fn set_acc_to_val(&mut self, val: u8, code: &mut Code) {
        if self.acc == Some(val) {
            return;
        }

        match self.acc {
            None => {
                code.imml(val);
                if val > 0x0f {
                    code.immh(val);
                }
            }
            Some(old) => {
                if old | (val << 4) == val {
                    code.immh(val);
                } else if val & 0x0f == val {
                    code.imml(val);
                } else {
                    code.imml(val);
                    code.immh(val);
                }
            }
        }

        self.touched_acc = true;
        self.acc = Some(val);
    }

    fn set_acc_to_reg(&mut self, reg: isa::Reg, code: &mut Code) {
        if self.acc.is_some() && self.acc == self.regs[reg as usize] {
            return;
        }

        code.acc_op_reg(isa::RegOp::Mov, reg);
        self.touched_acc = true;
        self.acc = self.regs[reg as usize];
    }

    fn set_reg_to_val(&mut self, reg: isa::Reg, val: u8, code: &mut Code) {
        if self.regs[reg as usize] == Some(val) {
            return;
        }

        self.set_acc_to_val(val, code);
        code.reg_op_acc(isa::RegOp::Mov, reg);
        self.touched_regs[reg as usize] = true;
        self.regs[reg as usize] = Some(val);
    }

    fn set_reg_to_reg(&mut self, dest: isa::Reg, src: isa::Reg, code: &mut Code) {
        if self.regs[dest as usize].is_some() && self.regs[dest as usize] == self.regs[src as usize]
        {
            return;
        }

        self.set_acc_to_reg(src, code);
        code.reg_op_acc(isa::RegOp::Mov, dest);
        self.touched_regs[dest as usize] = true;
        self.regs[dest as usize] = self.regs[src as usize];
    }

    fn reg_op_val(&mut self, dest: isa::Reg, op: isa::RegOp, src: u8, code: &mut Code) {
        if op == isa::RegOp::Mov {
            self.set_reg_to_val(dest, src, code);
            return;
        }

        self.set_acc_to_val(src, code);
        code.reg_op_acc(op.clone(), dest);

        match op {
            isa::RegOp::Cmp | isa::RegOp::Cmpc => (),
            _ => {
                self.touched_regs[dest as usize] = true;
                self.regs[dest as usize] = None;
            }
        }
    }

    fn reg_op_reg(&mut self, dest: isa::Reg, op: isa::RegOp, src: isa::Reg, code: &mut Code) {
        if op == isa::RegOp::Mov {
            self.set_reg_to_reg(dest, src, code);
            return;
        }

        self.set_acc_to_reg(src, code);
        code.reg_op_acc(op.clone(), dest);

        match op {
            isa::RegOp::Cmp | isa::RegOp::Cmpc => (),
            _ => {
                self.touched_regs[dest as usize] = true;
                self.regs[dest as usize] = None;
            }
        }
    }

    fn reg_load(&mut self, reg: isa::Reg, dbit: bool, code: &mut Code) {
        code.instr(isa::Instr::Mem(isa::MemOp::Ld, dbit, reg));
        self.touched_regs[reg as usize] = true;
        self.regs[reg as usize] = None;
    }

    fn reg_store(&mut self, reg: isa::Reg, dbit: bool, code: &mut Code) {
        code.instr(isa::Instr::Mem(isa::MemOp::St, dbit, reg));
    }

    fn stack_push(&mut self, reg: isa::Reg, code: &mut Code) {
        self.set_acc_to_reg(isa::Reg::SP, code);
        self.reg_store(reg, false, code);
        self.reg_op_val(isa::Reg::SP, isa::RegOp::Add, 1, code);
    }

    fn stack_pop(&mut self, reg: isa::Reg, code: &mut Code) {
        self.reg_op_val(isa::Reg::SP, isa::RegOp::Sub, 1, code);
        self.set_acc_to_reg(isa::Reg::SP, code);
        self.reg_load(reg, false, code);
    }

    fn clobber_all(&mut self) {
        self.acc = None;
        for reg in 0..8 {
            self.regs[reg as usize] = None;
        }
    }

    fn touch_all(&mut self) {
        self.touched_acc = true;
        for reg in 0..8 {
            self.touched_regs[reg as usize] = true;
        }
    }
}

struct FunctionInfo {
    addr: u16,
    length: u16,
    post_state: CpuState,
    is_leaf: bool,
}

impl FunctionInfo {
    fn new() -> Self {
        Self {
            addr: 0,
            length: 0,
            post_state: CpuState::new(),
            is_leaf: false,
        }
    }
}

pub struct Context {
    pub program: ast::Program,
    functions: Vec<FunctionInfo>,
    function_names: HashMap<String, usize>,
}

impl Context {
    pub fn new(program: ast::Program) -> Self {
        Self {
            program,
            functions: Vec::new(),
            function_names: HashMap::new(),
        }
    }

    fn current_func(&self) -> &FunctionInfo {
        return self.functions.last().unwrap();
    }
}

fn expr_is_zero_page(expr: &ast::ConstExpr, prog: &ast::Program) -> bool {
    match expr {
        ast::ConstExpr::Literal(..) => false,
        ast::ConstExpr::Constant(name) => {
            if prog.const_decls.contains_key(name) {
                false
            } else if prog.data_decls.contains_key(name) {
                true
            } else {
                false
            }
        }
        ast::ConstExpr::BinExpr(a, .., b) => {
            expr_is_zero_page(a, prog) || expr_is_zero_page(b, prog)
        }
    }
}

fn expr_get_dbit(expr: &ast::ConstExpr, prog: &ast::Program) -> bool {
    !expr_is_zero_page(expr, prog)
}

fn acc_get_dbit(acc: &ast::Acc, prog: &ast::Program) -> bool {
    match acc {
        ast::Acc::Const(expr) => expr_get_dbit(expr, prog),
        _ => false,
    }
}

fn gen_set_reg(
    ctx: &Context,
    state: &mut CpuState,
    reg: isa::Reg,
    acc: &ast::Acc,
    code: &mut Code,
) -> Result<(), String> {
    match acc {
        ast::Acc::Const(expr) => state.set_reg_to_val(reg, expr.eval(&ctx.program)? as u8, code),
        ast::Acc::Reg(src) => state.set_reg_to_reg(reg, *src, code),
    }

    Ok(())
}

fn gen_set_acc(
    ctx: &Context,
    state: &mut CpuState,
    acc: &ast::Acc,
    code: &mut Code,
) -> Result<(), String> {
    match acc {
        ast::Acc::Const(expr) => state.set_acc_to_val(expr.eval(&ctx.program)? as u8, code),
        ast::Acc::Reg(src) => state.set_acc_to_reg(*src, code),
    }

    Ok(())
}

fn block_has_func_call(block: &ast::Block) -> bool {
    for statm in block {
        match statm {
            ast::Statm::If(_cond, a, b) => {
                if block_has_func_call(a) || block_has_func_call(b) {
                    return true;
                }
            }
            ast::Statm::Loop(block) => {
                if block_has_func_call(block) {
                    return true;
                }
            }
            ast::Statm::While(_cond, block) => {
                if block_has_func_call(block) {
                    return true;
                }
            }
            ast::Statm::RegAssign(..) => (),
            ast::Statm::Load(..) => (),
            ast::Statm::Store(..) => (),
            ast::Statm::Call(..) => return true,
            ast::Statm::Return(..) => (),
        }
    }

    false
}

pub fn generate(mut ctx: Context) -> Result<Code, String> {
    let func_decls = ctx.program.func_decls;
    ctx.program.func_decls = vec![];
    let mut code = Code::new(0);
    for func in &func_decls {
        let func_info = FunctionInfo {
            addr: code.location,
            length: 0,
            post_state: CpuState::new_cluttered(),
            is_leaf: !block_has_func_call(&func.statms),
        };

        ctx.function_names
            .insert(func.name.clone(), ctx.functions.len());
        ctx.functions.push(func_info);

        let mut state = CpuState::new();
        let start = code.location;
        gen_function(&mut ctx, &mut state, func, &mut code)?;
        ctx.functions.last_mut().unwrap().post_state = state;
        ctx.functions.last_mut().unwrap().length = code.location - start;
    }

    ctx.program.func_decls = func_decls;
    Ok(code)
}

fn gen_function(
    ctx: &Context,
    state: &mut CpuState,
    decl: &ast::FuncDecl,
    code: &mut Code,
) -> Result<(), String> {
    code.indent(format!("Func {}", decl.name));
    if !ctx.current_func().is_leaf {
        state.stack_push(isa::Reg::RA, code); // Push return address
        state.stack_push(isa::Reg::RV, code); // Push return segment (stored in RV)
    }

    gen_block(ctx, state, &decl.statms, code)?;

    // Need to generate an implicit return if there are paths which don't return
    if !state.returned {
        gen_return_statm(ctx, state, &None, code)?;
    }

    code.dedent();
    Ok(())
}

fn gen_block(
    ctx: &Context,
    state: &mut CpuState,
    block: &ast::Block,
    code: &mut Code,
) -> Result<(), String> {
    for statm in block {
        gen_statm(ctx, state, statm, code)?;
    }

    Ok(())
}

fn gen_statm(
    ctx: &Context,
    state: &mut CpuState,
    statm: &ast::Statm,
    code: &mut Code,
) -> Result<(), String> {
    match statm {
        ast::Statm::If(cond, if_block, else_block) => {
            gen_if_statm(ctx, state, cond, if_block, else_block, code)
        }
        ast::Statm::Loop(block) => gen_loop_statm(ctx, state, block, code),
        ast::Statm::While(cond, block) => gen_while_statm(ctx, state, cond, block, code),
        ast::Statm::RegAssign(reg, op, acc) => gen_reg_assign_statm(ctx, state, reg, op, acc, code),
        ast::Statm::Load(reg, acc) => gen_load_statm(ctx, state, reg, acc, code),
        ast::Statm::Store(acc, reg) => gen_store_statm(ctx, state, acc, reg, code),
        ast::Statm::Call(name) => gen_call_statm(ctx, state, name, code),
        ast::Statm::Return(ret) => gen_return_statm(ctx, state, ret, code),
    }
}

fn gen_jump_unless(
    ctx: &Context,
    state: &mut CpuState,
    cond: &ast::Condition,
    dist: i8,
    code: &mut Code,
) -> Result<(), String> {
    let acc = match cond {
        ast::Condition::Eq(_, acc) => acc,
        ast::Condition::Neq(_, acc) => acc,
        ast::Condition::Gt(_, acc) => acc,
        ast::Condition::Ge(_, acc) => acc,
        ast::Condition::Lt(_, acc) => acc,
        ast::Condition::Le(_, acc) => acc,
    };

    let (dbit, dist) = if dist < 0 {
        (true, (dist & 0x0f) as u8)
    } else {
        (false, dist as u8)
    };

    gen_set_acc(ctx, state, acc, code)?;
    match cond {
        ast::Condition::Eq(..) => return Err("Unsupported condition".to_string()),
        ast::Condition::Neq(reg, _) => {
            code.instr(isa::Instr::Reg(isa::RegOp::Cmp, false, *reg));
            state.set_acc_to_val(dist, code);
            code.instr(isa::Instr::Branch(isa::BranchOp::Beq, dbit));
        }
        ast::Condition::Gt(reg, _) => {
            code.instr(isa::Instr::Reg(isa::RegOp::Cmp, false, *reg));
            state.set_acc_to_val(dist, code);
            code.instr(isa::Instr::Branch(isa::BranchOp::Bgt, dbit));
        }
        ast::Condition::Ge(reg, _) => {
            code.instr(isa::Instr::Reg(isa::RegOp::Cmp, false, *reg));
            state.set_acc_to_val(dist, code);
            code.instr(isa::Instr::Branch(isa::BranchOp::Bge, dbit));
        }
        ast::Condition::Lt(reg, _) => {
            code.instr(isa::Instr::Reg(isa::RegOp::Cmp, true, *reg));
            state.set_acc_to_val(dist, code);
            code.instr(isa::Instr::Branch(isa::BranchOp::Bgt, dbit));
        }
        ast::Condition::Le(reg, _) => {
            code.instr(isa::Instr::Reg(isa::RegOp::Cmp, true, *reg));
            state.set_acc_to_val(dist, code);
            code.instr(isa::Instr::Branch(isa::BranchOp::Bge, dbit));
        }
    }

    Ok(())
}

fn gen_if_statm(
    ctx: &Context,
    state: &mut CpuState,
    cond: &ast::Condition,
    if_block: &ast::Block,
    else_block: &ast::Block,
    code: &mut Code,
) -> Result<(), String> {
    code.indent("If".to_string());
    let (if_block, else_block, cond) = if let ast::Condition::Eq(reg, acc) = cond {
        (else_block, if_block, ast::Condition::Neq(*reg, acc.clone()))
    } else {
        (if_block, else_block, cond.clone())
    };

    let mut initial_if_length = 0;
    let mut initial_else_length = 0;

    let mut current_state;
    let mut current_code;
    loop {
        current_state = state.fork();
        current_code = code.fork();
        gen_jump_unless(
            ctx,
            &mut current_state,
            &cond,
            (initial_if_length + 1) as i8,
            &mut current_code,
        )?;
        let mut else_state = current_state.fork();
        let start = current_code.location;
        current_code.indent("If-block".to_string());
        gen_block(ctx, &mut current_state, if_block, &mut current_code)?;
        current_code.dedent();

        let mut length;
        if else_block.len() > 0 {
            // Generate a fake jump, so we know its length
            let mut ns = current_state.fork();
            let mut nc = current_code.fork();
            ns.set_acc_to_val((initial_else_length + 1) as u8, &mut nc);
            nc.instr(isa::Instr::Branch(isa::BranchOp::B, false));
            length = nc.location - start;

            // Pad out with nops if necessary
            while length < initial_if_length {
                current_code.nop();
                length += 1;
            }

            // Generate the actual jump
            current_state.set_acc_to_val((initial_else_length + 1) as u8, &mut current_code);
            current_code.instr(isa::Instr::Branch(isa::BranchOp::B, false));
            assert_eq!(current_code.location, length + start);
        } else {
            length = current_code.location - start;
            while length < initial_if_length {
                current_code.nop();
                length += 1;
            }
        }

        if length > initial_if_length {
            initial_if_length = length;
            continue;
        }

        if else_block.len() > 0 {
            let start = current_code.location;
            current_code.indent("Else-block".to_string());
            gen_block(ctx, &mut else_state, else_block, &mut current_code)?;
            current_code.dedent();
            let mut length = current_code.location - start;
            if length > initial_else_length {
                initial_else_length = length;
                continue;
            }

            while length < initial_else_length {
                current_code.nop();
                length += 1;
            }

            current_state.merge(&else_state);
        }

        break;
    }

    state.merge(&current_state);
    code.merge(current_code);
    code.dedent();
    Ok(())
}

fn gen_jump(ctx: &Context, state: &mut CpuState, dist: i8, op: isa::BranchOp, code: &mut Code) {
    if dist < 0 && dist > -16 {
        let d = (dist as u8) & 0x0f;
        state.set_acc_to_val(d, code);
        code.instr(isa::Instr::Branch(op, true));
    } else {
        state.set_acc_to_val(dist as u8, code);
        code.instr(isa::Instr::Branch(op, false));
    }
}

fn gen_jump_from_start(
    ctx: &Context,
    state: &mut CpuState,
    dist: i8,
    op: isa::BranchOp,
    code: &mut Code,
) {
    if dist > 0 {
        gen_jump(ctx, state, dist, op, code);
        return;
    }

    if dist > -15 {
        let acc = ((dist - 1) as u8) & 0x0f;
        code.imml(acc);
        state.touched_acc = true;
        state.acc = Some(acc);
        code.instr(isa::Instr::Branch(op, true));
    } else {
        let acc = (dist - 2) as u8;
        code.imml(acc);
        code.immh(acc);
        state.touched_acc = true;
        state.acc = Some(acc);
        code.instr(isa::Instr::Branch(op, false));
    }
}

fn gen_loop_statm(
    ctx: &Context,
    state: &mut CpuState,
    block: &ast::Block,
    code: &mut Code,
) -> Result<(), String> {
    code.indent("Loop".to_string());
    state.clobber_all(); // Can we avoid this somehow?
    let start = code.location;
    code.indent("Loop-block".to_string());
    gen_block(ctx, state, block, code)?;
    code.dedent();
    let length = code.location - start;

    gen_jump_from_start(ctx, state, -(length as i8), isa::BranchOp::B, code);

    // Currently, we never need to consider adding an implicit return
    // when we have a loop statement, since the only way to exit
    // a loop is to return.
    // If we ever add a 'break' statement or 'goto' or something like that,
    // we have to revisit this.
    state.returned = true;

    state.clobber_all();
    code.dedent();
    Ok(())
}

fn gen_while_statm(
    ctx: &Context,
    state: &mut CpuState,
    cond: &ast::Condition,
    block: &ast::Block,
    code: &mut Code,
) -> Result<(), String> {
    code.indent("While".to_string());
    state.clobber_all();
    let start = code.location;
    gen_if_statm(ctx, state, cond, block, &ast::Block::new(), code)?;
    let length = code.location - start;

    gen_jump_from_start(ctx, state, -(length as i8), isa::BranchOp::B, code);

    code.dedent();
    Ok(())
}

fn gen_reg_assign_statm(
    ctx: &Context,
    state: &mut CpuState,
    reg: &isa::Reg,
    op: &isa::RegOp,
    acc: &ast::Acc,
    code: &mut Code,
) -> Result<(), String> {
    match acc {
        ast::Acc::Const(expr) => {
            state.reg_op_val(*reg, op.clone(), expr.eval(&ctx.program)? as u8, code)
        }
        ast::Acc::Reg(src) => state.set_reg_to_reg(*reg, *src, code),
    }

    Ok(())
}

fn gen_load_statm(
    ctx: &Context,
    state: &mut CpuState,
    reg: &isa::Reg,
    acc: &ast::Acc,
    code: &mut Code,
) -> Result<(), String> {
    gen_set_acc(ctx, state, acc, code)?;
    state.reg_load(*reg, acc_get_dbit(acc, &ctx.program), code);
    Ok(())
}

fn gen_store_statm(
    ctx: &Context,
    state: &mut CpuState,
    acc: &ast::Acc,
    reg: &isa::Reg,
    code: &mut Code,
) -> Result<(), String> {
    gen_set_acc(ctx, state, acc, code)?;
    state.reg_store(*reg, acc_get_dbit(acc, &ctx.program), code);
    Ok(())
}

fn gen_call_statm(
    ctx: &Context,
    state: &mut CpuState,
    name: &String,
    code: &mut Code,
) -> Result<(), String> {
    code.indent(format!("Call {}", name));
    match &ctx.function_names.get(name) {
        None => {
            return Err(format!("Call to undefined function {}", name));
        }
        Some(func_idx) => {
            let func = &ctx.functions[**func_idx];

            state.set_reg_to_val(isa::Reg::CS, ((func.addr & 0xff00) >> 8) as u8, code);

            // We want to make sure we're not right at the end of a code boundary
            while code.location % 256 > 250 {
                code.nop();
            }

            state.set_reg_to_val(isa::Reg::RV, ((code.location & 0xff00) >> 8) as u8, code);
            state.set_acc_to_val((func.addr & 0x00ff) as u8, code);
            code.instr(isa::Instr::Jmp(isa::JmpOp::Call, false));
            state.merge(&func.post_state);
        }
    }

    state.regs[isa::Reg::CS as usize] = Some(((code.location & 0xff00) >> 8) as u8);
    code.dedent();
    Ok(())
}

fn gen_return_statm(
    ctx: &Context,
    state: &mut CpuState,
    ret: &Option<ast::Acc>,
    code: &mut Code,
) -> Result<(), String> {
    if ctx.current_func().is_leaf {
        state.set_reg_to_reg(isa::Reg::CS, isa::Reg::RV, code);

        match ret {
            Some(ret) => {
                gen_set_reg(ctx, state, isa::Reg::RV, ret, code)?;
            }
            _ => (),
        }

        state.set_acc_to_reg(isa::Reg::RA, code);
        code.instr(isa::Instr::Jmp(isa::JmpOp::Jmp, false));
        state.returned = true;
        return Ok(());
    }

    state.stack_pop(isa::Reg::CS, code); // Pop return segment

    match ret {
        Some(ret) => {
            gen_set_reg(ctx, state, isa::Reg::RV, ret, code)?;
        }
        _ => (),
    }

    state.stack_pop(isa::Reg::RA, code); // Pop return address
    code.instr(isa::Instr::Jmp(isa::JmpOp::Jmp, false));
    state.returned = true;
    Ok(())
}
