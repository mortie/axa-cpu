use super::isa::*;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};

pub struct Context {
    defines: HashMap<String, u8>,
    iptr: u16,
}

fn parse_reg<'a>(s: &'a str) -> Option<Reg> {
    match s {
        "cs" => Some(Reg::CS),
        "ds" => Some(Reg::DS),
        "sp" => Some(Reg::SP),
        "rv" => Some(Reg::RV),
        "a1" => Some(Reg::A1),
        "a2" => Some(Reg::A2),
        "a3" => Some(Reg::A3),
        "ra" => Some(Reg::RA),
        _ => None,
    }
}

fn is_numeric(s: &str) -> bool {
    for ch in s.bytes() {
        if !(ch >= b'0' && ch <= b'9') {
            return false;
        }
    }

    true
}

fn parse_imm<'a>(s: &'a str, ctx: &Context) -> Result<u8, String> {
    if s.starts_with("-") {
        return match parse_imm(&s[1..], ctx) {
            Ok(val) => Ok(((!val) as u16 + 1) as u8),
            err => err,
        };
    }

    let res;
    if s.starts_with("0x") {
        res = u8::from_str_radix(&s[2..], 16);
    } else if s.starts_with("0o") {
        res = u8::from_str_radix(&s[2..], 8);
    } else if s.starts_with("0b") {
        res = u8::from_str_radix(&s[2..], 2);
    } else if is_numeric(s) {
        res = u8::from_str_radix(&s, 10);
    } else if ctx.defines.contains_key(s) {
        res = Ok(ctx.defines[s]);
    } else if s.starts_with('\'') {
        let mut bytes = s.bytes();
        bytes.next(); // quote
        let mut ch = match bytes.next() {
            Some(ch) => ch,
            None => return Err("Invalid character literal".to_string()),
        };

        res = if ch == b'\\' {
            ch = match bytes.next() {
                Some(ch) => ch,
                None => return Err("Invalid character literal".to_string()),
            };

            match ch {
                b'n' => Ok(b'\n'),
                b'r' => Ok(b'\r'),
                b't' => Ok(b'\t'),
                b's' => Ok(b' '),
                b'0' => Ok(b'\0'),
                ch => Ok(ch),
            }
        } else {
            Ok(ch)
        };

        ch = match bytes.next() {
            Some(ch) => ch,
            None => return Err("Invalid character literal".to_string()),
        };

        if ch != b'\'' || bytes.next().is_some() {
            return Err("Invalid character literal".to_string());
        }
    } else {
        return Err("Invalid immediate".to_string());
    }

    match res {
        Ok(val) => Ok(val),
        Err(err) => Err(err.to_string()),
    }
}

fn assemble_reg<'a, It>(op: RegOp, mut parts: It) -> Result<Instr, String>
where
    It: Iterator<Item = &'a str>,
{
    let d = parts.next();
    let s = parts.next();
    if d.is_none() || s.is_none() || parts.next().is_some() {
        return Err("Expected 2 arguments".to_string());
    }
    let d = d.unwrap();
    let s = s.unwrap();

    let dbit;
    let reg;
    if d == "acc" {
        dbit = true;
        reg = parse_reg(s);
    } else if s == "acc" {
        dbit = false;
        reg = parse_reg(d);
    } else {
        return Err("Expected 1 operand to be accumulator".to_string());
    }

    if reg.is_none() {
        return Err("Invalid register name".to_string());
    }

    Ok(Instr::Reg(op, dbit, reg.unwrap()))
}

fn assemble_jmp<'a, It>(op: JmpOp, mut parts: It) -> Result<Instr, String>
where
    It: Iterator<Item = &'a str>,
{
    if parts.next().is_some() {
        return Err("Expected 0 arguments".to_string());
    }

    Ok(Instr::Jmp(op, false))
}

fn assemble_branch<'a, It>(op: BranchOp, dbit: bool, mut parts: It) -> Result<Instr, String>
where
    It: Iterator<Item = &'a str>,
{
    if parts.next().is_some() {
        return Err("Expected 0 arguments".to_string());
    }

    Ok(Instr::Branch(op, dbit))
}

fn assemble_mem<'a, It>(op: MemOp, dbit: bool, mut parts: It) -> Result<Instr, String>
where
    It: Iterator<Item = &'a str>,
{
    let reg = parts.next();
    if reg.is_none() || parts.next().is_some() {
        return Err("Expected 1 argument".to_string());
    }

    let reg = match parse_reg(reg.unwrap()) {
        Some(reg) => reg,
        None => return Err("Invalid register name".to_string()),
    };

    Ok(Instr::Mem(op, dbit, reg))
}

fn assemble_imm<'a, It>(op: ImmOp, mut parts: It, ctx: &Context) -> Result<Instr, String>
where
    It: Iterator<Item = &'a str>,
{
    let imm = match parts.next() {
        Some(imm) => imm,
        None => return Err("Expected 1 argument".to_string()),
    };

    let imm = parse_imm(&imm, ctx)?;

    match op {
        ImmOp::Imml => Ok(Instr::Imm(op, imm & 0x0f)),
        ImmOp::Immh => Ok(Instr::Imm(op, imm & 0xf0)),
    }
}

pub fn assemble_line(line: &str, w: &mut dyn Write, ctx: &mut Context) -> Result<(), String> {
    let mut parts = line.split_ascii_whitespace();
    let op = match parts.next() {
        Some(op) => op,
        None => return Ok(()),
    };

    if op.starts_with("#") {
        return Ok(());
    }

    if op == ".def" {
        let key = parts.next();
        let val = parts.next();
        if key.is_none() || val.is_none() || parts.next().is_some() {
            return Err("Expected 2 arguments".to_string());
        }

        let val = parse_imm(val.unwrap(), ctx)?;
        ctx.defines.insert(key.unwrap().to_string(), val);
        return Ok(());
    } else if op == ".zero" {
        let count = parts.next();
        if count.is_none() || parts.next().is_some() {
            return Err("Expected 1 argument".to_string());
        };

        let count = parse_imm(count.unwrap(), ctx)?;
        for _ in 0..count {
            if let Err(err) = w.write(&[0 as u8]) {
                return Err(err.to_string());
            }
            ctx.iptr += 1;
        }

        return Ok(());
    } else if op == ".byte" {
        let val = parts.next();
        if val.is_none() || parts.next().is_some() {
            return Err("Expected 1 argument".to_string());
        };

        let val = parse_imm(val.unwrap(), ctx);
        if let Err(err) = w.write(&[val.unwrap()]) {
            return Err(err.to_string());
        }
        ctx.iptr += 1;

        return Ok(());
    } else if op == ".align" {
        let count = parts.next();
        if count.is_none() || parts.next().is_some() {
            return Err("Expected 1 argument".to_string());
        };

        let count = parse_imm(count.unwrap(), ctx)?;
        while ctx.iptr % (count as u16) != 0 {
            if let Err(err) = w.write(&[0 as u8]) {
                return Err(err.to_string());
            }
            ctx.iptr += 1;
        }

        return Ok(());
    }

    let instr = match op {
        "add" => assemble_reg(RegOp::Add, parts),
        "sub" => assemble_reg(RegOp::Sub, parts),
        "xor" => assemble_reg(RegOp::Xor, parts),
        "and" => assemble_reg(RegOp::And, parts),
        "or" => assemble_reg(RegOp::Or, parts),
        "mov" => assemble_reg(RegOp::Mov, parts),
        "shr" => assemble_reg(RegOp::Shr, parts),
        "cmp" => assemble_reg(RegOp::Cmp, parts),
        "addc" => assemble_reg(RegOp::Addc, parts),
        "shrc" => assemble_reg(RegOp::Shrc, parts),
        "cmpc" => assemble_reg(RegOp::Cmpc, parts),
        "jmp" => assemble_jmp(JmpOp::Jmp, parts),
        "call" => assemble_jmp(JmpOp::Call, parts),
        "b" => assemble_branch(BranchOp::B, false, parts),
        "beq" => assemble_branch(BranchOp::Beq, false, parts),
        "bgt" => assemble_branch(BranchOp::Bgt, false, parts),
        "bge" => assemble_branch(BranchOp::Bge, false, parts),
        "bgts" => assemble_branch(BranchOp::Bgts, false, parts),
        "bges" => assemble_branch(BranchOp::Bges, false, parts),
        "bb" => assemble_branch(BranchOp::B, true, parts),
        "bbeq" => assemble_branch(BranchOp::Beq, true, parts),
        "bbgt" => assemble_branch(BranchOp::Bgt, true, parts),
        "bbge" => assemble_branch(BranchOp::Bge, true, parts),
        "bbgts" => assemble_branch(BranchOp::Bgts, true, parts),
        "bbges" => assemble_branch(BranchOp::Bges, true, parts),
        "lds" => assemble_mem(MemOp::Ld, false, parts),
        "sts" => assemble_mem(MemOp::St, false, parts),
        "ldd" => assemble_mem(MemOp::Ld, true, parts),
        "std" => assemble_mem(MemOp::St, true, parts),
        "imml" => assemble_imm(ImmOp::Imml, parts, &ctx),
        "immh" => assemble_imm(ImmOp::Immh, parts, &ctx),
        _ => return Err("Invalid instruction".to_string()),
    }?;

    if let Err(err) = w.write(&[instr.format()]) {
        return Err(err.to_string());
    }

    ctx.iptr += 1;
    Ok(())
}

pub fn assemble(r: &mut dyn Read, w: &mut dyn Write) -> Result<(), String> {
    let mut ctx = Context {
        defines: HashMap::new(),
        iptr: 0,
    };

    let reader = BufReader::new(r);
    let mut linenum = 1;
    for line in reader.lines() {
        let line = match line {
            Ok(line) => line,
            Err(err) => return Err(err.to_string()),
        };

        let res = assemble_line(&line, w, &mut ctx);
        if let Err(err) = res {
            return Err(format!("Line {}: {}", linenum, err));
        }

        linenum += 1;
    }

    Ok(())
}
