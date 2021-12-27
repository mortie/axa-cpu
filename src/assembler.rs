use super::isa::*;
use std::io::{BufRead, BufReader, Read, Write};

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

fn parse_imm<'a>(s: &'a str) -> Result<u8, String> {
    let res;
    if s.starts_with("0x") {
        res = u8::from_str_radix(&s[2..], 16);
    } else if s.starts_with("0o") {
        res = u8::from_str_radix(&s[2..], 8);
    } else if s.starts_with("0b") {
        res = u8::from_str_radix(&s[2..], 2);
    } else {
        res = u8::from_str_radix(&s, 10);
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

fn assemble_imm<'a, It>(op: ImmOp, mut parts: It) -> Result<Instr, String>
where
    It: Iterator<Item = &'a str>,
{
    let imm = match parts.next() {
        Some(imm) => imm,
        None => return Err("Expected 1 argument".to_string()),
    };

    let imm = parse_imm(&imm)?;

    match op {
        ImmOp::Imml => Ok(Instr::Imm(op, imm & 0x0f)),
        ImmOp::Immh => Ok(Instr::Imm(op, imm & 0xf0)),
    }
}

pub fn assemble_line(line: &str, w: &mut dyn Write) -> Result<(), String> {
    let mut parts = line.split_ascii_whitespace();
    let op = match parts.next() {
        Some(op) => op,
        None => return Ok(()),
    };

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
        "imml" => assemble_imm(ImmOp::Imml, parts),
        "immh" => assemble_imm(ImmOp::Immh, parts),
        _ => return Err("Invalid instruction".to_string()),
    }?;

    match w.write(&[instr.format()]) {
        Ok(_) => Ok(()),
        Err(err) => Err(err.to_string()),
    }
}

pub fn assemble(r: &mut dyn Read, w: &mut dyn Write) -> Result<(), String> {
    let reader = BufReader::new(r);
    let mut linenum = 1;
    for line in reader.lines() {
        let line = match line {
            Ok(line) => line,
            Err(err) => return Err(err.to_string()),
        };

        let res = assemble_line(&line, w);
        if let Err(err) = res {
            return Err(format!("Line {}: {}", linenum, err));
        }

        linenum += 1;
    }

    Ok(())
}
