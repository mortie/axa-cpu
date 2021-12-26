use std::fmt;

#[derive(Clone, Copy)]
pub enum Reg {
    CS = 0, DS = 1, SP = 2, RV = 3,
    A1 = 4, A2 = 5, A3 = 6, RA = 7,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::CS => write!(f, "cs"),
            Reg::DS => write!(f, "ds"),
            Reg::SP => write!(f, "sp"),
            Reg::RV => write!(f, "rv"),
            Reg::A1 => write!(f, "a1"),
            Reg::A2 => write!(f, "a2"),
            Reg::A3 => write!(f, "a3"),
            Reg::RA => write!(f, "ra"),
        }
    }
}

pub enum RegOp {
    Add, Sub, Xor, And,
    Or, Mov, Shr, Cmp,
    Addc, Shrc, Cmpc,
}

impl fmt::Display for RegOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegOp::Add => write!(f, "add"),
            RegOp::Sub => write!(f, "sub"),
            RegOp::Xor => write!(f, "xor"),
            RegOp::And => write!(f, "and"),
            RegOp::Or => write!(f, "or"),
            RegOp::Mov => write!(f, "mov"),
            RegOp::Shr => write!(f, "shr"),
            RegOp::Cmp => write!(f, "cmp"),
            RegOp::Addc => write!(f, "addc"),
            RegOp::Shrc => write!(f, "shrc"),
            RegOp::Cmpc => write!(f, "cmpc"),
        }
    }
}

pub enum JmpOp {
    Jmp = 0b000, Call = 0b001,
}

impl fmt::Display for JmpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JmpOp::Jmp => write!(f, "jmp"),
            JmpOp::Call => write!(f, "call"),
        }
    }
}

pub enum BranchOp {
    B = 0b010, Beq = 0b011, Bgt = 0b100,
    Bge = 0b101, Bgts = 0b110, Bges = 0b111,
}

impl fmt::Display for BranchOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BranchOp::B => write!(f, "b"),
            BranchOp::Beq => write!(f, "beq"),
            BranchOp::Bgt => write!(f, "bgt"),
            BranchOp::Bge => write!(f, "bge"),
            BranchOp::Bgts => write!(f, "bgts"),
            BranchOp::Bges => write!(f, "bges"),
        }
    }
}

pub enum MemOp {
    Ld, St,
}

impl fmt::Display for MemOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemOp::Ld => write!(f, "ld"),
            MemOp::St => write!(f, "st"),
        }
    }
}

pub enum ImmOp {
    Imml, Immh,
}

impl fmt::Display for ImmOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImmOp::Imml => write!(f, "imml"),
            ImmOp::Immh => write!(f, "immh"),
        }
    }
}

pub enum Instr {
    FmtReg(RegOp, bool, Reg),
    FmtJmp(JmpOp, bool),
    FmtBranch(BranchOp, bool),
    FmtMem(MemOp, bool, Reg),
    FmtImm(ImmOp, u8),
}

impl Instr {
    pub fn parse(instr: u8) -> Instr {
        let op = (instr & 0xf0) >> 4;
        let dbit = (instr & 0x08) >> 3 != 0;
        let regbits = instr & 0x07;
        let imm = instr & 0x0f;

        let reg = match regbits {
            0b000 => Reg::CS,
            0b001 => Reg::DS,
            0b010 => Reg::SP,
            0b011 => Reg::RV,
            0b100 => Reg::A1,
            0b101 => Reg::A2,
            0b110 => Reg::A3,
            0b111 => Reg::RA,
            _ => panic!("Illegal register"),
        };

        match op {
            0b0000 => Instr::FmtReg(RegOp::Add, dbit, reg),
            0b0001 => Instr::FmtReg(RegOp::Sub, dbit, reg),
            0b0010 => Instr::FmtReg(RegOp::Xor, dbit, reg),
            0b0011 => Instr::FmtReg(RegOp::And, dbit, reg),
            0b0100 => Instr::FmtReg(RegOp::Or,  dbit, reg),
            0b0101 => Instr::FmtReg(RegOp::Mov, dbit, reg),
            0b0110 => Instr::FmtReg(RegOp::Shr, dbit, reg),
            0b0111 => Instr::FmtReg(RegOp::Cmp, dbit, reg),
            0b1000 => Instr::FmtReg(RegOp::Addc, dbit, reg),
            0b1001 => Instr::FmtReg(RegOp::Shrc, dbit, reg),
            0b1010 => Instr::FmtReg(RegOp::Cmpc, dbit, reg),
            0b1011 => match regbits {
                0b000 => Instr::FmtJmp(JmpOp::Jmp, dbit),
                0b001 => Instr::FmtJmp(JmpOp::Call, dbit),
                0b010 => Instr::FmtBranch(BranchOp::B, dbit),
                0b011 => Instr::FmtBranch(BranchOp::Beq, dbit),
                0b100 => Instr::FmtBranch(BranchOp::Bgt, dbit),
                0b101 => Instr::FmtBranch(BranchOp::Bge, dbit),
                0b110 => Instr::FmtBranch(BranchOp::Bgts, dbit),
                0b111 => Instr::FmtBranch(BranchOp::Bges, dbit),
                _ => panic!("Illegal jump condition"),
            },
            0b1100 => Instr::FmtMem(MemOp::Ld, dbit, reg),
            0b1101 => Instr::FmtMem(MemOp::St, dbit, reg),
            0b1110 => Instr::FmtImm(ImmOp::Imml, imm),
            0b1111 => Instr::FmtImm(ImmOp::Immh, imm),
            _ => panic!("Illegal op"),
        }
    }

    pub fn format(self) -> u8 {
        match self {
            Instr::FmtReg(op, dbit, reg) => {
                let opcode = match op {
                    RegOp::Add => 0b0000,
                    RegOp::Sub => 0b0001,
                    RegOp::Xor => 0b0010,
                    RegOp::And => 0b0011,
                    RegOp::Or => 0b0100,
                    RegOp::Mov => 0b0101,
                    RegOp::Shr => 0b0110,
                    RegOp::Cmp => 0b0111,
                    RegOp::Addc => 0b1000,
                    RegOp::Shrc => 0b1001,
                    RegOp::Cmpc => 0b1011,
                };

                (opcode as u8) << 4 | (dbit as u8) << 3 | (reg as u8)
            },
            Instr::FmtJmp(op, dbit) =>
                0b1011_0000 | (dbit as u8) << 3 | (op as u8),
            Instr::FmtBranch(op, dbit) =>
                0b1011_0000 | (dbit as u8) << 3 | (op as u8),
            Instr::FmtMem(op, dbit, reg) => {
                let opcode = match op {
                    MemOp::Ld => 0b1100,
                    MemOp::St => 0b1101,
                };

                (opcode as u8) << 4 | (dbit as u8) << 3 | (reg as u8)
            },
            Instr::FmtImm(op, imm) => match op {
                ImmOp::Imml => 0b1110_0000 | (imm & 0x0f),
                ImmOp::Immh => 0b1111_0000 | ((imm & 0xf0) >> 4),
            },
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::FmtReg(op, dbit, reg) => match dbit {
                true => write!(f, "{} acc {}", op, reg),
                false => write!(f, "{} {} acc", op, reg),
            },
            Instr::FmtJmp(op, _sbit) => write!(f, "{}", op),
            Instr::FmtBranch(op, sbit) => match sbit {
                true => write!(f, "b{}", op),
                false => write!(f, "{}", op),
            },
            Instr::FmtMem(op, dbit, reg) => match dbit {
                true => write!(f, "{}d {}", op, reg),
                false => write!(f, "{}s {}", op, reg),
            },
            Instr::FmtImm(op, imm) => match op {
                ImmOp::Imml => write!(f, "imml 0x{:02x}", imm),
                ImmOp::Immh => write!(f, "immh 0x{:02x}", imm << 4),
            },
        }
    }
}
