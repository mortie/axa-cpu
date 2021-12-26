use super::isa;
use std::fmt;

struct MemMapping<'a> {
    start: u16,
    length: u16,
    mem: &'a mut dyn MemBlock,
}

pub struct Emulator<'a> {
    pub iptr: u16,
    pub acc: u8,
    pub regs: [u8; 8],
    pub zflag: bool,
    pub cflag: bool,
    pub oflag: bool,
    mem: Vec<MemMapping<'a>>,
}

pub trait MemBlock {
    fn load(&mut self, offset: u16) -> u8;
    fn store(&mut self, offset: u16, value: u8);
}

impl<'a> Emulator<'a> {
    pub fn new() -> Self {
        return Self{
            regs: [0; 8],
            acc: 0,
            iptr: 0,
            zflag: false,
            cflag: false,
            oflag: false,
            mem: Vec::new(),
        };
    }

    pub fn load(&mut self, addr: u16) -> u8 {
        for mapping in &mut self.mem {
            let start = mapping.start;
            let end = start + (mapping.length - 1);
            if addr >= start && addr <= end {
                return mapping.mem.load(addr - start);
            }
        }

        println!("Wild load: No block mapped to {}", addr);
        return 0;
    }

    pub fn store(&mut self, addr: u16, value: u8) {
        for mapping in &mut self.mem {
            let start = mapping.start;
            let end = start + (mapping.length - 1);
            if addr >= start && addr <= end {
                mapping.mem.store(addr - start, value);
                return;
            }
        }

        println!("Wild store: No block mapped to {}", addr);
    }

    pub fn map_memory(&mut self, start: u16, length: u16, mem: &'a mut dyn MemBlock) {
        self.mem.push(MemMapping{start, length, mem});
    }

    pub fn exec(&mut self, instr: isa::Instr) {
        match instr {
            isa::Instr::FmtReg(op, dbit, reg) => {
                let s: u16;
                let d: u16;
                let res: u16;
                let write: bool;
                let write_flags: bool;

                if dbit {
                    s = self.regs[reg as usize] as u16;
                    d = self.acc as u16;
                } else {
                    s = self.acc as u16;
                    d = self.regs[reg as usize] as u16;
                }

                match op {
                    isa::RegOp::Add => {
                        res = d + s;
                        write = true; write_flags = true;
                    },
                    isa::RegOp::Sub => {
                        res = d + (!s & 0xff) + 1;
                        write = true; write_flags = true;
                    },
                    isa::RegOp::Xor => {
                        res = d ^ s;
                        write = true; write_flags = true;
                    },
                    isa::RegOp::And => {
                        res = d & s;
                        write = true; write_flags = true;
                    },
                    isa::RegOp::Or => {
                        res = d | s;
                        write = true; write_flags = true;
                    },
                    isa::RegOp::Mov => {
                        res = s;
                        write = true; write_flags = false;
                    },
                    isa::RegOp::Shr => {
                        res = (d >> 1) | ((d & 1) << 8);
                        write = true; write_flags = true;
                    },
                    isa::RegOp::Cmp => {
                        res = d + (!s & 0xff) + 1;
                        write = false; write_flags = true;
                    },
                    isa::RegOp::Addc => {
                        res = d + s + self.cflag as u16;
                        write = true; write_flags = true;
                    },
                    isa::RegOp::Shrc => {
                        res = (d >> 1) | ((self.cflag as u16) << 7) | ((d & 1) << 8);
                        write = true; write_flags = true;
                    },
                    isa::RegOp::Cmpc => {
                        res = d + (!s & 0xff) + self.cflag as u16;
                        write = false; write_flags = true;
                    },
                }

                if write {
                    if dbit {
                        self.acc = res as u8;
                    } else {
                        self.regs[reg as usize] = res as u8;
                    }
                }

                if write_flags {
                    self.zflag = (res & 0xff) == 0;
                    self.cflag = (res & 0x100) != 0;
                    self.oflag =
                        ((s & 0x40 & d & 0x40) != 0 && (res & 0x80) != 0) ||
                        ((s & 0x80 & d & 0x80) != 0 && (res & 0x80) == 0);
                }

                self.iptr += 1;
            },
            isa::Instr::FmtJmp(op, sbit) => {
                let acc = match sbit {
                    false => self.acc as u16,
                    true => (self.acc | 0x80) as u16,
                };

                let cs = self.regs[isa::Reg::CS as usize] as u16;
                let dest = cs << 8 | acc;
                match op {
                    isa::JmpOp::Jmp => (),
                    isa::JmpOp::Call =>
                        self.regs[isa::Reg::RA as usize] = (self.iptr + 1) as u8,
                }

                self.iptr = dest;
            },
            isa::Instr::FmtBranch(op, sbit) => {
                let offset = match sbit {
                    false => self.acc as u16,
                    true => (self.acc as u16) | 0xff80,
                };

                let branch = match op {
                    isa::BranchOp::B => true,
                    isa::BranchOp::Beq => self.zflag,
                    isa::BranchOp::Bgt => self.cflag && !self.zflag,
                    isa::BranchOp::Bge => self.cflag,
                    isa::BranchOp::Bgts => self.oflag && !self.zflag,
                    isa::BranchOp::Bges => self.oflag,
                };

                if branch {
                    self.iptr += offset;
                } else {
                    self.iptr += 1;
                }
            },
            isa::Instr::FmtMem(op, dbit, reg) => {
                let addr = match dbit {
                    true =>
                        (self.regs[isa::Reg::DS as usize] as u16) << 8 |
                        self.acc as u16,
                    false => self.acc as u16,
                };

                match op {
                    isa::MemOp::Ld => {
                        self.regs[reg as usize] = self.load(addr);
                    },
                    isa::MemOp::St => {
                        self.store(addr, self.regs[reg as usize]);
                    },
                }

                self.iptr += 1;
            },
            isa::Instr::FmtImm(op, imm) => {
                match op {
                    isa::ImmOp::Imml => {
                        self.acc = imm;
                    },
                    isa::ImmOp::Immh => {
                        self.acc |= imm << 4;
                    },
                };

                self.iptr += 1;
            }
        }
    }
}

impl fmt::Display for Emulator<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Flag: Z:{} C:{} O:{}",
            self.zflag as u8, self.cflag as u8, self.oflag as u8)?;
        write!(f, "IPtr: 0x{:04x}", self.iptr)?;
        writeln!(f, ";  Accumulator: {}", self.acc)?;
        writeln!(f, "Regs: CS:{: <3} DS:{: <3} SP:{: <3} RV:{: <3}",
             self.regs[0], self.regs[1], self.regs[2], self.regs[3])?;
        write!(f, "      A1:{: <3} A2:{: <3} A3:{: <3} RA:{: <3}",
            self.regs[4], self.regs[5], self.regs[6], self.regs[7])?;
        return Ok(());
    }
}
