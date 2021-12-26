mod emulator;
mod isa;

use isa::*;
use std::cell::RefCell;
use std::io;
use std::io::Read;

struct RamBlock {
    data: Vec<u8>,
}

impl RamBlock {
    fn new(size: u16) -> Self {
        let mut data: Vec<u8> = Vec::new();
        data.resize(size as usize, 0);
        return Self{data};
    }
}

impl emulator::MemBlock for RamBlock {
    fn load(&mut self, offset: u16) -> u8 {
        return self.data[offset as usize];
    }
    fn store(&mut self, offset: u16, value: u8) {
        self.data[offset as usize] = value;
    }
}

struct DisplayBlock {
    rows: [[u8; 128/8]; 128],
}

impl DisplayBlock {
    fn new() -> Self {
        return Self{rows: [[0 as u8; 128/8]; 128]};
    }

    fn len(&self) -> usize {
        return self.rows.len() * self.rows[0].len();
    }

    fn render(&self) {
        let rowlen = self.rows[0].len();
        for rowidx in 0..self.rows.len() {
            for col in 0..rowlen {
                let a = self.rows[rowidx * 2][col];
                let b = self.rows[rowidx * 2 + 1][col];

                for bit in 0..8 {
                    let abit = (a & 1 << bit) >> bit != 0;
                    let bbit = (b & 1 << bit) >> bit != 0;
                    if abit && bbit {
                        print!("█");
                    } else if abit && !bbit {
                        print!("▀");
                    } else if !abit && bbit { 
                        print!("▄");
                    } else {
                        print!(" ");
                    }
                }
            }
            print!("\n");
        }
    }
}

impl emulator::MemBlock for DisplayBlock {
    fn load(&mut self, offset: u16) -> u8 {
        let row = (offset as usize) % self.rows.len();
        let col = (offset as usize) / self.rows.len();
        return self.rows[row][col];
    }
    fn store(&mut self, offset: u16, value: u8) {
        let row = (offset as usize) % self.rows.len();
        let col = (offset as usize) / self.rows.len();
        self.rows[row][col] = value;
        self.render();
    }
}

struct ControlBlock<'a> {
    data: &'a RefCell<u8>
}

impl emulator::MemBlock for ControlBlock<'_> {
    fn load(&mut self, _offset: u16) -> u8 { 0 }
    fn store(&mut self, _offset: u16, value: u8) {
        self.data.replace(value);
    }
}

fn main() -> Result<(), Box<io::Error>> {
    let mut emu = emulator::Emulator::new();

    let mut ram = RamBlock::new(1024);
    ram.data[0] = Instr::Imm(ImmOp::Imml, 0x01).format();
    ram.data[1] = Instr::Reg(RegOp::Mov, false, Reg::A1).format();
    ram.data[2] = Instr::Imm(ImmOp::Imml, 0xff).format();
    ram.data[3] = Instr::Imm(ImmOp::Immh, 0xff).format();
    ram.data[4] = Instr::Reg(RegOp::Mov, false, Reg::DS).format();
    ram.data[5] = Instr::Mem(MemOp::St, true, Reg::A1).format();
    emu.map_memory(0, 1024, &mut ram);

    let mut display = DisplayBlock::new();
    emu.map_memory(0x8000, display.len() as u16, &mut display);

    let ctrl_byte = RefCell::new(0 as u8);
    let mut ctrl = ControlBlock{data: &ctrl_byte};
    emu.map_memory(0xffff, 1, &mut ctrl);

    println!("{}", emu);
    while *ctrl_byte.borrow() == 0 {
        let instr = Instr::parse(emu.load(emu.iptr));
        println!("\n0x{:04x} {}", emu.iptr, instr);
        io::stdin().read(&mut [0 as u8; 1])?;

        emu.exec(instr);
        println!("{}", emu);
    }

    return Ok(());
}
