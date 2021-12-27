mod emulator;
mod isa;

use isa::*;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::process;

struct RamBlock {
    data: Vec<u8>,
}

impl RamBlock {
    fn new(size: u16) -> Self {
        let mut data: Vec<u8> = Vec::new();
        data.resize(size as usize, 0);
        Self { data }
    }
}

impl emulator::MemBlock for RamBlock {
    fn load(&mut self, offset: u16) -> u8 {
        self.data[offset as usize]
    }
    fn store(&mut self, offset: u16, value: u8) {
        self.data[offset as usize] = value;
    }
}

struct DisplayBlock {
    rows: [[u8; 128 / 8]; 128],
}

impl DisplayBlock {
    fn new() -> Self {
        Self {
            rows: [[0 as u8; 128 / 8]; 128],
        }
    }

    fn len(&self) -> usize {
        self.rows.len() * self.rows[0].len()
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
        self.rows[row][col]
    }
    fn store(&mut self, offset: u16, value: u8) {
        let row = (offset as usize) % self.rows.len();
        let col = (offset as usize) / self.rows.len();
        self.rows[row][col] = value;
        self.render();
    }
}

struct ControlBlock<'a> {
    data: &'a RefCell<u8>,
}

impl emulator::MemBlock for ControlBlock<'_> {
    fn load(&mut self, _offset: u16) -> u8 {
        0
    }
    fn store(&mut self, _offset: u16, value: u8) {
        self.data.replace(value);
    }
}

fn _main() -> Result<(), Box<io::Error>> {
    let mut emu = emulator::Emulator::new();

    let mut ram = RamBlock::new(1024);
    emu.map_memory(0, 1024, &mut ram);

    let mut display = DisplayBlock::new();
    emu.map_memory(0x8000, display.len() as u16, &mut display);

    let ctrl_byte = RefCell::new(0 as u8);
    let mut ctrl = ControlBlock { data: &ctrl_byte };
    emu.map_memory(0xffff, 1, &mut ctrl);

    println!("{}", emu);
    while *ctrl_byte.borrow() == 0 {
        let instr = Instr::parse(emu.load(emu.iptr));
        println!("\n0x{:04x} {}", emu.iptr, instr);
        io::stdin().read(&mut [0 as u8; 1])?;

        emu.exec(instr);
        println!("{}", emu);
    }

    Ok(())
}

fn usage(argv0: &str) {
    println!("Usage: {} emulate [--step] <path>", argv0);
}

struct EmuOpts {
    step: bool,
}

fn run_emulator(data: &Vec<u8>, opts: &EmuOpts) {
    let ram_size = 1024;
    if data.len() > ram_size {
        println!(
            "Program too long! Have {} bytes of RAM, program is {} bytes",
            ram_size,
            data.len()
        );
        return;
    }

    let mut emu = emulator::Emulator::new();

    let mut ram = RamBlock::new(ram_size as u16);
    for idx in 0..data.len() {
        ram.data[idx] = data[idx];
    }
    emu.map_memory(0, ram_size as u16, &mut ram);

    let mut display = DisplayBlock::new();
    emu.map_memory(0x8000, display.len() as u16, &mut display);

    let ctrl_byte = RefCell::new(0 as u8);
    let mut ctrl = ControlBlock { data: &ctrl_byte };
    emu.map_memory(0xffff, 1, &mut ctrl);

    if opts.step {
        println!("{}", emu);
    }

    while *ctrl_byte.borrow() == 0 {
        let instr = Instr::parse(emu.load(emu.iptr));

        if opts.step {
            println!("\n0x{:04x} {}", emu.iptr, instr);
            let maybe_err = io::stdin().read(&mut [0 as u8; 1]);
            if maybe_err.is_err() {
                println!("Failed to read stdin: {}", maybe_err.err().unwrap());
                return;
            }
        }

        emu.exec(instr);

        if opts.step {
            println!("{}", emu);
        }
    }
}

fn do_emulate(argv0: &str, args: &mut env::Args) -> i32 {
    let mut opts = EmuOpts { step: false };
    let mut path: Option<String> = None;

    for arg in args {
        if arg == "--step" {
            opts.step = true
        } else if path.is_none() {
            path = Some(arg)
        } else {
            usage(argv0);
            return 1;
        }
    }

    if path.is_none() {
        usage(argv0);
        return 1;
    }
    let path = path.unwrap();

    let data = fs::read(&path);
    if data.is_err() {
        println!("{}: {}", path, data.err().unwrap());
        return 1;
    }
    let data = data.unwrap();

    run_emulator(&data, &opts);
    0
}

fn main() {
    let mut args = env::args();
    let argv0 = args.next().unwrap();

    loop {
        let arg = args.next();
        if arg.is_none() {
            break;
        }
        let arg = arg.unwrap();

        if arg == "--help" || arg == "-h" {
            usage(&argv0);
            process::exit(0);
        } else if arg.starts_with("-") {
            usage(&argv0);
            process::exit(1);
        } else if arg == "emulate" {
            process::exit(do_emulate(&argv0, &mut args));
        } else {
            usage(&argv0);
            process::exit(1);
        }
    }

    usage(&argv0);
}
