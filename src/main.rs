mod assembler;
mod compiler;
mod emulator;
mod isa;

use compiler::codegen;
use compiler::lexer;
use compiler::parser;
use isa::*;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::process;
use std::thread;
use std::time::Duration;

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
    rows: [[u8; 32 / 8]; 32],
    x: Option<u8>,
}

impl DisplayBlock {
    fn new() -> Self {
        Self {
            rows: [[0u8; 32 / 8]; 32],
            x: None,
        }
    }

    fn len(&self) -> usize {
        self.rows.len() * self.rows[0].len() + 1
    }

    fn render(&self) {
        print!("\x1bc");
        let rowlen = self.rows[0].len();
        for rowidx in 0..(self.rows.len() / 2) {
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
    fn load(&mut self, _offset: u16) -> u8 {
        0
    }
    fn store(&mut self, _offset: u16, value: u8) {
        if value == 0xffu8 {
            self.render();
            for row in &mut self.rows {
                for pix in row {
                    *pix = 0;
                }
            }
        } else if let Some(x) = self.x {
            self.rows[value as usize][(x / 8u8) as usize] |= 1u8 << (x % 8);
            self.x = None;
        } else {
            self.x = Some(value);
        }
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

fn usage(argv0: &str) {
    println!("Usage: {} emulate  [--step] <path>", argv0);
    println!("       {} assemble [in] [out]", argv0);
    println!("       {} run      [--step] <path>", argv0);
    println!("       {} parse    <path>", argv0);
    println!("       {} compile  [in] [out]", argv0);
}

struct EmuOpts {
    step: bool,
    hz: u32,
}

impl EmuOpts {
    fn new() -> Self {
        Self {
            step: false,
            hz: 20,
        }
    }
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

    let ctrl_byte = RefCell::new(0u8);
    let mut ctrl = ControlBlock { data: &ctrl_byte };
    emu.map_memory(0xffff, 1, &mut ctrl);

    if opts.step {
        println!("{}", emu);
    }

    let sleep_dur = match opts.hz {
        0 => Duration::new(0, 0),
        hz => Duration::from_nanos(((1f64 / hz as f64) * 1000000000f64) as u64),
    };

    while *ctrl_byte.borrow() == 0 {
        let instr = Instr::parse(emu.load(emu.iptr));

        if opts.step {
            println!("\n0x{:04x} {}", emu.iptr, instr);
            let maybe_err = io::stdin().read(&mut [0u8; 1]);
            if maybe_err.is_err() {
                println!("Failed to read stdin: {}", maybe_err.err().unwrap());
                return;
            }
        } else if opts.hz > 0 {
            thread::sleep(sleep_dur);
        }

        emu.exec(instr);

        if opts.step {
            println!("{}", emu);
        }
    }

    if !opts.step {
        println!("{}", emu);
    }
}

fn do_emulate(argv0: &str, args: &mut env::Args) -> i32 {
    let mut opts = EmuOpts::new();
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

    let path = match path {
        Some(path) => path,
        None => {
            usage(&argv0);
            return 1;
        }
    };

    let data = match fs::read(&path) {
        Ok(data) => data,
        Err(err) => {
            println!("{}: {}", path, err);
            return 1;
        }
    };

    run_emulator(&data, &opts);
    0
}

fn do_assemble(argv0: &str, args: &mut env::Args) -> i32 {
    let mut inpath: Option<String> = None;
    let mut outpath: Option<String> = None;

    for arg in args {
        if inpath.is_none() {
            inpath = Some(arg);
        } else if outpath.is_none() {
            outpath = Some(arg);
        } else {
            usage(argv0);
            return 1;
        }
    }

    let mut instream: Box<dyn Read> = match inpath {
        Some(path) => match fs::File::open(&path) {
            Ok(f) => Box::new(f),
            Err(err) => {
                println!("{}: {}", path, err);
                return 1;
            }
        },
        None => Box::new(io::stdin()),
    };

    let mut outstream: Box<dyn Write> = match outpath {
        Some(path) => match fs::File::create(&path) {
            Ok(f) => Box::new(f),
            Err(err) => {
                println!("{}: {}", path, err);
                return 1;
            }
        },
        None => Box::new(io::stdout()),
    };

    if let Err(err) = assembler::assemble(&mut *instream, &mut *outstream) {
        println!("{}", err);
        return 1;
    }

    0
}

fn do_run(argv0: &str, args: &mut env::Args) -> i32 {
    let mut opts = EmuOpts::new();
    let mut path: Option<String> = None;

    loop {
        let arg = match args.next() {
            Some(arg) => arg,
            None => break,
        };

        if arg == "--step" {
            opts.step = true;
        } else if arg == "--hz" {
            let hz = match args.next() {
                Some(hz) => hz,
                None => {
                    println!("--hz requires an argument");
                    return 1;
                }
            };

            opts.hz = match hz.parse::<u32>() {
                Ok(hz) => hz,
                Err(err) => {
                    println!("Failed to parse hz: {}", err.to_string());
                    return 1;
                }
            }
        } else if path.is_none() {
            path = Some(arg)
        } else {
            usage(argv0);
            return 1;
        }
    }

    let path = match path {
        Some(path) => path,
        None => {
            usage(&argv0);
            return 1;
        }
    };

    let mut infile = match fs::File::open(&path) {
        Ok(f) => f,
        Err(err) => {
            println!("{}: {}", path, err);
            return 1;
        }
    };

    let mut data: Vec<u8> = Vec::new();
    if let Err(err) = assembler::assemble(&mut infile, &mut data) {
        println!("{}", err);
        return 1;
    }

    run_emulator(&data, &opts);
    0
}

fn do_parse(argv0: &str, args: &mut env::Args) -> i32 {
    let path = args.next();
    if path.is_none() || args.next().is_some() {
        usage(argv0);
        return 1;
    }

    let path = path.unwrap();

    let infile = match fs::File::open(&path) {
        Ok(f) => f,
        Err(err) => {
            println!("{}: {}", path, err);
            return 1;
        }
    };

    let mut lexer = lexer::Lexer::new(Box::new(infile));
    let program = match parser::parse_program(&mut lexer) {
        Ok(program) => program,
        Err(err) => {
            println!("{}: {}", path, err);
            return 1;
        }
    };

    println!("Made AST: {:#?}", program);

    0
}

fn do_compile(argv0: &str, args: &mut env::Args) -> i32 {
    let mut inpath: Option<String> = None;
    let mut outpath: Option<String> = None;

    for arg in args {
        if inpath.is_none() {
            inpath = Some(arg);
        } else if outpath.is_none() {
            outpath = Some(arg);
        } else {
            usage(argv0);
            return 1;
        }
    }

    let instream: Box<dyn Read> = match inpath {
        Some(path) => match fs::File::open(&path) {
            Ok(f) => Box::new(f),
            Err(err) => {
                println!("{}: {}", path, err);
                return 1;
            }
        },
        None => Box::new(io::stdin()),
    };

    let mut outstream: Box<dyn Write> = match outpath {
        Some(path) => match fs::File::create(&path) {
            Ok(f) => Box::new(f),
            Err(err) => {
                println!("{}: {}", path, err);
                return 1;
            }
        },
        None => Box::new(io::stdout()),
    };

    let mut lexer = lexer::Lexer::new(instream);
    let program = match parser::parse_program(&mut lexer) {
        Ok(program) => program,
        Err(err) => {
            println!("{}", err);
            return 1;
        }
    };

    let mut gen = codegen::Context::new(&program);
    match gen.generate() {
        Err(err) => {
            println!("{}", err);
            return 1;
        }
        _ => (),
    }

    let mut addr = 0u16;
    for instr in &gen.code {
        if let Some(text) = gen.annotations.get(&addr) {
            println!("{}:", text);
        }
        println!("    0x{:04x} {}", addr, isa::Instr::parse(*instr));
        addr += 1;
    }

    match outstream.write_all(&gen.code) {
        Err(err) => {
            println!("{}", err);
            return 1;
        }
        _ => (),
    }

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
        } else if arg == "assemble" {
            process::exit(do_assemble(&argv0, &mut args));
        } else if arg == "run" {
            process::exit(do_run(&argv0, &mut args));
        } else if arg == "parse" {
            process::exit(do_parse(&argv0, &mut args));
        } else if arg == "compile" {
            process::exit(do_compile(&argv0, &mut args));
        } else {
            usage(&argv0);
            process::exit(1);
        }
    }

    usage(&argv0);
}
