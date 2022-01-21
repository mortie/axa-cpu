mod assembler;
mod compiler;
mod emulator;
mod isa;

use compiler::codegen;
use compiler::lexer;
use compiler::parser;
use isa::*;
use std::env;
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::process;
use std::thread;
use std::time::Duration;
use std::cell::Cell;

const DISPLAY_WIDTH: usize = 32;
const DISPLAY_HEIGHT: usize = 32;

struct DisplayDevice {
    data: Vec<Cell<u8>>,
    x: Cell<Option<u8>>,
}

impl DisplayDevice {
    fn new() -> Self {
        let mut data: Vec<Cell<u8>> = Vec::new();
        data.resize((DISPLAY_WIDTH / 8) * DISPLAY_HEIGHT, Cell::new(0));
        Self {
            data,
            x: Cell::new(None),
        }
    }

    fn get_pixel(&self, x: usize, y: usize) -> bool {
        let xbyte = x / 8;
        let xbit = x % 8;
        let cell = &self.data[y * DISPLAY_WIDTH + xbyte];
        let byte = cell.get();
        match byte & (1 << xbit) {
            0 => false,
            _ => true,
        }
    }

    fn set_pixel(&self, x: usize, y: usize) {
        let xbyte = x / 8;
        let xbit = x % 8;
        let cell = &self.data[y * DISPLAY_WIDTH + xbyte];
        let mut byte = cell.get();
        byte |= 1 << xbit;
        cell.set(byte);
    }

    fn len(&self) -> usize {
        self.data.len()
    }

    fn render(&self) {
        print!("\x1bc");
        for y in 0..(DISPLAY_HEIGHT / 2) {
            let y = y * 2;
            for x in 0..DISPLAY_WIDTH {
                let a = self.get_pixel(x, y);
                let b = self.get_pixel(x, y + 1);

                if a && b {
                    print!("█");
                } else if a && !b {
                    print!("▀");
                } else if !a && b {
                    print!("▄");
                } else {
                    print!(" ");
                }
            }
            print!("\n");
        }
    }

    fn store(&self, _offset: u16, value: u8) {
        if value == 0xffu8 {
            self.render();
            for cell in &self.data {
                cell.set(0);
            }
            self.x.set(None);
        } else if let Some(x) = self.x.get() {
            self.set_pixel(x as usize, value as usize);
        } else {
            self.x.set(Some(value));
        }
    }
}

struct Memory {
    data: Vec<Cell<u8>>,
    halt: Cell<bool>,
    display: DisplayDevice,
}

impl Memory {
    fn new() -> Self {
        let mut data: Vec<Cell<u8>> = Vec::new();
        data.resize(1024, Cell::new(0));
        Self {
            data,
            halt: Cell::new(false),
            display: DisplayDevice::new(),
        }
    }
}

impl emulator::Memory for Memory {
    fn load(&self, addr: u16) -> u8 {
        println!("Load from address {}", addr);

        if (addr as usize) < self.data.len() {
            return self.data[addr as usize].get();
        }

        if addr >= 0x8000 && (addr as usize) < 0x8000 + self.display.len() {
            return 0;
        }

        if addr == 0xff00 {
            return 0;
        }

        if addr == 0xffff {
            return 0;
        }

        println!("Load from wild address {}!", addr);
        0
    }

    fn store(&self, addr: u16, value: u8) {
        if (addr as usize) < self.data.len() {
            self.data[addr as usize].set(value);
            return;
        }

        if addr >= 0x8000 && (addr as usize) < 0x8000 + self.display.len() {
            return self.display.store(addr - 0x8000, value);
        }

        if addr == 0xff00 {
            println!("Debug store: {}", value);
            return;
        }

        if addr == 0xffff {
            self.halt.set(true);
            return;
        }

        println!("Store of {} to wild address {}!", value, addr);
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
    let memory = Memory::new();
    if data.len() > memory.data.len() {
        println!(
            "Program too long! Have {} bytes of RAM, program is {} bytes",
            memory.data.len(),
            data.len()
        );
        return;
    }

    for idx in 0..data.len() {
        memory.data[idx].set(data[idx]);
    }

    let mut emu = emulator::Emulator::new(&memory);

    if opts.step {
        println!("{}", emu);
    }

    let sleep_dur = match opts.hz {
        0 => Duration::new(0, 0),
        hz => Duration::from_nanos(((1f64 / hz as f64) * 1000000000f64) as u64),
    };

    while !memory.halt.get() {
        let instr = Instr::parse(memory.data[emu.iptr as usize].get());

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
    let mut annotation_idx = 0;
    let mut annotation_depth = 0;
    let mut in_stack = false;
    let mut in_stack_depth = 0;
    let print_space = |depth| {
        for _ in 0..depth {
            print!("    ");
        }
    };

    for instr in &gen.code {
        loop {
            if annotation_idx >= gen.annotations.len() {
                break;
            }

            let (annotation_addr, annotation) = &gen.annotations[annotation_idx];
            if *annotation_addr != addr {
                break;
            }

            match annotation {
                codegen::Annotation::Indent(text) => {
                    print_space(annotation_depth);
                    println!("; {}", text);
                    annotation_depth += 1;

                    if text == "Stack" {
                        in_stack = true;
                        in_stack_depth = 1;
                        print_space(annotation_depth);
                        println!("[Skipping stack bytes]");
                    } else if in_stack {
                        in_stack_depth += 1;
                    }
                }
                codegen::Annotation::Dedent => {
                    annotation_depth -= 1;
                    if in_stack {
                        in_stack_depth -= 1;
                        if in_stack_depth == 0 {
                            in_stack = false;
                        }
                    }
                }
            }

            annotation_idx += 1;
        }

        if !in_stack {
            print_space(annotation_depth);
            println!("0x{:04x} {}", addr, isa::Instr::parse(*instr));
        }

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
