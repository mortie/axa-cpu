mod assembler;
mod compiler;
mod devices;
mod emulator;
mod isa;

use std::env;
use std::fs::File;
use std::io;
use std::io::sink;
use std::io::{Read, Write};
use std::path::Path;
use std::process;
use std::thread;
use std::time::Duration;

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

struct Opts {
    do_emulate: bool,
    do_print_asm: bool,
}

impl Opts {
    fn new() -> Self {
        Self {
            do_emulate: false,
            do_print_asm: false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum FileType {
    Ax,
    Asm,
    MachineCode,
}

type Input = (Box<dyn Read>, FileType);
type Output = (Box<dyn Write>, FileType);

fn get_file_type(path: &Path) -> Result<FileType, String> {
    match path.extension().and_then(|x| x.to_str()) {
        Some("ax") => Ok(FileType::Ax),
        Some("s") => Ok(FileType::Asm),
        Some("o") | None => Ok(FileType::MachineCode),
        Some(ext) => Err(format!("Unknown file extension: {}", ext)),
    }
}

fn open_file(path: &Path) -> Result<Input, String> {
    let t = get_file_type(path)?;
    let f = match File::open(path) {
        Ok(f) => f,
        Err(err) => return Err(format!("{}: {}", path.to_string_lossy(), err)),
    };
    let r: Box<dyn Read> = Box::new(f);
    Ok((r, t))
}

fn create_file(path: &Path) -> Result<Output, String> {
    let t = get_file_type(path)?;
    let f = match File::create(path) {
        Ok(f) => f,
        Err(err) => return Err(err.to_string()),
    };
    let w: Box<dyn Write> = Box::new(f);
    Ok((w, t))
}

fn print_asm(code: &compiler::codegen::Code) {
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

    for instr in &code.code {
        loop {
            if annotation_idx >= code.annotations.len() {
                break;
            }

            let (annotation_addr, annotation) = &code.annotations[annotation_idx];
            if *annotation_addr != addr {
                break;
            }

            match annotation {
                compiler::codegen::Annotation::Indent(text) => {
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
                compiler::codegen::Annotation::Dedent => {
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
}

fn ax_to_machine_code(
    input: Box<dyn Read>,
    output: &mut dyn Write,
    opts: Opts,
) -> Result<(), String> {
    let mut lexer = compiler::lexer::Lexer::new(input);
    let program = match compiler::parser::parse_program(&mut lexer) {
        Ok(program) => program,
        Err(err) => return Err(err.to_string()),
    };

    let gen = compiler::codegen::Context::new(program);
    let code = compiler::codegen::generate(gen)?;
    match output.write(&code.code[..]) {
        Err(err) => return Err(err.to_string()),
        _ => (),
    }

    if opts.do_print_asm {
        print_asm(&code);
    }

    Ok(())
}

fn ax_to_asm(input: Box<dyn Read>, output: &mut dyn Write, opts: Opts) -> Result<(), String> {
    let mut vec = Vec::new();
    ax_to_machine_code(input, &mut vec, opts)?;

    for ibyte in vec {
        let instr = isa::Instr::parse(ibyte);
        match output.write(&format!("{}\n", instr).as_bytes()) {
            Err(err) => return Err(err.to_string()),
            _ => (),
        }
    }

    Ok(())
}

fn asm_to_machine_code(
    mut input: Box<dyn Read>,
    output: &mut dyn Write,
    _opts: Opts,
) -> Result<(), String> {
    assembler::assemble(&mut *input, output)
}

fn file_to_machine_code(input: Input, opts: Opts) -> Result<Vec<u8>, String> {
    let (mut infile, ftype) = input;
    let mut code = Vec::new();
    match ftype {
        FileType::Ax => ax_to_machine_code(infile, &mut code, opts)?,
        FileType::Asm => asm_to_machine_code(infile, &mut code, opts)?,
        FileType::MachineCode => match infile.read_to_end(&mut code) {
            Err(err) => return Err(err.to_string()),
            _ => (),
        },
    }

    Ok(code)
}

fn run_emulator(data: &Vec<u8>, opts: &EmuOpts) {
    let memory = devices::Memory::new();
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
        let instr = isa::Instr::parse(memory.data[emu.iptr as usize].get());

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

fn usage(argv0: &str) {
    println!("Usage: {} [options] <input-file>", argv0);
    println!();
    println!("Options:");
    println!("  -o <file>:   Write result to <file>");
    println!("  --print-asm: Print annotated assembly code when compiling ax files");
    println!("  --run:       Run the emulator on the input file");
    println!("  --step:      Step through in the emulator");
    println!("  --hz <hz>:   Run emulator at <hz> Hz");
}

fn require_arg(name: &str, args: &mut env::Args) -> Result<String, String> {
    match args.next() {
        Some(arg) => Ok(arg),
        None => Err(format!("Option '{}' requires an argument", name)),
    }
}

fn main_impl() -> Result<(), String> {
    let mut input: Option<Input> = None;
    let mut output: Option<Output> = None;
    let mut opts = Opts::new();
    let mut emu_opts = EmuOpts::new();

    let mut args = env::args();
    let argv0 = match args.next() {
        Some(argv0) => argv0,
        None => return Err("No argv[0]".to_string()),
    };

    let mut dashes = false;
    loop {
        let arg = match args.next() {
            Some(arg) => arg,
            None => break,
        };

        if !dashes && arg == "--help" {
            usage(&argv0);
            return Ok(());
        } else if !dashes && arg == "-o" {
            let val = require_arg(&arg, &mut args)?;
            output = Some(create_file(Path::new(&val))?);
        } else if !dashes && arg == "--print-asm" {
            opts.do_print_asm = true;
        } else if !dashes && arg == "--run" {
            opts.do_emulate = true;
        } else if !dashes && arg == "--step" {
            emu_opts.step = true;
        } else if !dashes && arg == "--hz" {
            let val = require_arg(&arg, &mut args)?;
            emu_opts.hz = match val.parse::<u32>() {
                Ok(hz) => hz,
                Err(err) => return Err(err.to_string()),
            };
        } else if !dashes && arg == "--" {
            dashes = true;
        } else if !dashes && arg.chars().next() == Some('-') {
            return Err(format!("Unknown option: {}", arg));
        } else if input.is_none() {
            input = Some(open_file(Path::new(&arg))?);
        } else {
            return Err("Too many arguments".to_string());
        }
    }

    let input = match input {
        Some(input) => input,
        None => {
            usage(&argv0);
            println!();
            return Err("Missing 'input-file' argument".to_string());
        }
    };

    let output = match output {
        Some(output) => output,
        None => (Box::new(sink()) as Box<dyn Write>, FileType::MachineCode),
    };

    if opts.do_emulate {
        let code = file_to_machine_code(input, opts)?;
        run_emulator(&code, &emu_opts);
        return Ok(());
    }

    let (infile, intype) = input;
    let (mut outfile, outtype) = output;
    let outfile = &mut *outfile;
    match (intype, outtype) {
        (FileType::Ax, FileType::MachineCode) => ax_to_machine_code(infile, outfile, opts),
        (FileType::Ax, FileType::Asm) => ax_to_asm(infile, outfile, opts),
        (FileType::Asm, FileType::MachineCode) => asm_to_machine_code(infile, outfile, opts),
        _ => Err(format!(
            "Cannot transform a {:?} file into a {:?} file",
            intype, outtype
        )),
    }
}

fn main() {
    match main_impl() {
        Err(err) => {
            println!("Error: {}", err);
            process::exit(1);
        }
        Ok(()) => (),
    }
}
