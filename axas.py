#!/usr/bin/env python3

import sys
import os

FMT_2OP = 0
FMT_JMP = 1
FMT_MEM = 2
FMT_IMM = 3

ops = {
    "add":  (FMT_2OP, 0b0000),
    "sub":  (FMT_2OP, 0b0001),
    "xor":  (FMT_2OP, 0b0010),
    "and":  (FMT_2OP, 0b0011),
    "or":   (FMT_2OP, 0b0100),
    "mov":  (FMT_2OP, 0b0101),
    "shr":  (FMT_2OP, 0b0110),
    "cmp":  (FMT_2OP, 0b0111),
    "addc": (FMT_2OP, 0b1000),
    "shrc": (FMT_2OP, 0b1001),
    "cmpc": (FMT_2OP, 0b1010),
    "jmp":  (FMT_JMP, 0b1011, 0b0000),
    "call": (FMT_JMP, 0b1011, 0b0001),
    "b":    (FMT_JMP, 0b1011, 0b0010),
    "beq":  (FMT_JMP, 0b1011, 0b0011),
    "bgt":  (FMT_JMP, 0b1011, 0b0100),
    "bge":  (FMT_JMP, 0b1011, 0b0101),
    "bgts": (FMT_JMP, 0b1011, 0b0110),
    "bges": (FMT_JMP, 0b1011, 0b0111),
    "bb":   (FMT_JMP, 0b1011, 0b1010),
    "bbeq": (FMT_JMP, 0b1011, 0b1011),
    "bbgt": (FMT_JMP, 0b1011, 0b1100),
    "bbge": (FMT_JMP, 0b1011, 0b1101),
    "bbgts":(FMT_JMP, 0b1011, 0b1110),
    "bbges":(FMT_JMP, 0b1011, 0b1111),
    "lds":  (FMT_MEM, 0b1100, 0),
    "sts":  (FMT_MEM, 0b1101, 0),
    "ldd":  (FMT_MEM, 0b1100, 1),
    "std":  (FMT_MEM, 0b1101, 1),
    "imml": (FMT_IMM, 0b1110),
    "immh": (FMT_IMM, 0b1111),
}

regs = {
    "cs": 0b000,
    "ds": 0b001,
    "sp": 0b010,
    "rv": 0b011,
    "a1": 0b100,
    "a2": 0b101,
    "a3": 0b110,
    "ra": 0b111,
    "r0": 0b000,
    "r1": 0b001,
    "r2": 0b010,
    "r3": 0b011,
    "r4": 0b100,
    "r5": 0b101,
    "r6": 0b110,
    "r7": 0b111,
}

class AsmError(Exception):
    pass

def parse_pair(dest, src):
    if dest in regs and src == "acc":
        return 0, regs[dest]
    elif src in regs and dest == "acc":
        return 1, regs[src]
    elif src != "acc" and src not in regs:
        raise AsmError("Invalid source: " + src)
    elif dest != "acc" and dest not in regs:
        raise AsmError("Invalid destination: " + dest)
    else:
        raise AsmError("Unsupported destination/source pair: " + dest + " and " + src)

def parse_source(src):
    if src == "acc":
        return 0, 0
    elif src in regs:
        return 1, regs[src]
    else:
        raise AsmError("Invalid source: " + src)

def parse_reg(reg):
    if reg in regs:
        return regs[reg]
    elif reg == "acc":
        raise AsmError("Accumulator not allowed in this context")
    else:
        raise AsmError("Invalid register: " + src)

def parse_imm(imm):
    if imm.startswith("0x"):
        return int(imm[2:], 16)
    elif imm.startswith("0b"):
        return int(imm[2:], 2)
    elif imm.startswith("0o"):
        return int(imm[2:], 8)
    elif imm.startswith("'"):
        if len(imm) == 3 and imm[2] == "'":
            return ord(imm[1])
        elif len(imm) == 4 and imm[3] == "'" and imm[1] == "\\":
            ch = imm[2]
            if ch == "t":
                return ord("\t")
            elif ch == "n":
                return ord("\n")
            elif ch == "0":
                return 0
            else:
                raise AsmError("Invalid character literal escape sequence")
        else:
            raise AsmError("Invalid character literal")
    else:
        return int(imm)

def make_fmt_r(opcode, dbit, reg):
    return opcode << 4 | dbit << 3 | reg

def make_fmt_i(opcode, imm):
    return opcode << 4 | imm

def argcount(parts, n):
    if len(parts) - 1 != n:
        raise AsmError(
            "Operation " + parts[0] + " expected " +
            n + " arguments, got " + len(parts) - 1)

def assemble_line(parts):
    iname = parts[0]

    if iname not in ops:
        raise AsmError("Unknown operation: " + parts[0])

    op = ops[iname]
    opfmt = op[0]
    opcode = op[1]
    if opfmt == FMT_2OP:
        argcount(parts, 2)
        dbit, reg = parse_pair(parts[1], parts[2])
        return make_fmt_r(opcode, dbit, reg)
    elif opfmt == FMT_JMP:
        argcount(parts, 0)
        return make_fmt_r(opcode, op[2], reg)
    elif opfmt == FMT_MEM:
        argcount(parts, 1)
        reg = parse_reg(parts[1])
        return make_fmt_r(opcode, op[2], reg)
    elif opfmt == FMT_IMM:
        imm = parse_imm(parts[1])
        if opcode == ops["imml"][1]:
            imm = imm & 0x0f
        else:
            imm = (imm & 0xf0) >> 4
        return make_fmt_i(opcode, imm)

def iter_lines(lines):
    linenum = 1
    for line in lines:
        line = line.split("#")[0].strip()
        if line != "":
            yield linenum, line
        linenum += 1

def split_line(line):
    # This is what happens when you're too lazy to write a proper lexer
    xparts = line.split("' '")
    parts = []
    for xpart in xparts:
        for part in xpart.split():
            parts.append(part)
        parts.append("' '")
    parts.pop()
    return parts

def assemble(lines):
    for linenum, line in iter_lines(lines):
        parts = split_line(line)
        if len(parts) == 0:
            raise AsmError("Empty line")

        iname = parts[0]
        if iname == ".byte":
            argcount(parts, 1)
            yield parse_imm(parts[1])
        elif iname == ".zero":
            argcount(parts, 1)
            count = parse_imm(parts[1])
            for i in range(0, count):
                yield 0
        else:
            yield assemble_line(parts)

if __name__ == "__main__":
    infile = sys.stdin
    outfile = sys.stdout.buffer

    if len(sys.argv) > 1:
        infile = open(sys.argv[1])
    if len(sys.argv) > 2:
        outfile = open(sys.argv[2], "wb")
    if len(sys.argv) > 3:
        print("Usage: " + sys.argv[0] + " [infile] [outfile]")
        exit(1)

    if os.isatty(outfile.fileno()):
        print("Refusing to output binary data to a TTY.")
        exit(1)

    for instr in assemble(infile):
        outfile.write(bytes([instr]))
