#!/usr/bin/env python3

import sys

FMT_2OP = 0
FMT_1OP = 1
FMT_IMM = 3
FMT_SMEM = 4
FMT_DMEM = 5

ops = {
    "add":  (FMT_2OP, 0b0000),
    "sub":  (FMT_2OP, 0b0001),
    "xor":  (FMT_2OP, 0b0010),
    "and":  (FMT_2OP, 0b0011),
    "or":   (FMT_2OP, 0b0100),
    "mov":  (FMT_2OP, 0b0101),
    "shr":  (FMT_2OP, 0b0110),
    "cmp":  (FMT_2OP, 0b0111),
    "call": (FMT_1OP, 0b1000),
    "b":    (FMT_1OP, 0b1001),
    "beq":  (FMT_1OP, 0b1010),
    "blt":  (FMT_1OP, 0b1011),
    "lds":  (FMT_SMEM, 0b1100),
    "sts":  (FMT_SMEM, 0b1101),
    "ldd":  (FMT_DMEM, 0b1100),
    "std":  (FMT_DMEM, 0b1101),
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
    else:
        return int(imm)

def make_fmt_r(opcode, dbit, reg):
    return opcode << 4 | dbit << 3 | reg

def make_fmt_i(opcode, imm):
    return opcode << 4 | imm

def assemble_line(line):
    parts = list(map(lambda x: x.lower(), line.split()))
    if len(parts) == 0:
        raise AsmError("Empty line")
    if parts[0] not in ops:
        raise AsmError("Unknown operation: " + parts[0])

    def argcount(n):
        if len(parts) - 1 != n:
            raise AsmError(
                "Operation " + parts[0] + " expected " +
                n + " arguments, got " + len(parts) - 1)

    opfmt, opcode = ops[parts[0]]
    if opfmt == FMT_2OP:
        argcount(2)
        dbit, reg = parse_pair(parts[1], parts[2])
        return make_fmt_r(opcode, dbit, reg)
    elif opfmt == FMT_1OP:
        argcount(1)
        dbit, reg = parse_source(parts[1])
        return make_fmt_r(opcode, dbit, reg)
    elif opfmt == FMT_SMEM:
        argcount(1)
        reg = parse_reg(parts[1])
        return make_fmt_r(opcode, 0, reg)
    elif opfmt == FMT_DMEM:
        argcount(1)
        reg = parse_reg(parts[1])
        return make_fmt_r(opcode, 1, reg)
    elif opfmt == FMT_IMM:
        argcount(1)
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

def assemble(lines):
    for linenum, line in iter_lines(lines):
        yield assemble_line(line)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: " + sys.argv[0] + " <infile> <outfile>")
        exit(1)

    with open(sys.argv[1]) as infile:
        with open(sys.argv[2], "wb") as outfile:
            for instr in assemble(infile):
                outfile.write(bytes([instr]))
