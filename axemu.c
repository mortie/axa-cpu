#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

enum emu_op {
	EMU_OP_ADD =  0,
	EMU_OP_SUB =  1,
	EMU_OP_XOR =  2,
	EMU_OP_AND =  3,
	EMU_OP_OR =   4,
	EMU_OP_MOV =  5,
	EMU_OP_SHR =  6,
	EMU_OP_CMP =  7,
	EMU_OP_ADDC = 8,
	EMU_OP_SHRC = 9,
	EMU_OP_CMPC = 10,
	EMU_OP_JMP =  11,
	EMU_OP_LD =   12,
	EMU_OP_ST =   13,
	EMU_OP_IMML = 14,
	EMU_OP_IMMH = 15,
};

enum emu_jmp_op {
	EMU_J_JMP =  0,
	EMU_J_CALL = 1,
	EMU_J_B =    2,
	EMU_J_BEQ =  3,
	EMU_J_BGT =  4,
	EMU_J_BGE =  5,
	EMU_J_BGTS = 6,
	EMU_J_BGES = 7,
};

enum emu_reg {
	EMU_REG_CS = 0,
	EMU_REG_DS = 1,
	EMU_REG_SP = 2,
	EMU_REG_RV = 3,
	EMU_REG_A1 = 4,
	EMU_REG_A2 = 5,
	EMU_REG_A3 = 6,
	EMU_REG_RA = 7,
};

struct emu_mem_block;

struct emu_mem_vt {
	uint8_t (*read)(void *self, size_t offset);
	void (*write)(void *self, size_t offset, uint8_t data);
};

struct emu_mem_map {
	struct emu_mem_vt *vt;
	size_t start;
	size_t length;
};

struct emu {
	struct emu_mem_map *mem_maps;
	size_t mem_maps_len;

	uint8_t regs[8];
	uint8_t acc;
	uint16_t iptr;
	unsigned int zflag: 1;
	unsigned int cflag: 1;
	unsigned int oflag: 1;
};

void emu_init(struct emu *emu) {
	emu->mem_maps = NULL;
	emu->mem_maps_len = 0;
	memset(emu->regs, 0, sizeof(emu->regs));
	emu->acc = 0;
	emu->iptr = 0;
	emu->zflag = 0;
	emu->cflag = 0;
	emu->oflag = 0;
}

void emu_add_mem(struct emu *emu, struct emu_mem_vt *vt, size_t start, size_t length) {
	size_t idx = emu->mem_maps_len;
	emu->mem_maps_len += 1;
	emu->mem_maps = realloc(emu->mem_maps, emu->mem_maps_len * sizeof(*emu->mem_maps));
	emu->mem_maps[idx].vt = vt;
	emu->mem_maps[idx].start = start;
	emu->mem_maps[idx].length = length;
}

uint8_t emu_load(struct emu *emu, size_t addr) {
	for (size_t i = 0; i < emu->mem_maps_len; ++i) {
		struct emu_mem_map *map = &emu->mem_maps[i];
		if (map->start <= addr && map->start + map->length > addr) {
			return map->vt->read(map->vt, addr - map->start);
		}
	}

	return 0;
}

void emu_store(struct emu *emu, size_t addr, uint8_t data) {
	for (size_t i = 0; i < emu->mem_maps_len; ++i) {
		struct emu_mem_map *map = &emu->mem_maps[i];
		if (map->start <= addr && map->start + map->length > addr) {
			map->vt->write(map->vt, addr - map->start, data);
			return;
		}
	}
}

void emu_update_flags(struct emu *emu, uint16_t a, uint16_t b, uint16_t res) {
	emu->zflag = (res & 0xff) == 0;
	emu->cflag = (res & 0x100) >> 8;

	emu->oflag =
		((a & 0x40) && (b & 0x40) && (res & 0x80)) ||
		((a & 0x80) && (b & 0x80) && ((res & 0x80) == 0));
}

void emu_exec(struct emu *emu, uint8_t instr) {
	enum emu_op op = (instr & 0xf0) >> 4;
	uint8_t dbit = (instr & 0x08) >> 3;
	uint8_t reg = instr & 0x07;
	uint8_t imm = instr & 0x0f;

	uint16_t a;
	uint16_t b;
	uint16_t res;
	uint8_t *dst;

	if (dbit == 0) {
		a = emu->acc;
		b = emu->regs[reg];
		dst = &emu->regs[reg];
	} else {
		a = emu->regs[reg];
		b = emu->acc;
		dst = &emu->acc;
	}

	uint8_t acc;
	uint16_t boffset;

	switch (op) {
	case EMU_OP_ADD:
		res = a + b;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_SUB:
		b = ~b & 0xff;
		res = a + b + 1;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_XOR:
		res = a ^ b;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_AND:
		res = a & b;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_OR:
		res = a | b;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_MOV:
		*dst = (uint8_t)a;
		emu->iptr += 1;
		break;
	case EMU_OP_SHR:
		res = a >> 1;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->cflag = a & 1;
		emu->iptr += 1;
		break;
	case EMU_OP_CMP:
		b = ~b & 0xff;
		res = a + b + 1;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_ADDC:
		res = a + b + emu->cflag;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_SHRC:
		res = (a >> 1) | (emu->cflag << 7);
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->cflag = a & 1;
		emu->iptr += 1;
		break;
	case EMU_OP_CMPC:
		b = ~b & 0xff;
		res = a + b + emu->cflag;
		*dst = (uint8_t)res;
		emu_update_flags(emu, a, b, res);
		emu->iptr += 1;
		break;
	case EMU_OP_JMP:
		if (dbit) {
			acc = emu->acc | 0xf0;
			boffset = emu->acc | 0xfff0;
		} else {
			acc = emu->acc;
			boffset = acc;
		}

		switch ((enum emu_jmp_op)reg) {
		case EMU_J_JMP:
			emu->iptr = (uint16_t)emu->regs[EMU_REG_CS] << 8 | acc;
			break;
		case EMU_J_CALL:
			emu->regs[EMU_REG_RA] = emu->iptr + 1;
			emu->iptr = (uint16_t)emu->regs[EMU_REG_CS] << 8 | acc;
			break;
		case EMU_J_B:
			emu->iptr += boffset;
			break;
		case EMU_J_BEQ:
			if (emu->zflag) {
				emu->iptr += boffset;
			} else {
				emu->iptr += 1;
			}
			break;
		case EMU_J_BGT:
			if (emu->cflag && !emu->zflag) {
				emu->iptr += boffset;
			} else {
				emu->iptr += 1;
			}
			break;
		case EMU_J_BGE:
			if (emu->cflag) {
				emu->iptr += boffset;
			} else {
				emu->iptr += 1;
			}
			break;
		case EMU_J_BGTS:
			if (emu->oflag && !emu->zflag) {
				emu->iptr += boffset;
			} else {
				emu->iptr += 1;
			}
			break;
		case EMU_J_BGES:
			if (emu->oflag) {
				emu->iptr += boffset;
			} else {
				emu->iptr += 1;
			}
			break;
		}
		break;
	case EMU_OP_LD:
		if (dbit) {
			emu->regs[reg] = emu_load(emu, (uint16_t)emu->regs[EMU_REG_DS] << 8 | emu->acc);
		} else {
			emu->regs[reg] = emu_load(emu, emu->acc);
		}
		break;
	case EMU_OP_ST:
		if (dbit) {
			emu_store(emu, (uint16_t)emu->regs[EMU_REG_DS] << 8 | emu->acc, emu->regs[reg]);
		} else {
			emu_store(emu, emu->acc, emu->regs[reg]);
		}
		emu->iptr += 1;
		break;
	case EMU_OP_IMML:
		emu->acc = imm;
		emu->iptr += 1;
		break;
	case EMU_OP_IMMH:
		emu->acc = imm << 4;
		emu->iptr += 1;
		break;
	}
}

void emu_step(struct emu *emu) {
	emu_exec(emu, emu_load(emu, emu->iptr));
}

void emu_print_state(struct emu *emu) {
	printf("Acc: %02x\n", emu->acc);
	printf("Regs: CS:%02x DS:%02x SP:%02x RV:%02x A0:%02x A1:%02x A2:%02x RA:%02x\n",
			emu->regs[0], emu->regs[1], emu->regs[2], emu->regs[3],
			emu->regs[4], emu->regs[5], emu->regs[6], emu->regs[7]);
	printf("Flags: Z:%d C:%d O:%d\n",
			emu->zflag, emu->cflag, emu->oflag);
}

void emu_print_instr(uint8_t instr) {
	enum emu_op op = (instr & 0xf0) >> 4;
	uint8_t dbit = (instr & 0x08) >> 3;
	uint8_t reg = instr & 0x07;
	uint8_t imm = instr & 0x0f;

	char *opname;
	switch (op) {
	case EMU_OP_ADD:  opname = "add";  break;
	case EMU_OP_SUB:  opname = "sub";  break;
	case EMU_OP_XOR:  opname = "xor";  break;
	case EMU_OP_AND:  opname = "and";  break;
	case EMU_OP_OR:   opname = "or";   break;
	case EMU_OP_MOV:  opname = "mov";  break;
	case EMU_OP_SHR:  opname = "shr";  break;
	case EMU_OP_CMP:  opname = "cmp";  break;
	case EMU_OP_ADDC: opname = "addc"; break;
	case EMU_OP_SHRC: opname = "shrc"; break;
	case EMU_OP_CMPC: opname = "cmpc"; break;
	case EMU_OP_JMP:  opname = NULL;   break;
	case EMU_OP_LD:   opname = "ld";   break;
	case EMU_OP_ST:   opname = "st";   break;
	case EMU_OP_IMML: opname = "imml"; break;
	case EMU_OP_IMMH: opname = "immh"; break;
	}

	char *regname;
	switch (reg) {
	case EMU_REG_CS: regname = "cs"; break;
	case EMU_REG_DS: regname = "ds"; break;
	case EMU_REG_SP: regname = "sp"; break;
	case EMU_REG_RV: regname = "rv"; break;
	case EMU_REG_A1: regname = "a1"; break;
	case EMU_REG_A2: regname = "a2"; break;
	case EMU_REG_A3: regname = "a3"; break;
	case EMU_REG_RA: regname = "ra"; break;
	}

	if (op == EMU_OP_JMP) {
		switch ((enum emu_jmp_op)reg) {
		case EMU_J_JMP:  opname = "jmp";  break;
		case EMU_J_CALL: opname = "call"; break;
		case EMU_J_B:    opname = "b";    break;
		case EMU_J_BEQ:  opname = "beq";  break;
		case EMU_J_BGT:  opname = "bgt";  break;
		case EMU_J_BGE:  opname = "bge";  break;
		case EMU_J_BGTS: opname = "bgts"; break;
		case EMU_J_BGES: opname = "bges"; break;
		}

		if (dbit) {
			printf("b%s", opname);
		} else {
			printf("%s", opname);
		}
	} else if (op == EMU_OP_LD || op == EMU_OP_ST) {
		if (dbit) {
			printf("%sd %s", opname, regname);
		} else {
			printf("%ss %s", opname, regname);
		}
	} else if (op == EMU_OP_IMML) {
		printf("%s %02x", opname, imm);
	} else if (op == EMU_OP_IMMH) {
		printf("%s %02x", opname, imm << 4);
	} else {
		if (dbit) {
			printf("%s acc %s", opname, regname);
		} else {
			printf("%s %s acc", opname, regname);
		}
	}
}

struct ram_block {
	struct emu_mem_vt vt;
	uint8_t *data;
};

uint8_t ram_block_read(void *self, size_t offset) {
	struct ram_block *block = self;
	return block->data[offset];
}

void ram_block_write(void *self, size_t offset, uint8_t data) {
	struct ram_block *block = self;
	block->data[offset] = data;
}

int main(int argc, char **argv) {
	if (argc != 2) {
		printf("Usage: %s <binary>\n", argv[0]);
		return 1;
	}

	FILE *binf = fopen(argv[1], "rb");
	if (binf == NULL) {
		perror(argv[1]);
		return 1;
	}

	struct emu emu;
	emu_init(&emu);

	size_t ram_size = 1024 * 1024;
	uint8_t *ram_data = malloc(ram_size);
	struct ram_block ram = {
		.vt.read = ram_block_read,
		.vt.write = ram_block_write,
		.data = ram_data,
	};
	emu_add_mem(&emu, &ram.vt, 0, ram_size);

	fread(ram_data, 1, ram_size, binf);
	fclose(binf);

	emu_print_state(&emu);

	while (1) {
		uint8_t instr = emu_load(&emu, emu.iptr);
		printf("== %04x: %02x -- ", emu.iptr, instr);
		emu_print_instr(instr);
		printf("\n");
		getchar();
		emu_exec(&emu, instr);
		emu_print_state(&emu);
	}
}
