#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

enum axa_op {
	AXA_OP_ADD =  0,
	AXA_OP_SUB =  1,
	AXA_OP_XOR =  2,
	AXA_OP_AND =  3,
	AXA_OP_OR =   4,
	AXA_OP_MOV =  5,
	AXA_OP_SHR =  6,
	AXA_OP_CMP =  7,
	AXA_OP_ADDC = 8,
	AXA_OP_SHRC = 9,
	AXA_OP_CMPC = 10,
	AXA_OP_JMP =  11,
	AXA_OP_LD =   12,
	AXA_OP_ST =   13,
	AXA_OP_IMML = 14,
	AXA_OP_IMMH = 15,
};

enum axa_jmp_op {
	AXA_J_JMP =  0,
	AXA_J_CALL = 1,
	AXA_J_B =    2,
	AXA_J_BEQ =  3,
	AXA_J_BGT =  4,
	AXA_J_BGE =  5,
	AXA_J_BGTS = 6,
	AXA_J_BGES = 7,
};

enum axa_reg {
	AXA_REG_CS = 0,
	AXA_REG_DS = 1,
	AXA_REG_SP = 2,
	AXA_REG_RV = 3,
	AXA_REG_A1 = 4,
	AXA_REG_A2 = 5,
	AXA_REG_A3 = 6,
	AXA_REG_RA = 7,
};

struct axa_mem_block;

struct axa_mem_vt {
	uint8_t (*read)(void *self, size_t offset);
	void (*write)(void *self, size_t offset, uint8_t data);
};

struct axa_mem_map {
	struct axa_mem_vt *vt;
	size_t start;
	size_t length;
};

struct axa {
	struct axa_mem_map *mem_maps;
	size_t mem_maps_len;

	uint8_t regs[8];
	uint8_t acc;
	uint16_t iptr;
	unsigned int zflag: 1;
	unsigned int cflag: 1;
	unsigned int oflag: 1;
};

void axa_init(struct axa *axa) {
	axa->mem_maps = NULL;
	axa->mem_maps_len = 0;
	memset(axa->regs, 0, sizeof(axa->regs));
	axa->acc = 0;
	axa->iptr = 0;
	axa->zflag = 0;
	axa->cflag = 0;
	axa->oflag = 0;
}

void axa_add_mem(struct axa *axa, struct axa_mem_vt *vt, size_t start, size_t length) {
	size_t idx = axa->mem_maps_len;
	axa->mem_maps_len += 1;
	axa->mem_maps = realloc(axa->mem_maps, axa->mem_maps_len * sizeof(*axa->mem_maps));
	axa->mem_maps[idx].vt = vt;
	axa->mem_maps[idx].start = start;
	axa->mem_maps[idx].length = length;
}

uint8_t axa_load(struct axa *axa, size_t addr) {
	for (size_t i = 0; i < axa->mem_maps_len; ++i) {
		struct axa_mem_map *map = &axa->mem_maps[i];
		if (map->start <= addr && map->start + map->length > addr) {
			return map->vt->read(map->vt, addr - map->start);
		}
	}

	return 0;
}

void axa_store(struct axa *axa, size_t addr, uint8_t data) {
	for (size_t i = 0; i < axa->mem_maps_len; ++i) {
		struct axa_mem_map *map = &axa->mem_maps[i];
		if (map->start <= addr && map->start + map->length > addr) {
			map->vt->write(map->vt, addr - map->start, data);
			return;
		}
	}
}

void axa_update_flags(struct axa *axa, uint16_t a, uint16_t b, uint16_t res) {
	axa->zflag = (res & 0xff) == 0;
	axa->cflag = (res & 0x100) >> 8;

	axa->oflag =
		((a & 0x40) && (b & 0x40) && (res & 0x80)) ||
		((a & 0x80) && (b & 0x80) && ((res & 0x80) == 0));
}

void axa_exec(struct axa *axa, uint8_t instr) {
	enum axa_op op = (instr & 0xf0) >> 4;
	uint8_t dbit = (instr & 0x08) >> 3;
	uint8_t reg = instr & 0x07;
	uint8_t imm = instr & 0x0f;

	uint16_t a;
	uint16_t b;
	uint16_t res;
	uint8_t *dst;

	if (dbit == 0) {
		a = axa->acc;
		b = axa->regs[reg];
		dst = &axa->regs[reg];
	} else {
		a = axa->regs[reg];
		b = axa->acc;
		dst = &axa->acc;
	}

	uint8_t acc;
	uint16_t boffset;

	switch (op) {
	case AXA_OP_ADD:
		res = a + b;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_SUB:
		b = ~b & 0xff;
		res = a + b + 1;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_XOR:
		res = a ^ b;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_AND:
		res = a & b;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_OR:
		res = a | b;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_MOV:
		*dst = (uint8_t)a;
		axa->iptr += 1;
		break;
	case AXA_OP_SHR:
		res = a >> 1;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->cflag = a & 1;
		axa->iptr += 1;
		break;
	case AXA_OP_CMP:
		b = ~b & 0xff;
		res = a + b + 1;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_ADDC:
		res = a + b + axa->cflag;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_SHRC:
		res = (a >> 1) | (axa->cflag << 7);
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->cflag = a & 1;
		axa->iptr += 1;
		break;
	case AXA_OP_CMPC:
		b = ~b & 0xff;
		res = a + b + axa->cflag;
		*dst = (uint8_t)res;
		axa_update_flags(axa, a, b, res);
		axa->iptr += 1;
		break;
	case AXA_OP_JMP:
		if (dbit) {
			acc = axa->acc | 0xf0;
			boffset = axa->acc | 0xfff0;
		} else {
			acc = axa->acc;
			boffset = acc;
		}

		switch ((enum axa_jmp_op)reg) {
		case AXA_J_JMP:
			axa->iptr = (uint16_t)axa->regs[AXA_REG_CS] << 8 | acc;
			break;
		case AXA_J_CALL:
			axa->regs[AXA_REG_RA] = axa->iptr + 1;
			axa->iptr = (uint16_t)axa->regs[AXA_REG_CS] << 8 | acc;
			break;
		case AXA_J_B:
			axa->iptr += boffset;
			break;
		case AXA_J_BEQ:
			if (axa->zflag) {
				axa->iptr += boffset;
			} else {
				axa->iptr += 1;
			}
			break;
		case AXA_J_BGT:
			if (axa->cflag && !axa->zflag) {
				axa->iptr += boffset;
			} else {
				axa->iptr += 1;
			}
			break;
		case AXA_J_BGE:
			if (axa->cflag) {
				axa->iptr += boffset;
			} else {
				axa->iptr += 1;
			}
			break;
		case AXA_J_BGTS:
			if (axa->oflag && !axa->zflag) {
				axa->iptr += boffset;
			} else {
				axa->iptr += 1;
			}
			break;
		case AXA_J_BGES:
			if (axa->oflag) {
				axa->iptr += boffset;
			} else {
				axa->iptr += 1;
			}
			break;
		}
		break;
	case AXA_OP_LD:
		if (dbit) {
			axa->regs[reg] = axa_load(axa, (uint16_t)axa->regs[AXA_REG_DS] << 8 | axa->acc);
		} else {
			axa->regs[reg] = axa_load(axa, axa->acc);
		}
		break;
	case AXA_OP_ST:
		if (dbit) {
			axa_store(axa, (uint16_t)axa->regs[AXA_REG_DS] << 8 | axa->acc, axa->regs[reg]);
		} else {
			axa_store(axa, axa->acc, axa->regs[reg]);
		}
		axa->iptr += 1;
		break;
	case AXA_OP_IMML:
		axa->acc = imm;
		axa->iptr += 1;
		break;
	case AXA_OP_IMMH:
		axa->acc = imm << 4;
		axa->iptr += 1;
		break;
	}
}

void axa_step(struct axa *axa) {
	axa_exec(axa, axa_load(axa, axa->iptr));
}

void axa_print_state(struct axa *axa) {
	printf("Acc: %02x\n", axa->acc);
	printf("Regs: CS:%02x DS:%02x SP:%02x RV:%02x A0:%02x A1:%02x A2:%02x RA:%02x\n",
			axa->regs[0], axa->regs[1], axa->regs[2], axa->regs[3],
			axa->regs[4], axa->regs[5], axa->regs[6], axa->regs[7]);
	printf("Flags: Z:%d C:%d O:%d\n",
			axa->zflag, axa->cflag, axa->oflag);
}

void axa_print_instr(uint8_t instr) {
	enum axa_op op = (instr & 0xf0) >> 4;
	uint8_t dbit = (instr & 0x08) >> 3;
	uint8_t reg = instr & 0x07;
	uint8_t imm = instr & 0x0f;

	char *opname;
	switch (op) {
	case AXA_OP_ADD:  opname = "add";  break;
	case AXA_OP_SUB:  opname = "sub";  break;
	case AXA_OP_XOR:  opname = "xor";  break;
	case AXA_OP_AND:  opname = "and";  break;
	case AXA_OP_OR:   opname = "or";   break;
	case AXA_OP_MOV:  opname = "mov";  break;
	case AXA_OP_SHR:  opname = "shr";  break;
	case AXA_OP_CMP:  opname = "cmp";  break;
	case AXA_OP_ADDC: opname = "addc"; break;
	case AXA_OP_SHRC: opname = "shrc"; break;
	case AXA_OP_CMPC: opname = "cmpc"; break;
	case AXA_OP_JMP:  opname = NULL;   break;
	case AXA_OP_LD:   opname = "ld";   break;
	case AXA_OP_ST:   opname = "st";   break;
	case AXA_OP_IMML: opname = "imml"; break;
	case AXA_OP_IMMH: opname = "immh"; break;
	}

	char *regname;
	switch (reg) {
	case AXA_REG_CS: regname = "cs"; break;
	case AXA_REG_DS: regname = "ds"; break;
	case AXA_REG_SP: regname = "sp"; break;
	case AXA_REG_RV: regname = "rv"; break;
	case AXA_REG_A1: regname = "a1"; break;
	case AXA_REG_A2: regname = "a2"; break;
	case AXA_REG_A3: regname = "a3"; break;
	case AXA_REG_RA: regname = "ra"; break;
	}

	if (op == AXA_OP_JMP) {
		switch ((enum axa_jmp_op)reg) {
		case AXA_J_JMP:  opname = "jmp";  break;
		case AXA_J_CALL: opname = "call"; break;
		case AXA_J_B:    opname = "b";    break;
		case AXA_J_BEQ:  opname = "beq";  break;
		case AXA_J_BGT:  opname = "bgt";  break;
		case AXA_J_BGE:  opname = "bge";  break;
		case AXA_J_BGTS: opname = "bgts"; break;
		case AXA_J_BGES: opname = "bges"; break;
		}

		if (dbit) {
			printf("b%s", opname);
		} else {
			printf("%s", opname);
		}
	} else if (op == AXA_OP_LD || op == AXA_OP_ST) {
		if (dbit) {
			printf("%sd %s", opname, regname);
		} else {
			printf("%ss %s", opname, regname);
		}
	} else if (op == AXA_OP_IMML) {
		printf("%s %02x", opname, imm);
	} else if (op == AXA_OP_IMMH) {
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
	struct axa_mem_vt vt;
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

	struct axa axa;
	axa_init(&axa);

	size_t ram_size = 1024 * 1024;
	uint8_t *ram_data = malloc(ram_size);
	struct ram_block ram = {
		.vt.read = ram_block_read,
		.vt.write = ram_block_write,
		.data = ram_data,
	};
	axa_add_mem(&axa, &ram.vt, 0, ram_size);

	fread(ram_data, 1, ram_size, binf);
	fclose(binf);

	axa_print_state(&axa);

	while (1) {
		uint8_t instr = axa_load(&axa, axa.iptr);
		printf("== %04x: %02x -- ", axa.iptr, instr);
		axa_print_instr(instr);
		printf("\n");
		getchar();
		axa_exec(&axa, instr);
		axa_print_state(&axa);
	}
}
