/* Disassemble vsdsp instructions.
   Copyright 2008
   Free Software Foundation, Inc.

   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include <stdio.h>
#include <stdint.h>

#define STATIC_TABLE
#define DEFINE_TABLE

#include "opcode/vsdsp.h"
#include "dis-asm.h"

extern const vsdsp_opc_info_t vsdsp_opc_info[128];

static fprintf_ftype fpr;
static void *stream;

// Expects buf to be at least 5 bytes long
static char
*post_mod (uint8_t p, char *buf)
{
  if (p & 8) {
    if (p == 8)
      sprintf (buf, "%s", "*");
    else
      sprintf (buf, " - %d", 16 - p);
  } else if (p != 0) {
      sprintf (buf, " + %d", p);
  } else {
    buf[0] = '\0';
  }

  return buf;
}

static int
disassm_pmove_fullx (uint32_t c)
{
  char buf[5];
  uint8_t R, p, r;

  c = c & 0x3fff;
  R = c & 0x3f;
  p = (c >> 6) & 0xf;
  r = (c >> 10) & 0x7;

  // printf("  in %s, c %04x, R %2x, p %x, r %x\n", __func__, c, R, p, r);
  post_mod (p, buf);
  if (c & (1 << 13))
    fpr (stream, "stx %s, (i%d)%s", target_regs[R].name, r, buf);
  else
    fpr (stream, "ldx (i%d)%s, %s", r, buf, target_regs[R].name);

  return 0;
}

static int
disassm_pmove_fully(uint32_t c)
{
	char buf[5];
	uint8_t R, p, r;

	c = c & 0x3fff;
	R = c & 0x3f;
	p = (c >> 6) & 0xf;
	r = (c >> 10) & 0x7;

	// printf("  in %s, c %04x, R %2x, p %x, r %x\n", __func__, c, R, p, r);
	post_mod(p, buf);
	if (c & (1 << 13))
		fpr(stream, "sty\t%s, (i%d)%s", target_regs[R].name, r, buf);
	else
		fpr(stream, "ldy\t(i%d)%s, %s", r, buf, target_regs[R].name);

	return 0;
}

static int
disassm_pmove_long(uint32_t c)
{
	uint8_t op, s, d, r, R, p;
	char buf[4];

	c = c & 0x1fff;
	op = (c >> 10) & 0xf;
	s = (c >> 6) & 0x3f;
	d = c & 0x3f;
	// printf("  in %s op %x ", __func__, op);

	if (! (op & 0xc)) {
		fpr(stream, "mvy\t%s, %s\n", target_regs[s].name, target_regs[d].name);
	} else {
		switch (op & 0x3) {
		case 0: // long-X move
			printf("long-X move\n");
			break;
		case 1: // I-bus move
			s = (c >> 9) & 1;
			r = (c >> 6) & 0x7;
			p = (c >> 2) & 0xf;
			R = c & 3;
//			printf("I bus move load/store %d r: %d, p: %d, R: %d\n", s, r, p, R);
			post_mod(p, buf);
			if (!s)
				fpr(stream, "LDI (I%d) %s, %c\n", r, buf, 'A' + R);
			else
				fpr(stream, "STI %c, (I%d) %s\n", 'A' + R, r, buf);
			break;
		default:
			printf("RESERVED\n");
		}
	}
	return 0;
}

// In the parallel move, perform 2 short moves
static int
disassm_pmove_short(uint32_t c)
{
	char buf[] = "\0\0";
	uint8_t R, p, r;

//	printf("  in %s, c %08x\n", __func__, c);
	if (!(c & (1 << 16))) {
		printf("%s  RESERVED\n", __func__);
		return -1;
	}
	c = c & 0x1ffff;
	R = (c >> 8) & 0x7;
	p = (c >> 11) & 0x1;
	r = (c >> 12) & 0x7;

// 	printf("  in %s, c %04x, R %2x, p %x, r %x\n", __func__, c, R, p, r);
	buf[0] = p ? '*' : '\0';
	if (c & (1 << 15))
		fpr(stream, "STX %s, (I%d)%s", target_regs[R].name, r, buf);
	else
		fpr(stream, "LDX (I%d)%s, %s", r, buf, target_regs[R].name);

	R = c & 0x7;
	p = (c >> 3) & 0x1;
	r = (c >> 4) & 0x7;

//	printf("  in %s, c %04x, R %2x, p %x, r %x\n", __func__, c, R, p, r);
	buf[0] = p ? '*' : '\0';
	if (c & (1 << 15))
		fpr(stream, "STY %s, (I%d)%s", target_regs[R].name, r, buf);
	else
		fpr(stream, "LDY (I%d)%s, %s", r, buf, target_regs[R].name);

	return 0;
}

static int
(*disassm_pmove[18])(uint32_t) =
{
	disassm_pmove_fullx, disassm_pmove_long, disassm_pmove_fully, disassm_pmove_short,
	disassm_pmove_short, disassm_pmove_short, disassm_pmove_short, disassm_pmove_short
};

static void
decode_pmove(uint32_t c)
{
	uint8_t op;

	c &= 0x1ffff;
	op = c >> 14;
//	printf("parallel move: %05x op2: %d\n", c, op);
	disassm_pmove[op](c);
	printf("\n");
}

static int
disassm_ldc(uint32_t c)
{
	uint8_t	op1 = c & 0x3f;
	uint16_t constant = (c >> 6) & 0xffff;

	// printf("in %s:\n", __func__);
	fpr(stream, "LDC %x, %s\n", constant, target_regs[op1].name);

	return 0;
}

static char conditions[32][3] =
{
	"", "CS", "ES", "VS", "NS", "ZS", "RV", "RV",
	"LT", "LE", "RV", "RV", "RV","RV", "RV", "RV",
	"RV", "CC", "EC", "VC", "NC", "ZC", "RV", "RV",
	"GE", "GT", "RV", "RV", "RV","RV", "RV", "RV"
};

static int
disassm_control(uint32_t c)
{
	uint8_t m, r, op = (c >> 24) & 0xf, R, cond;
	uint32_t addr;

	// printf("  in %s, actual op is %x\n", __func__, op);

	switch (op) {
	case 0xd: // HALT
		fpr(stream, "HALT\n");
		break;
	case 0xa: // JMPI
		addr = (c >> 6) & 0xffff;
		r = c & 0x7;
		m = c >> 3 & 0x3;
		if (m == 1)
			fpr(stream, "JMPI %04x, (I%d) + %d\n", addr, r, m);
		else if (m == 0)
			fpr(stream, "JMPI %04x, (I%d)\n", addr, r);
		else
			fpr(stream, "JMPI %04x, (I%d) - %d\n", addr, r, m);
		break;

	case 0x9: // CALLcc
		cond = c & 0x3f;
		if (cond > 0x20) {
			printf ("  CALL RESERVED\n");
			break;
		}
		addr = (c >> 6) & 0xffff;
		fpr(stream, "CALL%s %x\n", conditions[cond], addr);
		break;

	case 0x8: // Jcc
		cond = c & 0x3f;
		if (cond > 0x20) {
			printf ("  J RESERVED\n");
			break;
		}
		addr = (c >> 6) & 0xffff;
		fpr(stream, "J%s %x\n", conditions[cond], addr);
		break;

	case 0x2: // RESP
		r = (c >> 17) & 0x7;
		R = (c >> 20) & 0x7;
		fpr(stream, "RESP %s, %s\n", alu_op[r], alu_op[R]);
		break;

	case 0x1: // RETI
		if (c & (1 << 23)) {
			r = (c >> 6) & 0x7;
			fpr(stream, "RETI I%d\n", r);
		} else {
			fpr(stream, "RETI\n");
		}
		break;
	default:
		fpr(stream, "RESERVED\n");
	}
	return 0;
}

static int
disassm_dmove(uint32_t c)
{
//	printf("in %s\n", __func__);
	disassm_pmove_fullx(c >> 14);
	printf("; ");
	disassm_pmove_fully(c);
	printf("\n");

	return 0;
}

static int
two_op_arithmetic(char *op, uint32_t c)
{
	uint8_t r, R, A;
// 	printf("in %s\n", __func__);

	r = (c >> 20) & 0xf;
	R = (c >> 24) & 0xf;
	A = (c >> 17) & 0x7;
//	printf("%s r %x, R %x, A %x\n", __func__, r, R, A);
	
	// Is this a 40 bit operation, because one op is 40 bits?
	if (r > 9 || R > 9) // Registers P, A, B, C, D are 40 bits
		fpr(stream, "%s %s, %s, %c;", op,  alu_op[R], alu_op[r], alu_op[A][0]);
	else
		fpr(stream, "%s %s, %s, %s;", op, alu_op[R], alu_op[r], alu_op[A]);

	decode_pmove(c & 0x1ffff);

	return 0;
}

static int
disassm_add(uint32_t c)
{
	char op[] = "ADD";

	return two_op_arithmetic(op, c);
}

static int
disassm_mac(uint32_t c)
{
	printf("in %s with %08x\n", __func__, c);

	return 0;
}

static int
disassm_sub(uint32_t c)
{
	char op[] = "SUB";

	return two_op_arithmetic(op, c);
}

static int
disassm_msu(uint32_t c)
{
	printf("in %s with %08x\n", __func__, c);

	return 0;
}

static int
disassm_addc(uint32_t c)
{
	char op[] = "ADDC";

	return two_op_arithmetic(op, c);
}

static int
disassm_subc(uint32_t c)
{
	char op[] = "SUBC";

	return two_op_arithmetic(op, c);
}

static int
disassm_ashl(uint32_t c)
{
	char op[] = "ASHL";

	return two_op_arithmetic(op, c);
}

static int
disassm_and(uint32_t c)
{
	char op[] = "AND";

	return two_op_arithmetic(op, c);
}

static int
disassm_or(uint32_t c)
{
	char op[] = "OR";

	return two_op_arithmetic(op, c);
}

static int
disassm_xor(uint32_t c)
{
	char op[] = "XOR";

	return two_op_arithmetic(op, c);
}

static int
disassm_reserved(uint32_t c)
{
	printf("in %s with %08x\n", __func__, c);

	return 0;
}

static char data_format[4][3] = { "SS", "SU", "US", "UU"};

// These operations all have an opcode 0xfxxxxxxx and use a single operand
static int
disassm_single(uint32_t c)
{
  uint8_t op = (c >> 24) & 0xf, r, A, R;

  printf("%s c: %04x, op %x\n", __func__, c, op);
  r = (c >> 20) & 0xf;
  A = (c >> 17) & 0x7;
       
  switch(op)
    {
    case 0x0:
      fpr(stream, "ABS %s, %s;", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x1:
      fpr(stream, "ASR %s, %s;", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x2: // Logical Shift Right
      fpr(stream, "LSR %s, %s;", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x3: // Logical shift right with Carry
      fpr(stream, "LSRC %s, %s;", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x4:
      printf("in %s NOP ", __func__);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x5:
      fpr(stream, "EXP %s, %s;", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x6: // Saturate 40 bit to 32 bits, setting overflow flag
      fpr(stream, "SAT %s, %s;", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x7: // Round and saturate
      fpr(stream, "RND %s, %s;", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0xe:
    case 0xf:
      r = (c >> 17) & 0x7;
      R = (c >> 20) & 0x7;
      fpr(stream, "MUL%s %s, %s;", data_format[(c >> 23) & 0x3], alu_op[r], alu_op[R]);
      decode_pmove(c & 0x1ffff);
      break;
    default:
      printf("in %s RESERVED\n", __func__);
    }

  return 0;
}

int (*op_decode[16])(uint32_t) =
{
  disassm_ldc, disassm_ldc, disassm_control, disassm_dmove,
  disassm_add, disassm_mac, disassm_sub, disassm_msu,
  disassm_addc, disassm_subc, disassm_ashl, disassm_and,
  disassm_or, disassm_xor, disassm_reserved, disassm_single
};

int
print_insn_vsdsp (bfd_vma addr, struct disassemble_info *info)
{
  int status;
  bfd_byte buffer[4];
  uint32_t iword;
//  const vsdsp_opc_info_t *opcode;
  
  printf("%s called\n", __func__);
  stream = info->stream;
  fpr = info->fprintf_func;

  printf("A1\n");
  if ((status = info->read_memory_func (addr, buffer, 4, info)))
    goto fail;

  iword = bfd_getb32(buffer);

  printf("A1 got %08x\n", iword);

  op_decode[iword >> 28] (iword);

  printf("A2\n");
//  fpr (stream, "%s", vsdsp_opc_info[opcode].name);

  return 4;

 fail:
  info->memory_error_func (status, addr, info);
  return -1;
}
