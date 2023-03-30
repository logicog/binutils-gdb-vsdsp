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
bool full_ops;

/* Helper for parsing the options.  */

static void
parse_disassembler_options (const char *options)
{
  const char *option;

  FOR_EACH_DISASSEMBLER_OPTION (option, options)
    {
      if (disassembler_options_cmp (option, "full_ops") == 0)
	full_ops = true;
      else
      full_ops = false;
    }
}

// Expects buf to be at least 5 bytes long
static char
*post_mod (uint8_t p, char *buf)
{
  if (p & 8) {
    if (p == 8)
      sprintf (buf, "%s", "*");
    else
      sprintf (buf, "-%d", 16 - p);
  } else if (p != 0) {
      sprintf (buf, "+%d", p);
  } else {
    buf[0] = '\0';
  }

  return buf;
}

static int
disassm_pmove_fullx (uint32_t c, bool follows_op)
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
    {
      if (follows_op)
	fpr(stream, "&; ");
      fpr (stream, "stx %s, (i%d)%s", target_regs[R].name, r, buf);
    }
  else if (R != 0x24 || full_ops)  // parallel load to NOP normally suppressed
    {
      if (follows_op)
	fpr(stream, " &; ");
      fpr (stream, "ldx (i%d)%s, %s", r, buf, target_regs[R].name);
    }
  else return 0;

  return 1;
}

static int
disassm_pmove_fully(uint32_t c, bool follows_op)
{
  char buf[5];
  uint8_t R, p, r;

  c = c & 0x3fff;
  R = c & 0x3f;
  p = (c >> 6) & 0xf;
  r = (c >> 10) & 0x7;

  post_mod(p, buf);
  if (c & (1 << 13))
    {
      if (follows_op)
	fpr(stream, " &; ");
      fpr(stream, "sty %s, (i%d)%s", target_regs[R].name, r, buf);
    }
  else if (R != 0x24 || full_ops)  // parallel load to NOP normally suppressed
    {
      if (follows_op)
	fpr(stream, " &; ");
      fpr(stream, "ldy (i%d)%s, %s", r, buf, target_regs[R].name);
    }
  else return 0;

  return 1;
}

static int
disassm_pmove_long(uint32_t c, bool follows_op)
{
  uint8_t op, s, d, r, R, p;
  char buf[4];

  c = c & 0x1fff;
  op = (c >> 10) & 0xf;
  s = (c >> 6) & 0x3f;
  d = c & 0x3f;

  if (! (op & 0xc))
    {
      if (follows_op)
	fpr(stream, " &; ");
      fpr(stream, "mv %s, %s", target_regs[s].name, target_regs[d].name);
    } else {
      s = (c >> 9) & 1;
      r = (c >> 6) & 0x7;
      switch (op & 0x3)
	{
	case 0: // long-X move
	  R = c & 0x3f;
	  if (s)
	    {
	      fpr(stream, "sty %s, (i%d)", target_regs[R].name, r);
	    }
	  else if (R != 0x24 || full_ops)
	    {
	      if (follows_op)
		fpr(stream, " &; ");
	      fpr(stream, "ldy (i%d), %s", r, target_regs[R].name);
	    }
	  break;
	case 1: // I-bus move
	  p = (c >> 2) & 0xf;
	  R = c & 3;
	  if (follows_op)
	    fpr(stream, " &; ");
	  post_mod(p, buf);
	  if (!s)
	    fpr(stream, "ldi (I%d) %s, %c\n", r, buf, 'A' + R);
	  else
	    fpr(stream, "sti %c, (I%d) %s\n", 'A' + R, r, buf);
	  break;
	default:
	  printf("RESERVED\n");
	}
    }
  return 1;
}

// In the parallel move, perform 2 short moves

static int
disassm_pmove_short(uint32_t c, bool follows_op)
{
  char buf[] = "\0\0";
  uint8_t R, p, r;

//  printf("%s called\n", __func__);
  if (!(c & (1 << 16)))
    {
      printf("%s  RESERVED\n", __func__);
      return -1;
    }

  if (follows_op)
    fpr(stream, " &; ");
  c = c & 0x1ffff;
  R = (c >> 8) & 0x7;
  p = (c >> 11) & 0x1;
  r = (c >> 12) & 0x7;

  buf[0] = p ? '*' : '\0';
  if (c & (1 << 15))
    fpr(stream, "stx %s, (i%d)%s", target_regs[R].name, r, buf);
  else
    fpr(stream, "ldx (i%d)%s, %s", r, buf, target_regs[R].name);

  R = c & 0x7;
  p = (c >> 3) & 0x1;
  r = (c >> 4) & 0x7;

  buf[0] = p ? '*' : '\0';
  if (c & (1 << 15))
    fpr(stream, "sty %s, (i%d)%s", target_regs[R].name, r, buf);
  else if (R != 0x24 || full_ops)  // parallel load to NOP normally suppressed
    fpr(stream, "ldy (i%d)%s, %s", r, buf, target_regs[R].name);

  return 0;
}

static int
(*disassm_pmove[18])(uint32_t, bool) =
{
  disassm_pmove_fullx, disassm_pmove_long, disassm_pmove_fully, disassm_pmove_short,
  disassm_pmove_short, disassm_pmove_short, disassm_pmove_short, disassm_pmove_short
};

static int
decode_pmove (uint32_t c, bool follows_op)
{
  uint8_t op;

  c &= 0x1ffff;
  op = c >> 14;
//  printf("parallel move: %05x op2: %d\n", c, op);
  return disassm_pmove[op](c, follows_op);
}

static int
disassm_ldc (uint32_t c, struct disassemble_info *info)
{
  uint8_t	op1 = c & 0x3f;
  uint16_t constant = (c >> 6) & 0xffff;

  // printf("in %s:\n", __func__);
  if (op1 == 0x24 && !full_ops)
    {
      fpr(stream, "nop");
      return 1;
    }
  fpr(stream, "ldc ");
  (*info->print_address_func) (constant, info);
  fpr(stream, ", %s", target_regs[op1].name);

  return 0;
}

static char conditions[32][3] =
{
  "", "cs", "es", "vs", "ns", "zs", "RV", "RV",
  "lt", "le", "RV", "RV", "RV","RV", "RV", "RV",
  "RV", "cc", "ec", "vc", "nc", "zc", "RV", "RV",
  "ge", "gt", "RV", "RV", "RV", "RV", "RV", "RV"
};

static int
disassm_control (uint32_t c, struct disassemble_info *info)
{
  uint8_t m, r, op = (c >> 24) & 0xf, R, cond;
  uint32_t addr;

  // printf("  in %s, actual op is %x\n", __func__, op);

  switch (op)
    {
    case 0x0: // JRcc
      cond = c & 0x3f;
      if (cond > 0x20)
	{
	  printf ("jr RESERVED");
	  break;
	}
      fpr(stream, "jr%s", conditions[cond]);
      break;

    case 0xd: // HALT
      fpr(stream, "halt");
      break;
    case 0xa: // JMPI
      addr = (c >> 6) & 0xffff;
      r = c & 0x7;
      m = c >> 3 & 0x3;
      fpr(stream, "jmpi ");
      (*info->print_address_func) (addr, info);
      if (m == 1)
	fpr(stream, ", (i%d) + %d", r, m);
      else if (m == 0)
	fpr(stream, ", (i%d)", r);
      else
	fpr(stream, ", (i%d) - %d", r, m);
      break;

    case 0x9: // CALLcc
      cond = c & 0x3f;
      if (cond > 0x20)
	{
	  printf ("CALL RESERVED");
	  break;
	}
      addr = (c >> 6) & 0xffff;
      fpr(stream, "call%s ", conditions[cond]);
      (*info->print_address_func) (addr, info);
      break;

    case 0x8: // Jcc
      cond = c & 0x3f;
      if (cond > 0x20)
	{
	  printf ("J RESERVED");
	  break;
	}
      addr = (c >> 6) & 0xffff;
      fpr(stream, "j%s ", conditions[cond]);
      (*info->print_address_func) (addr, info);
      break;

    case 0x2: // RESP
      r = (c >> 17) & 0x7;
      R = (c >> 20) & 0x7;
      fpr(stream, "resp %s, %s", alu_op[r], alu_op[R]);
      break;

    case 0x1: // RETI
      if (c & (1 << 23))
	{
	  r = (c >> 6) & 0x7;
	  fpr(stream, "reti i%d", r);
	}
      else
	{
	  fpr(stream, "reti");
	}
      break;

    default:
      fpr(stream, "RESERVED");
    }
  return 0;
}

static int
disassm_dmove (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  int count = disassm_pmove_fullx(c >> 14, false);
  count += disassm_pmove_fully(c, count);

  if (!count)
    fpr(stream, "nop");

  return 0;
}

static int
two_op_arithmetic (char *op, uint32_t c)
{
  uint8_t r, R, A;

  R = (c >> 24) & 0xf;
  r = (c >> 20) & 0xf;
  A = (c >> 17) & 0x7;

  // Is this a 40 bit operation, because one op is 40 bits?
  if (r > 9 || R > 9) // Registers P, A, B, C, D are 40 bits
    fpr(stream, "%s %s, %s, %c", op,  alu_op[R], alu_op[r], alu_op[A][0]);
  else
    fpr(stream, "%s %s, %s, %s", op, alu_op[R], alu_op[r], alu_op[A]);

  decode_pmove(c & 0x1ffff, true);

  return 0;
}

static int
disassm_add (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "add";

  return two_op_arithmetic(op, c);
}

static int
disassm_mac (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  printf("in %s with %08x\n", __func__, c);
  // TODO

  return 0;
}

static int
disassm_sub (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "sub";

  return two_op_arithmetic(op, c);
}

static int
disassm_msu (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  printf("in %s with %08x\n", __func__, c);
  // TODO
  return 0;
}

static int
disassm_addc (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "addc";

  return two_op_arithmetic(op, c);
}

static int
disassm_subc (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "subc";

  return two_op_arithmetic(op, c);
}

static int
disassm_ashl (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "ashl";

  return two_op_arithmetic(op, c);
}

static int
disassm_and (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "and";

  return two_op_arithmetic(op, c);
}

static int
disassm_or (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "or";

  return two_op_arithmetic(op, c);
}

static int
disassm_xor (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  char op[] = "xor";

  return two_op_arithmetic(op, c);
}

static int
disassm_reserved (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  printf("in %s with %08x\n", __func__, c);
  abort();
  return 0;
}

static char data_format[4][3] = { "SS", "SU", "US", "UU"};

// These operations all have an opcode 0xfxxxxxxx and use a single operand
static int
disassm_single (uint32_t c, struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  uint8_t op = (c >> 24) & 0xf, r, A, R;

  r = (c >> 20) & 0xf;
  A = (c >> 17) & 0x7;
       
  switch(op)
    {
    case 0x0:
      fpr(stream, "abs %s, %s", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff, true);
      break;
    case 0x1:
      fpr(stream, "asr %s, %s", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff, true);
      break;
    case 0x2: // Logical Shift Right
      fpr(stream, "lsr %s, %s", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff, true);
      break;
    case 0x3: // Logical shift right with Carry
      fpr(stream, "lsrc %s, %s", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff, true);
      break;
    case 0x4: // NOP
      if (!decode_pmove(c & 0x1ffff, false))
	fpr(stream, "nop");
      break;
    case 0x5:
      fpr(stream, "exp %s, %s", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff, true);
      break;
    case 0x6: // Saturate 40 bit to 32 bits, setting overflow flag
      fpr(stream, "sat %s, %s", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff, true);
      break;
    case 0x7: // Round and saturate
      fpr(stream, "rnd %s, %s", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff, true);
      break;
    case 0xe:
    case 0xf:
      r = (c >> 17) & 0x7;
      R = (c >> 20) & 0x7;
      fpr(stream, "mul%s %s, %s", data_format[(c >> 23) & 0x3], alu_op[r], alu_op[R]);
      decode_pmove(c & 0x1ffff, true);
      break;
    default:
      printf("in %s RESERVED\n", __func__);
    }

  return 0;
}

int (*op_decode[16]) (uint32_t, struct disassemble_info *info) =
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

  if (info->disassembler_options)
    {
      parse_disassembler_options (info->disassembler_options);

      /* Avoid repeated parsing of the options.  */
      info->disassembler_options = NULL;
    }

  stream = info->stream;
  fpr = info->fprintf_func;

  if ((status = info->read_memory_func (addr, buffer, 4, info)))
    goto fail;

  iword = bfd_getb32(buffer);
  op_decode[iword >> 28] (iword, info);

  return 4;

 fail:
  info->memory_error_func (status, addr, info);
  return -1;
}
