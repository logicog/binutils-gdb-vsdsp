/* Definitions for decoding the vsdsp opcode table.
   Copyright 2008 Free Software Foundation, Inc.
   Contributed by Anthony Green (green@spindazzle.org).

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA
02110-1301, USA.  */


/*
 * All VS_DSP instructions are 32 bit long. The first 4 bits define the major
 * type of the opcode:
 * 0b0000 LDC (load immediate)
 * 0b0001 LDC (load immediate)
 * 0b0010 control operations (jumps, calls, return statement, halt...)
 * 0b0011 double move instructions for 2 parallel full moves
 * 0b0100 ADD
 * 0b0101 MAC
 * 0b0110 SUB
 * 0b0111 MSU
 * 0b1000 ADDC (add with carry)
 * 0b1001 SUBC (subtract with carry)
 * 0b1010 ASLH (arithmetic shift left)
 * 0b1011 AND
 * 0b1100 OR
 * 0b1101 XOR
 * 0b1110 reserved
 * 0b1111 operations with single operand
 */

typedef enum
{
  VSDSP_OP_LDC = 1,
  VSDSP_OP_CONTROL,
  VSDSP_OP_MOVE,
  VSDSP_OP_ADD,
  VSDSP_OP_MAC,
  VSDSP_OP_SUB,
  VSDSP_OP_MSU,
  VSDSP_OP_ADDC,
  VSDSP_OP_SUBC,
  VSDSP_OP_ASLH,
  VSDSP_OP_AND,
  VSDSP_OP_OR,
  VSDSP_OP_XOR,
  VSDSP_OP_RESERVED,
  VSDSP_OP_SINGLE
} vsdsp_op_major;

struct target_reg_entry
{
  unsigned char code;
  char name[6];
};

#define OP_ALLOWS_PMOVE 0x1
#define OP_IN_PMOVE 0x2
#define OP_DOUBLE_MOVE 0x4

#define N_VSDSP_OPCODES 81

/* A parallel NOP move (ldx (i0), NOP) to fill up the parallel part of an instruction */
#define PARALLEL_MV_NOP 0x24
/* Opcode in bits 28-32 for two double full moves */
#define DOUBLE_FULL_MOVES_OPCODE 0x3

typedef struct vsdsp_opc_info_t
{
  unsigned char opcode;
  vsdsp_op_major itype;
  const char *name;
  unsigned char flags;
} vsdsp_opc_info_t;

struct target_cc_entry
{
  unsigned char code;
  char name[3];
};

extern const struct target_reg_entry target_regs[64];
extern const char alu_op[16][6];
extern const struct target_cc_entry target_cc_codes[14];
