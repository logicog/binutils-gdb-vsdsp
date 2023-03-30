/* vsdsp-opc.c -- Definitions for vsdsp opcodes.
   Copyright 2008 Free Software Foundation, Inc.
   Contributed by Anthony Green (green@spindazzle.org).

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
   along with this file; see the file COPYING.  If not, write to the
   Free Software Foundation, 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "opcode/vsdsp.h"

const vsdsp_opc_info_t vsdsp_opc_info[12] =
{
  { 0x00, VSDSP_OP_LDC, "ldc" },
  { 0x01, VSDSP_OP_LDC, "ldc" },
  { 0x02, VSDSP_OP_ADD, "add" },
  { 0x03, VSDSP_OP_MAC, "mac" },
  { 0x04, VSDSP_OP_SUB, "sub" },
  { 0x05, VSDSP_OP_MSU, "msu" },
  { 0x06, VSDSP_OP_ADDC, "addc" },
  { 0x07, VSDSP_OP_SUBC, "subc" },
  { 0x08, VSDSP_OP_ASLH, "aslh" },
  { 0x09, VSDSP_OP_AND, "and" },
  { 0x0a, VSDSP_OP_OR, "or" },
  { 0x0b, VSDSP_OP_XOR, "xor" }
};


/* List of target registers for moves, load and store
 * operations: mvx/mvy, ldx/ldy, stx/sty, ldc
 */
const struct target_reg_entry target_regs[64] =
{
  { 0x00, "a0" },
  { 0x01, "a1" },
  { 0x02, "b0" },
  { 0x03, "b1" },
  { 0x04, "c0" },
  { 0x05, "c1" },
  { 0x06, "d0" },
  { 0x07, "d1" },
  
  { 0x08, "lr0" },
  { 0x09, "lr1" },
  { 0x0a, "mro" },
  { 0x0b, "rsrvd" },
  { 0x0c, "null" },
  { 0x0d, "lc" }, 
  { 0x0e, "ls" },
  { 0x0f, "le" },

  { 0x10, "i0" },
  { 0x11, "i1" },
  { 0x12, "i2" },
  { 0x13, "i3" },
  { 0x14, "i4" },
  { 0x15, "i5" },
  { 0x16, "i6" },
  { 0x17, "i7" },

  { 0x18, "rsrvd" },
  { 0x19, "rsrvd" },
  { 0x1a, "rsrvd" },
  { 0x1b, "rsrvd" },
  { 0x1c, "rsrvd" },
  { 0x1d, "rsrvd" },
  { 0x1e, "rsrvd" },
  { 0x1f, "rsrvd" },

  { 0x20, "a2" },
  { 0x21, "b2" },
  { 0x22, "c2" },
  { 0x23, "d2" },
  { 0x24, "nop" },
  { 0x25, "rsrvd" },
  { 0x26, "rsrvd" },
  { 0x27, "rsrvd" },

  { 0x28, "rsrvd" },
  { 0x29, "rsrvd" },
  { 0x2a, "rsrvd" },
  { 0x2b, "rsrvd" },
  { 0x2c, "rsrvd" },
  { 0x2d, "rsrvd" },
  { 0x2e, "rsrvd" },
  { 0x2f, "rsrvd" },

  { 0x30, "rsrvd" },
  { 0x31, "rsrvd" },
  { 0x32, "rsrvd" },
  { 0x33, "rsrvd" },
  { 0x34, "rsrvd" },
  { 0x35, "rsrvd" },
  { 0x36, "rsrvd" },
  { 0x37, "rsrvd" },

  { 0x38, "rsrvd" },
  { 0x39, "rsrvd" },
  { 0x3a, "rsrvd" },
  { 0x3b, "rsrvd" },
  { 0x3c, "rsrvd" },
  { 0x3d, "rsrvd" },
  { 0x3e, "ipr0" },
  { 0x3f, "ipr1" }
};


const char alu_op[16][6] =
{
  "a0", "a1", "b0", "b1", "c0", "c1", "d0", "d1",
  "null", "ones", "rsrvd", "p", "a", "b", "c", "d"
};
