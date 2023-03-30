/* Simulator for the vsdsp processor
   Copyright (C) 2008 Free Software Foundation, Inc.
   Contributed by Anthony Green

This file is part of GDB, the GNU debugger.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "defs.h"

#include <signal.h>
#include <stdlib.h>
#include <sys/times.h>
#include <sys/param.h>
#include <netinet/in.h>	/* for byte ordering macros */
#include "bfd.h"
#include "libiberty.h"

#include "sim-main.h"
#include "sim-base.h"
#include "sim-options.h"
#include "sim-io.h"
#include "sim-signal.h"
#include "target-newlib-syscall.h"

static unsigned long  heap_ptr = 0;
host_callback *       callback;

/* The machine state.

   This state is maintained in host byte order.  The fetch/store
   register functions must translate between host byte order and the
   target processor byte order.  Keeping this data in target byte
   order simplifies the register read/write functions.  Keeping this
   data in native order improves the performance of the simulator.
   Simulation speed is deemed more important.  */

#define NUM_VSDSP_AREGS 4
#define NUM_VSDSP_IREGS 8
static const char *reg_names[32] = 
  { "a0", "a1", "a2", "b0", "b1", "b2", "c0", "c1", 
    "c2", "d0", "d1", "d2", "a", "b", "c", "d",
    "i0", "i1", "i2", "i3", "i4", "i5", "i6", "i7",
    "p0", "p1", "lc", "le", "ls", "ipr0", "ipr1", "mr0"
  };

#ifdef HOST_BIG_ENDIAN
typedef struct __attribute__((__packed__)) alu_reg
{
  uint16_t a3;
  uint16_t a2;
  uint16_t a1;
  uint16_t a0;
} alu_reg_t;
#else
struct __attribute__((__packed__)) alu_reg
{
  uint16_t a0;
  uint16_t a1;
  uint16_t a2;
  uint16_t a3;
};
#endif

typedef union
{
  struct alu_reg reg;
  uint64_t asint;
} alu_reg_t;

#define VSDP_CARRY_FLAG		0x01
#define VSDP_EXTENSION_FLAG	0x02
#define VSDP_OVERFLOW_FLAG	0x04
#define VSDP_NEGATIVE_FLAG	0x08
#define VSDP_ZERO_FLAG		0x10
#define VSDP_LOOP_FLAG		0x80
#define VSDP_ROUNDING_MODE	0x100
#define VSDP_INTEGER_MODE	0x200
#define VSDP_SATURATION_MODE	0x400


/* The ordering of the vsdsp_cpu structure is matched in the
   gdb/config/vsdsp/tm-vsdsp.h file in the REGISTER_NAMES macro.  */
struct vsdsp_cpu
{
  alu_reg_t	aregs[NUM_VSDSP_AREGS];		// ALU registers a0, a1, a2 .. d0, d1, d2
  uint16_t	iregs[NUM_VSDSP_IREGS];		// Address Registers i0, ..., i7
  uint64_t	p;				// 40 bit Multiplier pipeline register P
  uint16_t	pc;				// program counter
  uint16_t	lc, le, ls, lr0, lr1;		// Loop registers
  uint16_t	ipr0, ipr1;			// Page registers
  uint16_t	mr0[3];				// Mode register in 3 pipeline stages
  uint32_t	imsize;				// Instruction memory sizes in 32 bit words
  uint32_t	xmsize, ymsize;			// X/Y Memory sizes in 16 bit words
  uint32_t	*imemory;
  uint16_t	*xmemory, *ymemory;
  uint32_t	exception;
  unsigned long   insts;               		// instruction counter
};

static struct vsdsp_cpu vsdsp_cpu;

static void
update_target_reg(int r, uint16_t v)
{
  if (r < 0x08)
    {
      if (r & 1) {
	vsdsp_cpu.aregs[r/2].reg.a1 = v;
	// Sign-extend to a2
	vsdsp_cpu.aregs[r/2].reg.a2 = v & 0x8000 ? 0xff : 0x00;
      } else {
	vsdsp_cpu.aregs[r/2].reg.a0 = v;
      }
    } 
  else if (r == 0x08)
    vsdsp_cpu.lr0 = v;
  else if (r == 0x09)
    vsdsp_cpu.lr1 = v;
  else if (r == 0x0a)
    vsdsp_cpu.mr0[2] = v;
  else if (r == 0x0d)
    vsdsp_cpu.lc = v;
  else if (r == 0x0e)
    vsdsp_cpu.ls = v;
  else if (r == 0x0f)
    vsdsp_cpu.le = v;
  else if (r < 0x18 && r > 0x0f)
    vsdsp_cpu.iregs[r - 0x10] = v;
  else if (r == 0x20)
    vsdsp_cpu.aregs[0].reg.a2 = v & 0xff;
  else if (r == 0x21)
    vsdsp_cpu.aregs[1].reg.a2 = v & 0xff;
  else if (r == 0x22)
    vsdsp_cpu.aregs[2].reg.a2 = v & 0xff;
  else if (r == 0x23)
    vsdsp_cpu.aregs[3].reg.a2 = v & 0xff;
  else if (r == 0x24)
    return; // NOP
  else if (r == 0x3e)
    vsdsp_cpu.ipr0 = v;
  else if (r == 0x3f)
    vsdsp_cpu.ipr1 = v;
  else
    vsdsp_cpu.exception = SIGILL;
}

/* Used for updating the target operand of an ALU operation
 * Note that P is only available as operand2, and can only be
 * fetched. It is however the implicit target of e.g. the MAC
 * multiply-accumulate operation
 */
static void
update_alu_reg(int r, uint64_t v)
{
  if (r < 0x08)
    {
      if (r & 1)
	vsdsp_cpu.aregs[r/2].reg.a1 = v;
      else
	vsdsp_cpu.aregs[r/2].reg.a0 = v;
    }
  else if (r == 0x0c)
    vsdsp_cpu.aregs[0].asint = v & 0xffffffffff;
  else if (r == 0x0d)
    vsdsp_cpu.aregs[1].asint = v & 0xffffffffff;
  else if (r == 0x0e)
    vsdsp_cpu.aregs[2].asint = v & 0xffffffffff;
  else if (r == 0x0f)
    vsdsp_cpu.aregs[3].asint = v & 0xffffffffff;
  else if (r == 0x08 || r == 0x09)
    return; // NULL, ONES
  else
    vsdsp_cpu.exception = SIGILL;
}

static uint16_t
fetch_target_reg(int r)
{
  uint16_t v = 0;

  if (r < 0x08)
    {
      if (r & 1)
	v = vsdsp_cpu.aregs[r/2].reg.a1;
      else
	v = vsdsp_cpu.aregs[r/2].reg.a0;
    } 
  else if (r == 0x08)
    v = vsdsp_cpu.lr0;
  else if (r == 0x09)
    v = vsdsp_cpu.lr1;
  else if (r == 0x0a)
    v = vsdsp_cpu.mr0[0];
  else if (r == 0x0d)
    v = vsdsp_cpu.lc;
  else if (r == 0x0e)
    v = vsdsp_cpu.ls;
  else if (r == 0x0f)
    v = vsdsp_cpu.le;
  else if (r < 0x18 && r > 0x0f)
    v = vsdsp_cpu.iregs[r - 0x10];
  else if (r == 0x20)
    v = vsdsp_cpu.aregs[0].reg.a2 & 0xff;
  else if (r == 0x21)
    v = vsdsp_cpu.aregs[1].reg.a2 & 0xff;
  else if (r == 0x22)
    v = vsdsp_cpu.aregs[2].reg.a2 & 0xff;
  else if (r == 0x23)
    v = vsdsp_cpu.aregs[3].reg.a2 & 0xff;
  else if (r == 0x24)
    return 0; // NOP FIXME: the operation should do nothing
  else if (r == 0x3e)
    v = vsdsp_cpu.ipr0;
  else if (r == 0x3f)
    v = vsdsp_cpu.ipr1;
  else
    vsdsp_cpu.exception = SIGILL;

  return v;
}

static uint64_t
fetch_alu_reg(int r)
{
  uint64_t v;

  if (r < 0x08)
    {
      if (r & 1)
	v = vsdsp_cpu.aregs[r/2].reg.a1;
      else
	v = vsdsp_cpu.aregs[r/2].reg.a0;
    }
  else if (r == 0x0b)
    v = vsdsp_cpu.p & 0xffffffffff;  // P is only 40 bits long
  else if (r == 0x0c)
    v = vsdsp_cpu.aregs[0].asint & 0xffffffffff;
  else if (r == 0x0d)
    v = vsdsp_cpu.aregs[1].asint & 0xffffffffff;
  else if (r == 0x0e)
    v = vsdsp_cpu.aregs[2].asint & 0xffffffffff;
  else if (r == 0x0f)
    v = vsdsp_cpu.aregs[3].asint & 0xffffffffff;
  else if (r == 0x08)
    v = 0;		// NULL
  else if (r == 0x09)
    v = 0xffffffffff;	// ONES // TODO: Check length???
  else
    v = vsdsp_cpu.exception = SIGILL;

  return v;
}

static char *myname;
static SIM_OPEN_KIND sim_kind;
static int issue_messages = 0;
static struct vsdsp_cpu vsdsp_cpu;

/* Default to a 1 Mbyte (== 2^20 bytes = 2^18 32bit words) instruction memory space  */
#define SIM_IMEM_SIZE 18

/* Default of 1MB X and Y Data space */
#define SIM_XMEM_SIZE 19
#define SIM_YMEM_SIZE 19

#define	MEM_FLOOR	64

static void
sim_size_i_mem (int power)
{
  vsdsp_cpu.imsize = 1 << (power + 2);

  if (vsdsp_cpu.imemory)
    free (vsdsp_cpu.imemory);

  /* Watch out for the '0 count' problem. There's probably a better
     way.. e.g., why do we use 64 here?  */
  if (vsdsp_cpu.imsize < MEM_FLOOR)	/* Ensure a boundary.  */
    vsdsp_cpu.imemory = (uint32_t *) calloc (MEM_FLOOR, (MEM_FLOOR + vsdsp_cpu.imsize) / MEM_FLOOR);
  else
    vsdsp_cpu.imemory = (uint32_t *) calloc (MEM_FLOOR, vsdsp_cpu.imsize / MEM_FLOOR);

  if (!vsdsp_cpu.imemory)
    {
      if (issue_messages)
	fprintf (stderr,
		 "Not enough VM for simulation of %d bytes of I RAM\n",
		 vsdsp_cpu.imsize);

      vsdsp_cpu.imsize = 1;
      vsdsp_cpu.imemory = (uint32_t *) calloc (1, 1);
    }
}

static void
sim_size_x_mem (int power)
{
  vsdsp_cpu.xmsize = 1 << (power + 1);

  if (vsdsp_cpu.xmemory)
    free (vsdsp_cpu.xmemory);

  /* Watch out for the '0 count' problem. There's probably a better
     way.. e.g., why do we use 64 here?  */
  if (vsdsp_cpu.xmsize < MEM_FLOOR)	/* Ensure a boundary.  */
    vsdsp_cpu.xmemory = (uint16_t *) calloc (MEM_FLOOR, (MEM_FLOOR + vsdsp_cpu.xmsize) / MEM_FLOOR);
  else
    vsdsp_cpu.xmemory = (uint16_t *) calloc (MEM_FLOOR, vsdsp_cpu.xmsize / MEM_FLOOR);

  if (!vsdsp_cpu.xmemory)
    {
      if (issue_messages)
	fprintf (stderr,
		 "Not enough VM for simulation of %d bytes of X RAM\n",
		 vsdsp_cpu.xmsize);

      vsdsp_cpu.xmsize = 1;
      vsdsp_cpu.xmemory = (uint16_t *) calloc (1, 1);
    }
}

static void
sim_size_y_mem (int power)
{
  vsdsp_cpu.ymsize = 1 << (power + 1);

  if (vsdsp_cpu.ymemory)
    free (vsdsp_cpu.ymemory);

  /* Watch out for the '0 count' problem. There's probably a better
     way.. e.g., why do we use 64 here?  */
  if (vsdsp_cpu.ymsize < MEM_FLOOR)	/* Ensure a boundary.  */
    vsdsp_cpu.ymemory = (uint16_t *) calloc (MEM_FLOOR, (MEM_FLOOR + vsdsp_cpu.ymsize) / MEM_FLOOR);
  else
    vsdsp_cpu.ymemory = (uint16_t *) calloc (MEM_FLOOR, vsdsp_cpu.ymsize / MEM_FLOOR);

  if (!vsdsp_cpu.ymemory)
    {
      if (issue_messages)
	fprintf (stderr,
		 "Not enough VM for simulation of %d bytes of Y RAM\n",
		 vsdsp_cpu.ymsize);

      vsdsp_cpu.ymsize = 1;
      vsdsp_cpu.ymemory = (uint16_t *) calloc (1, 1);
    }
}

static void
init_pointers ()
{
  if (vsdsp_cpu.imsize != (1 << SIM_IMEM_SIZE))
    sim_size_i_mem (SIM_IMEM_SIZE);
  if (vsdsp_cpu.xmsize != (1 << SIM_XMEM_SIZE))
    sim_size_x_mem (SIM_XMEM_SIZE);
  if (vsdsp_cpu.ymsize != (1 << SIM_YMEM_SIZE))
    sim_size_y_mem (SIM_YMEM_SIZE);
}


static void
set_initial_gprs ()
{
  int i;
  long space;
  unsigned long memsize;
  
  init_pointers ();

  /* Set up machine just out of reset.  */
  vsdsp_cpu.pc = 0;
  
  memsize = vsdsp_cpu.imsize / (1024 * 1024);
  if (issue_messages > 1)
    fprintf (stderr, "Simulated memory of %ld Mbytes I RAM (0x0 .. 0x%08x)\n",
	     memsize, vsdsp_cpu.imsize - 1);

  memsize = vsdsp_cpu.xmsize / (1024 * 1024);
  if (issue_messages > 1)
    fprintf (stderr, "Simulated memory of %ld Mbytes X RAM (0x0 .. 0x%08x)\n",
	     memsize, vsdsp_cpu.xmsize - 1);

  memsize = vsdsp_cpu.ymsize / (1024 * 1024);
  if (issue_messages > 1)
    fprintf (stderr, "Simulated memory of %ld Mbytes Y RAM (0x0 .. 0x%08x)\n",
	     memsize, vsdsp_cpu.ymsize - 1);

  /* Clean out the register contents.  */
  for (i = 0; i < NUM_VSDSP_AREGS; i++)
    vsdsp_cpu.aregs[i].asint = 0;

  for (i = 0; i < NUM_VSDSP_IREGS; i++)
    vsdsp_cpu.iregs[i] = 0;
}

static void
interrupt ()
{
  vsdsp_cpu.exception = SIGINT;
}

static int tracing = 0;

static void
sim_post_mod(int mode, int reg)
{
  uint16_t v;

  if (mode == 0x8)
    return;			// FIXME: modify according to \bar{i}

  v = vsdsp_cpu.iregs[reg];
   if (mode & 8)
     v -= 16 - mode;
   else
     v += mode;
   vsdsp_cpu.iregs[reg] = v;
}


static int
sim_pmove_fullx (uint32_t c)
{
  uint16_t v;
  uint8_t R, p, r;

  c = c & 0x3fff;
  R = c & 0x3f;
  p = (c >> 6) & 0xf;
  r = (c >> 10) & 0x7;

  // TODO: Check address against xmsize. CPU-behaviour???

  if (c & (1 << 13))	// stx
    vsdsp_cpu.xmemory[vsdsp_cpu.iregs[r]] = fetch_target_reg(R);
  else			// ldx
    update_target_reg(R, vsdsp_cpu.xmemory[vsdsp_cpu.iregs[r]]);

  sim_post_mod(p, r);
  return 0;
}

static int
sim_pmove_fully(uint32_t c)
{
  char buf[5];
  uint8_t R, p, r;

  c = c & 0x3fff;
  R = c & 0x3f;
  p = (c >> 6) & 0xf;
  r = (c >> 10) & 0x7;

  if (c & (1 << 13))	// sty
    vsdsp_cpu.ymemory[vsdsp_cpu.iregs[r]] = fetch_target_reg(R);
  else			// ldy
    update_target_reg(R, vsdsp_cpu.ymemory[vsdsp_cpu.iregs[r]]);

  sim_post_mod(p, r);
  return 0;
}

static int
sim_pmove_long(uint32_t c)
{
  uint8_t op, s, d, r, R, p;
  char buf[4];

  c = c & 0x1fff;
  op = (c >> 10) & 0xf;
  s = (c >> 6) & 0x3f;
  d = c & 0x3f;
// printf("  in %s op %x ", __func__, op);

  if (! (op & 0xc))
    {
      update_target_reg(d, fetch_target_reg(s));
    } else {
      switch (op & 0x3)
	{
	case 0: // long-X move
	  printf("NOT IMPLEMENTED long-X move\n");
	  return -1;
	case 1: // I-bus move
	  s = (c >> 9) & 1;
	  r = (c >> 6) & 0x7;
	  p = (c >> 2) & 0xf;
	  R = c & 3;
//		printf("I bus move load/store %d r: %d, p: %d, R: %d\n", s, r, p, R);
	  if (!s) // LDI
	    vsdsp_cpu.aregs[R].asint = vsdsp_cpu.imemory[vsdsp_cpu.iregs[r]];
	  else  // STI
	    vsdsp_cpu.imemory[vsdsp_cpu.iregs[r]] = vsdsp_cpu.aregs[R].asint;
	  sim_post_mod(p, r);
	  break;
	default:
	  printf("%s RESERVED\n", __func__);
	  return -1;
	}
    }
  return 0;
}

// In the parallel move, perform 2 short moves
static int
sim_pmove_short(uint32_t c)
{
  uint8_t R, p, r;

  if (!(c & (1 << 16)))
    {
      printf("%s  RESERVED\n", __func__);
      return -1;
    }

  c = c & 0x1ffff;
  R = (c >> 8) & 0x7;
  p = (c >> 11) & 0x1;
  r = (c >> 12) & 0x7;

  if (c & (1 << 15))	// stx
    vsdsp_cpu.xmemory[vsdsp_cpu.iregs[r]] = fetch_target_reg(R);
  else			// ldx
    update_target_reg(R, vsdsp_cpu.xmemory[vsdsp_cpu.iregs[r]]);

  R = c & 0x7;
  p = (c >> 3) & 0x1;
  r = (c >> 4) & 0x7;

  if (c & (1 << 15))	// sty
    vsdsp_cpu.ymemory[vsdsp_cpu.iregs[r]] = fetch_target_reg(R);
  else			// ldy
    update_target_reg(R, vsdsp_cpu.ymemory[vsdsp_cpu.iregs[r]]);

  return 0;
}

static int
(*sim_pmove[18])(uint32_t) =
{
	sim_pmove_fullx, sim_pmove_long, sim_pmove_fully, sim_pmove_short,
	sim_pmove_short, sim_pmove_short, sim_pmove_short, sim_pmove_short
};

static int
decode_pmove(uint32_t c)
{
  uint8_t op;

  c &= 0x1ffff;
  op = c >> 14;
//	printf("parallel move: %05x op2: %d\n", c, op);
  sim_pmove[op](c);
  printf("\n");
  return 0;
  
}

static int
sim_ldc(uint32_t c)
{
  uint8_t	op1 = c & 0x3f;
  uint16_t constant = (c >> 6) & 0xffff;

  update_target_reg(op1, constant);
  return 0;
}

static bool
sim_test_condition(int cond)
{
  if (cond < 10 && cond > 0)
    return vsdsp_cpu.mr0[0] & (1 << (cond - 1));
  else if (cond > 16 && cond < 26)
    return !(vsdsp_cpu.mr0[0] & (1 << (cond - 17)));

  vsdsp_cpu.exception = SIGILL;
  return false;
}

static int
sim_control(uint32_t c)
{
  uint8_t m, r, op = (c >> 24) & 0xf, R, cond;
  uint32_t addr;

  // printf("  in %s, actual op is %x\n", __func__, op);

  switch (op) 
    {
    case 0xd: // HALT
      printf("%s: HALT not IMPLEMENTED\n", __func__);
      break;

    case 0xa: // JMPI
      addr = (c >> 6) & 0xffff;
      r = c & 0x7;
      m = c >> 3 & 0x3;

      vsdsp_cpu.ipr0 = 0;
      vsdsp_cpu.pc = addr;
      
      if (m == 1)
	 vsdsp_cpu.iregs[r]++;
      else if (m == 3)
	vsdsp_cpu.iregs[r]--;

    case 0x9: // CALLcc
      cond = c & 0x3f;
      if (cond > 0x20) {
	vsdsp_cpu.exception = SIGILL;
	return -1;
      }

      if (sim_test_condition(cond)) {
	vsdsp_cpu.lr0 = vsdsp_cpu.pc + 2;
	addr = (c >> 6) & 0xffff;
	vsdsp_cpu.pc = addr;
      }
      break;

    case 0x8: // Jcc
      cond = c & 0x3f;
      if (cond > 0x20) {
	vsdsp_cpu.exception = SIGILL;
	return -1;
      }
      vsdsp_cpu.mr0[2] &= ~VSDP_LOOP_FLAG;
      addr = (c >> 6) & 0xffff;
      if (sim_test_condition(cond)) {
	vsdsp_cpu.lr0 = vsdsp_cpu.pc + 2;  // TODO: We need to do actual pipelining
	addr = (c >> 6) & 0xffff;
	vsdsp_cpu.pc = addr;
      }
      break;
  
      case 0x4: // LOOP
      case 0x5:
      case 0x6:
      case 0x7:
	printf("%s LOOP not IMPLEMENTED\n", __func__);
	vsdsp_cpu.exception = SIGILL;
	break;

      case 0x2: // RESP
	r = (c >> 17) & 0x7;
	R = (c >> 20) & 0x7;
	// printf("%s: RESP %s, %s not IMPLEMENTED\n", __func__, alu_op[r], alu_op[R]);
	printf("%s: RESP not IMPLEMENTED\n", __func__);
	vsdsp_cpu.exception = SIGILL;
	break;

      case 0x1: // RETI
	if (c & (1 << 23)) {
	  r = (c >> 6) & 0x7;
	  printf("%s: RETI I%d not IMPLEMENTED\n", __func__, r);
	  vsdsp_cpu.exception = SIGILL;
	} else {
	  printf("%s: RETI not IMPLEMENTED\n", __func__);
	  vsdsp_cpu.exception = SIGILL;
	}
	break;

      case 0x0: // JRcc
	  
      default:
	printf("%s: RESERVED\n", __func__);
	vsdsp_cpu.exception = SIGILL;
	return -1;
    }
  return 0;
}

static int
sim_dmove(uint32_t c)
{
//	printf("in %s\n", __func__);
	sim_pmove_fullx(c >> 14);
	sim_pmove_fully(c);

	return 0;
}

static int
sim_add(uint32_t c)
{
  uint8_t r, R, A;
  uint64_t res;

  r = (c >> 20) & 0xf;
  R = (c >> 24) & 0xf;
  A = (c >> 17) & 0x7;
  
  res = fetch_alu_reg(r) + fetch_alu_reg(R);
  
  vsdsp_cpu.mr0[2] &= ~VSDP_CARRY_FLAG;
  vsdsp_cpu.mr0[2] &= ~VSDP_EXTENSION_FLAG;
  vsdsp_cpu.mr0[2] &= ~VSDP_ZERO_FLAG;
  vsdsp_cpu.mr0[2] &= ~VSDP_NEGATIVE_FLAG;
  vsdsp_cpu.mr0[2] &= ~VSDP_OVERFLOW_FLAG;
  if (r > 9 || R > 9) // Registers P, A, B, C, D are 40 bits.
    {
      vsdsp_cpu.mr0[2] |= res & (((uint64_t)1) << 40) ? VSDP_CARRY_FLAG : 0;
      vsdsp_cpu.mr0[2] |= res & 0xffffffffff ? 0 : VSDP_CARRY_FLAG;
      if (((res >> 31) & 0x1ff) == 0x1ff || ((res >> 31) & 0x1ff) == 0x0)
	vsdsp_cpu.mr0[2] |= VSDP_EXTENSION_FLAG;
    } else {
      vsdsp_cpu.mr0[2] |= res & (1 << 16) ? VSDP_CARRY_FLAG : 0;
      vsdsp_cpu.mr0[2] |= res & 0xffff ? 0 : VSDP_CARRY_FLAG;
    }
  update_alu_reg(A, res);
  return decode_pmove(c & 0x1ffff);
}

static int
sim_mac(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_sub(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_msu(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_addc(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_subc(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_ashl(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_and(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_or(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_xor(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static int
sim_reserved(uint32_t c)
{
  printf("in %s with %08x\n", __func__, c);

  return 0;
}

static char data_format[4][3] = { "SS", "SU", "US", "UU"};

// These operations all have an opcode 0xfxxxxxxx and use a single operand
static int
sim_single(uint32_t c)
{
  uint8_t op = (c >> 24) & 0xf, r, A, R;

  printf("%s c: %04x, op %x\n", __func__, c, op);
  r = (c >> 20) & 0xf;
  A = (c >> 17) & 0x7;
       
  switch(op)
    {
    case 0x0:
      // printf("abs %s, %s; ", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x1:
      // printf("asr %s, %s; ", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x2: // Logical Shift Right
      // printf("lsr %s, %s; ", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x3: // Logical shift right with Carry
      // printf("lsrc %s, %s; ", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x4:
      // printf("in %s NOP ", __func__);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x5:
      // printf("exp %s, %s; ", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x6: // Saturate 40 bit to 32 bits, setting overflow flag
      // printf("sat %s, %s; ", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0x7: // Round and saturate
      // printf("rnd %s, %s; ", alu_op[r], alu_op[A]);
      decode_pmove(c & 0x1ffff);
      break;
    case 0xe:
    case 0xf:
      r = (c >> 17) & 0x7;
      R = (c >> 20) & 0x7;
      // printf("mul%s %s, %s; ", data_format[(c >> 23) & 0x3], alu_op[r], alu_op[R]);
      decode_pmove(c & 0x1ffff);
      break;
    default:
      printf("in %s RESERVED\n", __func__);
    }

  return 0;
}

int (*op_decode[16])(uint32_t) =
{
  sim_ldc, sim_ldc, sim_control, sim_dmove, sim_add, sim_mac, sim_sub, sim_msu,
  sim_addc, sim_subc, sim_ashl, sim_and, sim_or, sim_xor, sim_reserved, sim_single
};

void
sim_resume (sd, step, siggnal)
     SIM_DESC sd;
     int step, siggnal;
{
  uint16_t pc;
  uint32_t opc;
  unsigned long insts;
  uint32_t inst, opcode;
  void (* sigsave)();
  uint32_t *memory = vsdsp_cpu.imemory;

  sigsave = signal (SIGINT, interrupt);
  vsdsp_cpu.exception = step ? SIGTRAP: 0;
  pc = vsdsp_cpu.pc;
  insts = vsdsp_cpu.insts;

  /* Run instructions here. */

  if (tracing)
    callback->printf_filtered (callback, "# set pc to 0x%x\n",
			       pc);

  do 
    {
      opc = pc;

      /* Fetch the instruction at pc */
      inst = memory[pc];
      
      /* Decode instruction  */
      opcode = inst >> 28;

      op_decode[opcode] (inst);
      
      insts++;

      // Update program counter only for non-control instructions
      if (opcode != 0x2)
	pc++;

      // Push mode register changes through the pipeline
      vsdsp_cpu.mr0[0] = vsdsp_cpu.mr0[1];
      vsdsp_cpu.mr0[1] = vsdsp_cpu.mr0[2];

    } while (!vsdsp_cpu.exception);

  /* Hide away the things we've cached while executing.  */
  vsdsp_cpu.pc = pc;
  vsdsp_cpu.insts += insts;		/* instructions done ... */

  signal (SIGINT, sigsave);
}

int
sim_write (SIM_DESC sd, SIM_ADDR addr, const void *buffer, int size)
{
  int i;
  init_pointers ();
  
  memcpy (& vsdsp_cpu.imemory[addr], buffer, size);
  
  return size;
}

int
sim_read (SIM_DESC sd, SIM_ADDR addr, void *buffer, int size)
{
  int i;
  init_pointers ();
  
  memcpy (buffer, & vsdsp_cpu.imemory[addr], size);
  
  return size;
}

void
sim_stop_reason (SIM_DESC sd, enum sim_stop *reason, int *sigrc)
{
  if (vsdsp_cpu.exception == SIGQUIT)
    {
      * reason = sim_exited;
      * sigrc = vsdsp_cpu.aregs[0].reg.a0;
    }
  else
    {
      * reason = sim_stopped;
      * sigrc = vsdsp_cpu.exception;
    }
}


int
sim_stop (sd)
     SIM_DESC sd;
{
  vsdsp_cpu.exception = SIGINT;
  return 1;
}


void
sim_info (sd, verbose)
     SIM_DESC sd;
     int verbose;
{
  callback->printf_filtered (callback, "\n\n# instructions executed  %10ld\n",
			     vsdsp_cpu.insts);
}


SIM_DESC
sim_open (SIM_OPEN_KIND kind, host_callback *cb, struct bfd *abfd, char * const *argv)
{
  int osize = SIM_IMEM_SIZE;
  myname = argv[0];
  callback = cb;
  
  if (kind == SIM_OPEN_STANDALONE)
    issue_messages = 1;
  
  /* Discard and reacquire memory -- start with a clean slate.  */
  sim_size_i_mem (1);		/* small */
  sim_size_i_mem (osize);	/* and back again */
  sim_size_x_mem (1);		/* small */
  sim_size_x_mem (osize);	/* and back again */
  sim_size_y_mem (1);		/* small */
  sim_size_y_mem (osize);	/* and back again */

  set_initial_gprs ();	/* Reset the GPR registers.  */
  
  /* Fudge our descriptor for now.  */
  return (SIM_DESC) 1;
}

void
sim_close (sd, quitting)
     SIM_DESC sd;
     int quitting;
{
  /* nothing to do */
}


SIM_RC
sim_create_inferior (SIM_DESC sd, struct bfd *prog_bfd,
		     char * const *argv, char * const *env)
{
  char ** avp;
  int l;

  /* Set the initial register set.  */
  l = issue_messages;
  issue_messages = 0;
  set_initial_gprs ();
  issue_messages = l;
  
  vsdsp_cpu.pc = bfd_get_start_address (prog_bfd);

  return SIM_RC_OK;
}
