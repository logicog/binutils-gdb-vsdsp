/* tc-vsdsp.c -- Assemble code for vsdsp
   Copyright 2008
   Free Software Foundation, Inc.

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to
   the Free Software Foundation, 51 Franklin Street - Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* Contributed by Birger Koblitz  */

#include "as.h"
#include "safe-ctype.h"
#include "opcode/vsdsp.h"
#include <assert.h>

extern const vsdsp_opc_info_t vsdsp_opc_info[128];

const char comment_chars[]        = "#";
const char line_separator_chars[] = ";";
const char line_comment_chars[]   = "#";

const char EXP_CHARS[] = "eE";
const char FLT_CHARS[] = "rRsSfFdDxXeE";

static int pending_reloc;
static char *last_output;
static unsigned char last_flags;
static htab_t opcode_hash_control;
static htab_t treg_hash_control;
static htab_t cc_hash_control;

static char *
parse_exp  (char *s, expressionS *op)
{
  char *save = input_line_pointer;

  input_line_pointer = s;
  expression (op);
  s = input_line_pointer;
  input_line_pointer = save;
  return s;
}

static inline char *
skip_space (char *s)
{
  while (*s == ' ' || *s == '\t')
    ++s;

  return s;
}

/* Parse a register used as target in a load/store operation
 */
static int
parse_target_reg (char **sptr)
{
  char *s = skip_space (*sptr);
  char buf[10];
  struct target_reg_entry *treg;
  int cnt;

  cnt = 0;
  memset (buf, '\0', 10);
  while ((ISALNUM (*s)) && cnt < 10)
    buf[cnt++] = TOLOWER (*s++);

  treg = (struct target_reg_entry *) str_hash_find (treg_hash_control, buf);

  if (treg == NULL)
    {
      as_bad (_("unknown target register %s"), buf);
      return -1;
    }

  *sptr = s;
  return treg->code;
}

/* Parse a register used in an arithmetic operation, they are:
 * a0, a1, b0, b1, c0, c1, d0, d1,
 * null, ones, rsrvd, p, a, b, c, d
 */
static int
parse_alu_reg (char **sptr)
{
  char *s = skip_space (*sptr);
  char buf[8];
  int cnt;
  int l, r;

  cnt = 0;
  memset (buf, '\0', 8);
  while ((ISALNUM (*s)) && cnt < 8)
    buf[cnt++] = TOLOWER (*s++);

  // Optimize search span based on the length of the register names
  l = cnt < 2 ? 11 : 0;
  r = cnt != 2 ? 16 : 8;
  do
    {
      int ans = strcmp (buf, alu_op[l]);

      if (ans) {
	  *sptr = s;
	  return l;
      }
      l++;
    }
  while (l < r);

  return -1;
}

const pseudo_typeS md_pseudo_table[] =
{
  {0, 0, 0}
};

void
md_operand (expressionS *op __attribute__((unused)))
{
  /* Empty for now. */
}

/* This function is called once, at assembler startup time.  It sets
   up the hash table with all the opcodes in it, and also initializes
   some aliases for compatibility with other assemblers. */

void
md_begin (void)
{
  const vsdsp_opc_info_t *opcode;
  const struct target_reg_entry *treg;
  const struct target_cc_entry *ccentry;
  int count;

  opcode_hash_control = str_htab_create ();
  treg_hash_control = str_htab_create ();
  cc_hash_control = str_htab_create ();

  /* Insert memonics of the major opcodes into hash table.  */
  for (count = 0, opcode = vsdsp_opc_info; count++ < N_VSDSP_OPCODES; opcode++)
    str_hash_insert (opcode_hash_control, opcode->name, opcode, 0);

  /* Create hash table of target register names */
  for (count = 0, treg = target_regs; count++ < 64; treg++)
    str_hash_insert (treg_hash_control, treg->name, treg, 0);

  /* Create hash table of control codes struct target_cc_entry target_cc_codes */
  for (count = 0, ccentry = target_cc_codes; count++ < 14; ccentry++)
    str_hash_insert (cc_hash_control, ccentry->name, ccentry, 0);

  last_flags = 0;
  last_output = NULL;

  bfd_set_arch_mach (stdoutput, TARGET_ARCH, 0);
}

struct move_op {
  int addr_reg;
  bool is_indirect;
  int post_mod;
  int post_mod_type;
  unsigned char post_mod_code;
};

static int
read_move_op(char **s, struct move_op *op)
{
  const char *op_end;
  char *str = *s;

  // printf("%s A >%s<\n", __func__, str);
  op->is_indirect = false;
  op->post_mod_code = 0;
  if (*str == '(')
    {
      op->is_indirect = true;
      str++;
    }
  str = skip_space (str);
  if (TOLOWER (*str) != 'i')
    {
      as_bad (_("not a valid address operand"));
      ignore_rest_of_line ();
      return -1;
    }
  op->addr_reg = *(++str) - '0';
  if (op->addr_reg < 0 || op->addr_reg > 7)
    {
      as_bad (_("not a valid address operand"));
      ignore_rest_of_line ();
      return -1;
    }
  // printf("%s A1 >%s<\n", __func__, str);
  str = skip_space (str + 1);
  if (op->is_indirect)
    {
      if (*str++ != ')')
	{
	  as_bad (_("missing closing parenthesis"));
	  ignore_rest_of_line ();
	  return -1;
	}
    }
  // printf("%s B >%s<\n", __func__, str);
  op_end = str;
  while (*op_end && *op_end != ','
	 && *op_end != '-' && *op_end != '+' && !ISDIGIT(*op_end))
    op_end++;
//  printf("%s B1 op_end >%s<\n", __func__, op_end);
  if (*op_end == '+' || *op_end == '-' || ISDIGIT(*op_end))
    op->post_mod = strtol(str, &str, 10);
  else
    op->post_mod = 0;
//  printf("%s C >%s<\n", __func__, str);
  if (op->post_mod)
    printf("%s offset is %d\n", __func__, op->post_mod);
  if (op->post_mod < -7  || op->post_mod > 7)
    {
      as_bad (_("post modification is invalid"));
      ignore_rest_of_line ();
      return -1;
    }
  if (op->post_mod >= 0)
    op->post_mod_code = op->post_mod;
  else
    op->post_mod_code = (abs(op->post_mod) ^ 0xf) + 1 ;
  *s = str;
  return 0;
}

/* This is the guts of the machine-dependent assembler.  STR points to
   a machine dependent instruction.  This function is supposed to emit
   the frags/bytes it assembles to.  */

void
md_assemble (char *str)
{
  char *op_start;
  char *op_end;

  vsdsp_opc_info_t *opcode;
  char *output = 0;
  uint32_t iword = 0;
  int reg, reg2, A;
  char pend;
  bool need_fix = false;
  expressionS exp;
  struct move_op m_op;
  struct target_cc_entry *condition;
  char cc_code;
  // Single full move
  uint16_t f_move;
  // Parallel move (full, 2xshort, register-to-register, long-X or I-bus)
  uint32_t p_move;
  // Short move as part of a double short move in a parallel move
  uint16_t s_move;
  bool	line_continued = false;

  do
    {
      /* Initialize the expression.  */
      exp.X_op = O_absent;

      int nlen = 0;

      /* Drop leading whitespace.  */
      str = skip_space (str);

      /* Find the op code end.  */
      op_start = str;
      for (op_end = str; !is_end_of_line[*op_end & 0xff] && *op_end != ' ';
	   op_end++)
	nlen++;

      pend = *op_end;
      *op_end = 0;
  
      printf("%s >>>>>>%s<<<<<<<\n", __func__, op_start);
      if (nlen == 0)
	as_bad (_("can't find opcode "));

      opcode = (vsdsp_opc_info_t *) str_hash_find (opcode_hash_control, op_start);
      *op_end = pend;

      if (opcode == NULL)
	{
	  as_bad (_("unknown opcode %s"), op_start);
	  return;
	}

      printf("USING opcode %x %s\n", opcode->opcode, opcode->name);

      while (ISSPACE (*op_end))
	op_end++;
      str = op_end;

      switch (opcode->itype)
      {
	case VSDSP_OP_LDC:		// ldc value, reg
	  str = parse_exp (str, &exp);
	  str = skip_space (str);
	  if (exp.X_op != O_absent && *str == ',')
	    {
	      int immediate = exp.X_add_number;

	      str = skip_space (str + 1);
	      reg = parse_target_reg (&str);
	      if (reg < 0) {
		as_bad (_("not a valid target register "));
		return;
	      }
	      iword = opcode->opcode << 28 | (immediate & 0xffff) << 6 | reg;
	      printf("%s: Got immediate %d, target register is %d\n", __func__, immediate, reg);
	      need_fix = true;
	    }
	  break;

	case VSDSP_OP_CONTROL:
	  printf("%s identified VSDSP_OP_CONTROL\n", __func__);
	  cc_code = 0;

	  switch (opcode->opcode)
	  {
	    case 0x20: // JRcc
	      printf("%s identified JRcc\n", __func__);
	      need_fix = false;
	      op_start += 2;
	      break;
	    case 0x21: // RETI
	      need_fix = false;
	      op_start += 4;
	      break;
	    case 0x22: // JMPI
	      need_fix = true;
	      op_start += 4;
	      break;
	    case 0x28: // Jcc
	      need_fix = true;
	      op_start += 1;
	      break;
	    case 0x29: // CALLcc
	      need_fix = true;
	      op_start += 4;
	      break;
	    case 0x2d: // HALT
	      need_fix = false;
	      op_start += 4;
	      break;
	    default:
	      as_bad (_("invalid control instruction "));
	      return;
	  }
	  if (*op_start &&  ISALNUM (*op_start))
	    {
	      pend = *(op_start + 2);
	      *(op_start + 2) = '\0';
	      condition = (struct target_cc_entry *) str_hash_find (cc_hash_control, op_start);
	      *(op_start + 2) = pend;
	      printf("%s identified CALL >%s<\n", __func__, op_start);
	      if (!condition)
		{
		  as_bad (_("invalid condition code "));
		  return;
		}
	      printf("%s identified CALL with condition %s\n", __func__, condition->name);
	      cc_code = condition->code;
	    }
	  printf("%s need fix? %d %x\n", __func__, need_fix, opcode->opcode);
	  if (need_fix)
	    str = parse_exp (str, &exp);
	  str = skip_space (str);
	  iword = opcode->opcode << 24 | cc_code;
	  break;

	case VSDSP_OP_ADD:
	case VSDSP_OP_ADDC:
	case VSDSP_OP_MAC:
	case VSDSP_OP_SUB:
	case VSDSP_OP_MSU:
	case VSDSP_OP_SUBC:
	case VSDSP_OP_ASLH:
	case VSDSP_OP_AND:
	case VSDSP_OP_OR:
	case VSDSP_OP_XOR:
	  reg = parse_alu_reg (&str);
	  str = skip_space (str + 1);
	  reg2 = parse_alu_reg (&str);
	  str = skip_space (str + 1);
	  A = parse_alu_reg (&str) & 0x7;
	  printf("%s: Got reg %d, reg2 %d, A: %d\n", __func__, reg, reg2, A);
	  iword = opcode->opcode << 28 | reg << 24 | reg2 << 20 | A << 17;
	  iword |= PARALLEL_MV_NOP;
	  break;

	case VSDSP_OP_SINGLE:
	  // Is this a NOP?
	  if (opcode->opcode == 0xf4)
	    {
	      // For now we fill the parallel move slot with another NOP
	      iword = 0xf4000000 | PARALLEL_MV_NOP;
	      break;
	    }
	  reg2 = parse_alu_reg (&str);
	  str = skip_space (str + 1);
	  reg = parse_alu_reg (&str);
	  if (reg >= 8 )
	    {
	      as_bad (_("not a valid target register "));
	      return;
	    }
	  iword = opcode->opcode << 24 | reg2 << 20 | reg << 17;
	  printf("%s a iword %08x\n", __func__, iword);
	  if (opcode->flags & OP_ALLOWS_PMOVE)
	    iword |= PARALLEL_MV_NOP;
	  printf("%s b iword %08x\n", __func__, iword);
	  break;

	  /* mvx, mvy, stx/y/i, ldx/y/i which can stand alone, be paired in a
	    double move or just be a side-effect in a parallel move to an arithmetic operation */
	  case VSDSP_OP_MOVE:
	    printf("%s: In VSDSP_OP_MOVE, line continued: %d\n", __func__, line_continued);
	    f_move = p_move = 0x0;
	    switch (opcode->name[0])
	    {
	      case 's': // Store operation
		reg = parse_target_reg (&str);
		str = skip_space (str + 1);
		if (read_move_op(&str, &m_op))
		  return;
		p_move = 1 << 13 | m_op.addr_reg << 10 | m_op.post_mod_code << 6 | reg;
		f_move = p_move | (opcode->name[2] == 'y' ? (1 << 15) : 0);
		break;
	      case 'l': // Load operation
		if (read_move_op(&str, &m_op))
		  return;
		str = skip_space (str + 1);
		reg = parse_target_reg (&str);
		p_move = m_op.addr_reg << 10 | m_op.post_mod_code << 6 | reg;
		f_move = p_move | (opcode->name[2] == 'y' ? (1 << 15) : 0);
		break;
	      case 'm':  // Register-register move on the y-bus, only as parallel move
		reg = parse_target_reg (&str);
		str = skip_space (str + 1);
		reg2 = parse_target_reg (&str);
		p_move = 0x15 << 10 | reg << 6 | reg2;
		break;
	      default:
		abort();
	    }
	    /* Verify if the full move fulfills the conditions for a short move */
	    if (f_move && ! (f_move & (0x3f << 3)))
	      s_move = ((f_move & (0x1f << 9)) << 3) | (f_move & 0x7);
	    else
	  s_move = S_MOVE_INV;
	    printf("%s In move, last_flags: %x\n", __func__, last_flags);
	    if (last_flags & OP_ALLOWS_PMOVE && opcode->flags & OP_IN_PMOVE)
	      {
		printf("%s testing parallel move last_output %08x f_move %04x p_move %08x s_move %04x\n",
		      __func__, iword, f_move, p_move, s_move);
		iword &= 0xfffe0000;
		iword |= opcode->opcode << 10 |  m_op.addr_reg << 6 | reg;
	      }
	    else if (! (last_flags & OP_ALLOWS_PMOVE))
	      {
		printf("%s starting standalone move f_move %04x p_move %08x s_move %04x\n", __func__, f_move, p_move, s_move);
		f_move = opcode->name[0] == 's' ? (1 << 13) : 0;
		if (opcode->name[2] == 'x')
		  iword = (DOUBLE_FULL_MOVES_OPCODE << 28) | f_move << 14 | FULL_YMOVE_NOP;
		else
		  iword = (DOUBLE_FULL_MOVES_OPCODE << 28) | FULL_YMOVE_NOP | (f_move << 14);
	      }
	    break;

	default:
	  printf("%s reserved\n", __func__);
	}

      if (*str == ';')
	{
	  line_continued = true;
	  str = skip_space (str + 1);
	  printf("%s line continues: >%s<\n", __func__, str);
	}
      last_flags = opcode->flags;
    }
  while (*str && *str != '#');
  
  printf("%s final iword %08x\n", __func__, iword);
  printf("%s: One more FRAG\n", __func__);
  output = frag_more (4);
  printf("%s storing %08x\n", __func__, iword);
  md_number_to_chars (output, iword, 4);

  if (need_fix)
    {
      printf("%s NEED FIX output %016lx literal: %016lx\n", __func__, (intptr_t)output, (intptr_t)frag_now->fr_literal);
	     fix_new_exp (frag_now,
			  (output - frag_now->fr_literal),
			   4,
			  &exp,
			  0,
			  BFD_RELOC_16);
    }

  if (*str != 0)
    as_warn ("extra stuff on line ignored");
  
  if (pending_reloc)
    as_bad ("Something forgot to clean up\n");
}

/* Turn a string in input_line_pointer into a floating point constant
   of type type, and store the appropriate bytes in *LITP.  The number
   of LITTLENUMS emitted is stored in *SIZEP .  An error message is
   returned, or NULL on OK.  */

const char *
md_atof (int type, char *litP, int *sizeP)
{
  int prec;
  LITTLENUM_TYPE words[4];
  char *t;
  int i;

  switch (type)
    {
    case 'f':
      prec = 2;
      break;

    case 'd':
      prec = 4;
      break;

    default:
      *sizeP = 0;
      return _("bad call to md_atof");
    }

  t = atof_ieee (input_line_pointer, type, words);
  if (t)
    input_line_pointer = t;

  *sizeP = prec * 2;

  for (i = prec - 1; i >= 0; i--)
    {
      md_number_to_chars (litP, (valueT) words[i], 2);
      litP += 2;
    }

  return NULL;
}

const char *md_shortopts = "";

struct option md_longopts[] =
{
  {NULL, no_argument, NULL, 0}
};
size_t md_longopts_size = sizeof (md_longopts);

/* We have no target specific options yet, so these next
   two functions are empty.  */
int
md_parse_option (int c ATTRIBUTE_UNUSED, const char *arg ATTRIBUTE_UNUSED)
{
  return 0;
}

void
md_show_usage (FILE *stream ATTRIBUTE_UNUSED)
{
}

/* Apply a fixup to the object file.  */

void
md_apply_fix (fixS *fixP, valueT * valP ATTRIBUTE_UNUSED, segT seg ATTRIBUTE_UNUSED)
{
  char *buf = fixP->fx_where + fixP->fx_frag->fr_literal;
  uint32_t val = *valP;
  uint32_t max, min;
  uint32_t op = 0;

  printf("%s applying fix %08lx, val %08x\n", __func__, (intptr_t)buf, val);
  max = min = 0;
  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_16:
      for (int i = 0; i < 4; i++) {
	  op <<= 8;
	  op |= *buf++;
	}
	op |= (val & 0xffff) << 6;
	printf("%s op is now %08x\n", __func__, op);
       *buf++ = op >> 24;
       *buf++ = op >> 16;
       *buf++ = op >> 8;
       *buf++ = op >> 0;
      break;

    default:
      abort ();
    }

  if (max != 0 && (val < min || val > max))
    as_bad_where (fixP->fx_file, fixP->fx_line, _("offset out of range"));

  if (fixP->fx_addsy == NULL && fixP->fx_pcrel == 0)
    fixP->fx_done = 1;
}

/* Put number into target byte order (big endian).  */

void
md_number_to_chars (char *ptr, valueT use, int nbytes)
{
  number_to_chars_bigendian (ptr, use, nbytes);
}

void
md_convert_frag (bfd *abfd ATTRIBUTE_UNUSED,
                 segT sec ATTRIBUTE_UNUSED,
                 fragS *fragP ATTRIBUTE_UNUSED)
{
  abort();
}

/* Translate internal representation of relocation info to BFD target
   format.  */

arelent *
tc_gen_reloc (asection *section ATTRIBUTE_UNUSED, fixS *fixp)
{
  arelent *rel;
  bfd_reloc_code_real_type r_type;

  rel = xmalloc (sizeof (arelent));
  rel->sym_ptr_ptr = xmalloc (sizeof (asymbol *));
  *rel->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
  rel->address = fixp->fx_frag->fr_address + fixp->fx_where;

  r_type = fixp->fx_r_type;
  rel->addend = fixp->fx_addnumber;
  rel->howto = bfd_reloc_type_lookup (stdoutput, r_type);

  if (rel->howto == NULL)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
		    _("Cannot represent relocation type %s"),
		    bfd_get_reloc_code_name (r_type));
      /* Set howto to a garbage value so that we can keep going.  */
      rel->howto = bfd_reloc_type_lookup (stdoutput, BFD_RELOC_32);
      assert (rel->howto != NULL);
    }

  return rel;
}
