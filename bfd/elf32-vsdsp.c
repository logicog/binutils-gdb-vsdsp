/* vsdsp support for 32-bit ELF
   Copyright 2022 Free Software Foundation, Inc.

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "bfd.h"
#include "libbfd.h"
#include "elf-bfd.h"
#include "elf/vsdsp.h"

static bfd_reloc_status_type
vsdsp_elf_reloc (bfd *abfd,
		 arelent *reloc_entry,
		 asymbol *symbol_in,
		 void * data,
		 asection *input_section,
		 bfd *output_bfd,
		 char **error_message ATTRIBUTE_UNUSED)
{
  uint32_t insn;
  bfd_vma sym_value;
  uint32_t val;
  enum elf_vsdsp_reloc_type r_type;
  bfd_vma addr = reloc_entry->address;
  bfd_byte *hit_data = addr + (bfd_byte *) data;

  printf("%s called\n", __func__);
  r_type = (enum elf_vsdsp_reloc_type) reloc_entry->howto->type;

  if (output_bfd != NULL)
    {
      /* Partial linking--do nothing.  */
      printf("%s called A\n", __func__);
      reloc_entry->address += input_section->output_offset;
      return bfd_reloc_ok;
    }

      printf("%s called B\n", __func__);
  if (symbol_in != NULL
      && bfd_is_und_section (symbol_in->section))
    return bfd_reloc_undefined;

      printf("%s called C\n", __func__);
  if (bfd_is_com_section (symbol_in->section))
    sym_value = 0;
  else
    sym_value = (symbol_in->value +
		 symbol_in->section->output_section->vma +
		 symbol_in->section->output_offset);

  switch (r_type)
    {
    case R_VSDSP_DIR16:
      insn = bfd_get_32 (abfd, hit_data);
      val = (insn >> 6) & 0xffff;
      insn &= 0xfffc0003f;
      printf("%s insn: %08x, sym_value %08lx \n", __func__, insn, sym_value);
      insn |= ((sym_value + reloc_entry->addend + val) & 0xffff) << 6;
      printf("%s insn: now %08x\n", __func__, insn);
      bfd_put_32 (abfd, (bfd_vma) insn, hit_data);
      break;

    default:
      printf("%s called E\n", __func__);
      abort ();
      break;
    }

  return bfd_reloc_ok;
}

static reloc_howto_type vsdsp_elf_howto_table[] =
{
  /* No relocation.  */
  HOWTO (R_VSDSP_NONE,		/* type */
	 0,			/* rightshift */
	 0,			/* size (0 = byte, 1 = short, 2 = long) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont, /* complain_on_overflow */
	 vsdsp_elf_reloc,	        /* special_function */
	 "R_VSDSP_NONE",	        /* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* 32 bit absolute relocation.  Setting partial_inplace to TRUE and
     src_mask to a non-zero value is similar to the COFF toolchain.  */
  HOWTO (R_VSDSP_DIR16,	        /* type */
	 0,			/* rightshift */
	 2,			/* size (0 = byte, 1 = short, 2 = long) */
	 16,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 vsdsp_elf_reloc,		/* special_function */
	 "R_VSDSP_DIR16",		/* name */
	 true,			/* partial_inplace */
	 0xffff,		/* src_mask */
	 0xffff,		/* dst_mask */
	 false),		/* pcrel_offset */
};

/* This structure is used to map BFD reloc codes to vsdsp elf relocs.  */

struct elf_reloc_map
{
  bfd_reloc_code_real_type bfd_reloc_val;
  unsigned char elf_reloc_val;
};

/* An array mapping BFD reloc codes to vsdsp elf relocs.  */

static const struct elf_reloc_map vsdsp_reloc_map[] =
{
    { BFD_RELOC_NONE, 		R_VSDSP_NONE          },
    { BFD_RELOC_16, 		R_VSDSP_DIR16         }
};

/* Given a BFD reloc code, return the howto structure for the
   corresponding vsdsp elf reloc.  */

static reloc_howto_type *
vsdsp_elf_reloc_type_lookup (bfd *abfd ATTRIBUTE_UNUSED,
			     bfd_reloc_code_real_type code)
{
  unsigned int i;

  for (i = 0; i < sizeof (vsdsp_reloc_map) / sizeof (struct elf_reloc_map); i++)
    if (vsdsp_reloc_map[i].bfd_reloc_val == code)
      return & vsdsp_elf_howto_table[(int) vsdsp_reloc_map[i].elf_reloc_val];

  return NULL;
}

static reloc_howto_type *
vsdsp_elf_reloc_name_lookup (bfd *abfd ATTRIBUTE_UNUSED,
			  const char *r_name)
{
  unsigned int i;

  for (i = 0;
       i < sizeof (vsdsp_elf_howto_table) / sizeof (vsdsp_elf_howto_table[0]);
       i++)
    if (vsdsp_elf_howto_table[i].name != NULL
	&& strcasecmp (vsdsp_elf_howto_table[i].name, r_name) == 0)
      return &vsdsp_elf_howto_table[i];

  return NULL;
}

/* Given an ELF reloc, fill in the howto field of a relent.  */
static bool
vsdsp_elf_info_to_howto (bfd *abfd ATTRIBUTE_UNUSED,
		      arelent *cache_ptr,
		      Elf_Internal_Rela *dst)
{
  unsigned int r;

  r = ELF32_R_TYPE (dst->r_info);

  BFD_ASSERT (r < (unsigned int) R_VSDSP_MAX);

  cache_ptr->howto = &vsdsp_elf_howto_table[r];
  return true;
}

#define TARGET_BIG_SYM			vsdsp_elf32_vec
#define TARGET_BIG_NAME			"elf32-vsdsp"
#define ELF_ARCH			bfd_arch_vsdsp
#define ELF_MACHINE_CODE		EM_VSDSP
#define ELF_MAXPAGESIZE  		1
#define bfd_elf32_bfd_reloc_type_lookup vsdsp_elf_reloc_type_lookup
#define bfd_elf32_bfd_reloc_name_lookup vsdsp_elf_reloc_name_lookup
#define elf_info_to_howto		vsdsp_elf_info_to_howto
#define OCTETS_PER_BYTE(ABFD, SEC) 4

/*
#define bfd_elf32_bfd_reloc_type_lookup bfd_default_reloc_type_lookup
#define bfd_elf32_bfd_reloc_name_lookup _bfd_norelocs_bfd_reloc_name_lookup
#define elf_info_to_howto		_bfd_elf_no_info_to_howto
*/
#include "elf32-target.h"
