/* Prototypes for exported functions defined in hip.c
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007, 2010
   Free Software Foundation, Inc.
   Contributed by Hans-Peter Nilsson (hp@bitrange.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

extern void hip_init_expanders (void);
extern int hip_eh_return_data_regno (int);
extern int hip_initial_elimination_offset (int, int);
extern int hip_starting_frame_offset (void);
extern int hip_function_arg_regno_p (int, int);
extern void hip_function_profiler (FILE *, int);
extern int hip_reversible_cc_mode (enum machine_mode);
extern int hip_register_move_cost
  (enum machine_mode, enum reg_class, enum reg_class);
extern const char *hip_text_section_asm_op (void);
extern const char *hip_data_section_asm_op (void);
extern void hip_output_quoted_string (FILE *, const char *, int);
extern void hip_asm_output_source_line  (FILE *, int);
extern void hip_asm_output_ascii (FILE *, const char *, int);
extern void hip_asm_output_label (FILE *, const char *);
extern void hip_asm_output_internal_label (FILE *, const char *);
extern void hip_asm_weaken_label (FILE *, const char *);
extern void hip_asm_output_labelref (FILE *, const char *);
extern void hip_asm_output_def (FILE *, const char *, const char *);
extern int hip_print_operand_punct_valid_p (int);
extern void hip_asm_output_reg_push (FILE *, int);
extern void hip_asm_output_reg_pop (FILE *, int);
extern void hip_asm_output_skip (FILE *, int);
extern void hip_asm_output_align (FILE *, int);
extern int hip_shiftable_wyde_value (unsigned HOST_WIDEST_INT);
extern void hip_output_register_setting (FILE *, int, HOST_WIDEST_INT, int);
extern int hip_opposite_regno (int, int);
extern int hip_local_regno (int);
extern unsigned hip_dbx_register_number (unsigned);
extern int hip_use_simple_return (void);
extern void hip_make_decl_one_only (tree);
extern int hip_data_alignment (tree, int);
extern int hip_constant_alignment (tree, int);
extern unsigned hip_local_alignment (tree, unsigned);
extern void hip_asm_output_pool_prologue (FILE *, const char *, tree, int);
extern void hip_asm_output_aligned_common (FILE *, const char *, int, int);
extern void hip_asm_output_aligned_local (FILE *, const char *, int, int);
extern void hip_asm_declare_register_global
  (FILE *, tree, int, const char *);
extern void hip_asm_output_addr_diff_elt (FILE *, rtx, int, int);
extern void hip_asm_output_addr_vec_elt (FILE *, int);
extern enum reg_class hip_preferred_reload_class (rtx, enum reg_class);
extern enum reg_class hip_preferred_output_reload_class
  (rtx, enum reg_class);
extern enum reg_class hip_secondary_reload_class
  (enum reg_class, enum machine_mode, rtx, int);
extern int hip_const_ok_for_letter_p (HOST_WIDE_INT, int);
extern int hip_const_double_ok_for_letter_p (rtx, int);
extern int hip_extra_constraint (rtx, int, int);
extern rtx hip_dynamic_chain_address (rtx);
extern rtx hip_return_addr_rtx (int, rtx);
extern rtx hip_eh_return_stackadj_rtx (void);
extern rtx hip_eh_return_handler_rtx (void);
extern int hip_constant_address_p (rtx);
extern int hip_legitimate_constant_p (rtx);
extern void hip_print_operand (FILE *, rtx, int);
extern void hip_print_operand_address (FILE *, rtx);
extern void hip_expand_prologue (void);
extern void hip_expand_epilogue (void);
extern rtx hip_get_hard_reg_initial_val (enum machine_mode, int);
extern int hip_asm_preferred_eh_data_format (int, int);
extern void hip_setup_frame_addresses (void);

#ifdef RTX_CODE
/* Needs to be ifdef:d for sake of enum rtx_code.  */
extern enum machine_mode hip_select_cc_mode (enum rtx_code, rtx, rtx);
extern void hip_canonicalize_comparison (enum rtx_code *, rtx *, rtx *);
extern rtx hip_gen_compare_reg (enum rtx_code, rtx, rtx);
#endif

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */