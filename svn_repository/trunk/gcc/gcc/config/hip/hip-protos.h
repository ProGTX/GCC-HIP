/* Prototypes for hip.c functions used in the md file & elsewhere.
	 Copyright (C) 2009, 2010
	 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.	If not see
<http://www.gnu.org/licenses/>.	*/

extern void	hip_push_register(int);
extern void	hip_pop_register(int);
extern void	hip_expand_prologue(void);
extern void	hip_expand_epilogue(void);
extern int	hip_initial_elimination_offset(int, int);
extern void	hip_print_operand(FILE*, rtx, int);
extern void	hip_print_operand_address(FILE*, rtx);
extern bool hip_return_in_memory(const_tree, const_tree);
extern void hip_asm_output_skip(FILE*, int);