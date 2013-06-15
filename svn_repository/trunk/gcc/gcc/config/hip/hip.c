/* Target Code for hip
	 Copyright (C) 2008, 2009, 2010  Free Software Foundation
	 Contributed by Anthony Green.

	 This file is part of GCC.

	 GCC is free software; you can redistribute it and/or modify it
	 under the terms of the GNU General Public License as published
	 by the Free Software Foundation; either version 3, or (at your
	 option) any later version.

	 GCC is distributed in the hope that it will be useful, but WITHOUT
	 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
	 or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
	 License for more details.

	 You should have received a copy of the GNU General Public License
	 along with GCC; see the file COPYING3.  If not see
	 <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "reload.h"
#include "diagnostic-core.h"
#include "obstack.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"
#include "langhooks.h"
#include "df.h"
#include "string.h"

#define LOSE_AND_RETURN(msgid, x)	\
if(1)								\
{									\
	hip_operand_lossage(msgid, x);	\
	return;							\
}

/*
	TODO: Probably need to break up for large values.
*/
void
hip_asm_output_skip(FILE* stream, int nbytes)
{
	char zeros[1024];

	strcpy(zeros, "0");

	int i;
	for(i = 1; i < nbytes - 1; ++i)
	{
		strcat(zeros, ",0");
	}

	if(nbytes > 1)
	{
		strcat(zeros, ",0");
	}

	fprintf(stream, "\t.byte\t%s\n", zeros);
}

static bool
hip_assemble_integer(rtx x, unsigned int size, int aligned_p)
{
	char name[6];

	switch(size)
	{
		case 1:
			strcpy(name, "byte");
			break;
		case 2:
			strcpy(name, "word16");
			break;
		case 4:
			strcpy(name, "word");
			break;
		case 8:
			strcpy(name, "word64");
			break;
		default:
			fprintf(stderr, "Size of integer cannot be %u", size);
			return false;
	}

	if(aligned_p != 1)
	{
		fprintf(stderr, "%s is not aligned, aligned_p: %d", name, aligned_p);
	}

	fprintf(asm_out_file, "\t.%s\t", name);
	hip_print_operand(asm_out_file, x, 0);
	fprintf(asm_out_file, "\n", name);

	return true;
}

static void
hip_file_start(void)
{
	default_file_start();

	/*
		Initialize stack and jump to main.
	*/
	fprintf(
		asm_out_file,
		"	.text\n"
		"	lhi		r30, 0x4FC\n"
		"	addui	r30, r30, 0x4FC\n"
		"	lhi		r26, main\n"
		"	addui	r26, r26, main\n"
		"	j		0(r26)\n"
	);

	/*
		For some strange reason GCC sometimes prints variables before jumping to the data section.
		TODO: This is a work-around.
	*/
	switch_to_section(data_section);
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

bool
hip_return_in_memory(const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
	const HOST_WIDE_INT size = int_size_in_bytes(type);
	return (size == -1 || size > 2 * UNITS_PER_WORD);
}

/* Define how to find the value returned by a function.
	 VALTYPE is the data type of the value (as a tree).
	 If the precise function being called is known, FUNC is its
	 FUNCTION_DECL; otherwise, FUNC is 0.

	 We always return values in register r28 for hip.  */

static rtx
hip_function_value(
	const_tree rettype,
	const_tree fntype_or_decl ATTRIBUTE_UNUSED,
	bool outgoing ATTRIBUTE_UNUSED
)
{
	return gen_rtx_REG(TYPE_MODE(rettype), HIP_RV);
}

/* Define how to find the value returned by a library function.

	 We always return values in register r28 for hip.  */

static rtx
hip_libcall_value(
	enum machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED
)
{
	return gen_rtx_REG(mode, HIP_RV);
}

/* Handle TARGET_FUNCTION_VALUE_REGNO_P.

	 We always return values in register r28 for hip.  */

static bool
hip_function_value_regno_p (const unsigned int regno)
{
	return (regno == HIP_RV);
}

/* Emit an error message when we're in an asm, and a fatal error for
	 "normal" insns.  Formatted output isn't easily implemented, since we
	 use output_operand_lossage to output the actual message and handle the
	 categorization of the error.  */

static void
hip_operand_lossage (const char *msgid, rtx op)
{
	debug_rtx(op);
	output_operand_lossage("%s", msgid);
}

/* The PRINT_OPERAND_ADDRESS worker.  */

void
hip_print_operand_address(FILE *file, rtx x)
{
	switch(GET_CODE(x))
	{
		case REG:
			fprintf(file, "0(%s)", reg_names[REGNO(x)]);
			break;

		case PLUS:
			switch(GET_CODE(XEXP(x, 1)))
			{
				case CONST_INT:
					fprintf(
						file,
						"%ld(%s)",
						INTVAL(XEXP(x, 1)),
						reg_names[REGNO(XEXP(x, 0))]
					);
					break;
				case SYMBOL_REF:
					output_addr_const(file, XEXP(x, 1));
					fprintf(file, "0(%s)", reg_names[REGNO(XEXP(x, 0))]);
					break;
				case CONST:
					{
						rtx plus = XEXP(XEXP(x, 1), 0);
						if(	GET_CODE(XEXP(plus, 0)) == SYMBOL_REF &&
							CONST_INT_P(XEXP(plus, 1))	)
						{
							output_addr_const(file, XEXP(plus, 0));
							fprintf(
								file,
								"+%ld(%s)",
								INTVAL(XEXP(plus, 1)),
								reg_names[REGNO(XEXP(x, 0))]
							);
						}
						else
							abort();
					}
					break;
				default:
					abort();
			}
			break;

		default:
			output_addr_const (file, x);
			break;
	}
}

/* The PRINT_OPERAND worker.  */

void
hip_print_operand (FILE *file, rtx x, int code)
{
	rtx operand = x;

	/* New code entries should just be added to the switch below.  If
	 handling is finished, just return.  If handling was just a
	 modification of the operand, the modified operand should be put in
	 "operand", and then do a break to let default handling
	 (zero-modifier) output the operand.  */

	switch(code)
	{
		case 0:
		  /* No code, print as usual.  */
		  break;

		default:
			LOSE_AND_RETURN("invalid operand modifier letter", x);
	}

	/* Print an operand as without a modifier letter.  */
	switch (GET_CODE (operand))
	{
		case REG:
			if(REGNO(operand) >= HIP_NUMBER_HARD_REGS)
				internal_error("internal error: bad register: %d", REGNO(operand));
			fprintf(file, "%s", reg_names[REGNO(operand)]);
			return;

		case MEM:
			output_address(XEXP(operand, 0));
			return;

		default:
			/* No need to handle all strange variants, let output_addr_const
			do it for us.  */
			if(CONSTANT_P(operand))
			{
				output_addr_const(file, operand);
				return;
			}
			LOSE_AND_RETURN("unexpected operand", x);
			break;
	}
}

/* Per-function machine data.  */
struct GTY(()) machine_function
 {
	 /* Number of bytes saved on the stack for callee saved registers.  */
	 int callee_saved_reg_size;

	 /* Number of bytes saved on the stack for local variables.  */
	 int local_vars_size;

	 /* The sum of 2 sizes: locals vars and padding byte for saving the
	* registers.  Used in expand_prologue () and expand_epilogue().  */
	 int size_for_adjusting_sp;
 };

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
hip_init_machine_status(void)
{
	return ggc_alloc_cleared_machine_function ();
}


/* The TARGET_OPTION_OVERRIDE worker.
	 All this curently does is set init_machine_status.  */
static void
hip_option_override(void)
{
	/* Set the per-function-data initializer.  */
	init_machine_status = hip_init_machine_status;
}

void
hip_push_register(int regno)
{
	rtx insn;

	insn = emit_insn(
		gen_movsi_push(
			gen_rtx_REG(Pmode, regno)
		)
	);
	RTX_FRAME_RELATED_P(insn) = 1;

	insn = emit_insn(gen_subsi3(stack_pointer_rtx, stack_pointer_rtx, GEN_INT(UNITS_PER_WORD)));
	RTX_FRAME_RELATED_P(insn) = 1;
}

void
hip_pop_register(int regno)
{
	rtx insn;

	insn = emit_insn(gen_addsi3(stack_pointer_rtx, stack_pointer_rtx, GEN_INT(UNITS_PER_WORD)));
	RTX_FRAME_RELATED_P(insn) = 1;

	rtx reg = gen_rtx_REG(Pmode, regno);
	insn = emit_insn(gen_movsi_pop(reg, reg));
	RTX_FRAME_RELATED_P(insn) = 1;
}

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  */

static void
hip_compute_frame(void)
{
	/* For aligning the local variables.  */
	int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
	int padding_locals;
	int regno;

	/* Padding needed for each element of the frame.  */
	cfun->machine->local_vars_size = get_frame_size ();

	/* Align to the stack alignment.  */
	padding_locals = cfun->machine->local_vars_size % stack_alignment;
	if(padding_locals)
		padding_locals = stack_alignment - padding_locals;

	cfun->machine->local_vars_size += padding_locals;

	cfun->machine->callee_saved_reg_size = 0;

	/* Save callee-saved registers.  */
	for(regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
		if(df_regs_ever_live_p(regno) && (! call_used_regs[regno]))
			cfun->machine->callee_saved_reg_size += 4;
	}

	cfun->machine->size_for_adjusting_sp =
		crtl->args.pretend_args_size	+
		cfun->machine->local_vars_size	+
		(ACCUMULATE_OUTGOING_ARGS ? crtl->outgoing_args_size : 0);
}

void
hip_expand_prologue(void)
{
	int regno;
	rtx insn;

	hip_compute_frame();

	/* Save callee-saved registers.  */
	for(regno = HIP_NUMBER_HARD_REGS - 1; regno >= 0; --regno)
	{
		if(
			!fixed_regs[regno] &&
			!call_used_regs[regno] &&
			df_regs_ever_live_p(regno)
		)
		{
			hip_push_register(regno);
		}
	}

	if(cfun->machine->callee_saved_reg_size != 0)
	{
		insn = emit_move_insn(hard_frame_pointer_rtx, stack_pointer_rtx);
		RTX_FRAME_RELATED_P(insn) = 1;
	}
}

void
hip_expand_epilogue(void)
{
	int regno;

	if(cfun->machine->callee_saved_reg_size != 0)
	{
		emit_move_insn(stack_pointer_rtx, hard_frame_pointer_rtx);

		for(regno = 0; regno < HIP_NUMBER_HARD_REGS; ++regno)
		{
			if(
				!fixed_regs[regno] &&
				!call_used_regs[regno] &&
				df_regs_ever_live_p(regno)
			)
			{
				hip_pop_register(regno);
			}
		}
	}

	emit_jump_insn(gen_returner());
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */

int
hip_initial_elimination_offset(int from, int to)
{
	int ret;

	if((from) == FRAME_POINTER_REGNUM && (to) == HARD_FRAME_POINTER_REGNUM)
	{
		/* Compute this since we need to use cfun->machine->local_vars_size.  */
		hip_compute_frame ();
		ret = -cfun->machine->callee_saved_reg_size;
	}
	else if((from) == ARG_POINTER_REGNUM && (to) == HARD_FRAME_POINTER_REGNUM)
		ret = 0x00;
	else
		abort();

	return ret;
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
hip_setup_incoming_varargs(
	CUMULATIVE_ARGS *cum,
	enum machine_mode mode ATTRIBUTE_UNUSED,
	tree type ATTRIBUTE_UNUSED,
	int *pretend_size, int no_rtl
)
{
	int regno;
	int regs = 8 - *cum;

	*pretend_size = regs < 0 ? 0 : GET_MODE_SIZE (SImode) * regs;

	if(no_rtl)
		return;

	fprintf(stderr, "TARGET_SETUP_INCOMING_VARARGS\n");

	for(regno = *cum; regno < 8; regno++)
	{
		rtx reg = gen_rtx_REG(SImode, regno);
		rtx slot = gen_rtx_PLUS(
			Pmode,
			gen_rtx_REG(SImode, ARG_POINTER_REGNUM),
			GEN_INT(UNITS_PER_WORD * (3 + (regno-2)))
		);

		emit_move_insn(gen_rtx_MEM(SImode, slot), reg);
	}
}


/*
	Return the next register to be used to hold a function argument or
	NULL_RTX if there's no more space.
*/
static rtx
hip_function_arg(CUMULATIVE_ARGS *cum, enum machine_mode mode,
			const_tree type ATTRIBUTE_UNUSED,
			bool named ATTRIBUTE_UNUSED)
{
	if(*cum <= HIP_P2)
	{
		return gen_rtx_REG(mode, *cum);
	}
	else
	{
		return NULL_RTX;
	}
}

#define HIP_FUNCTION_ARG_SIZE(MODE, TYPE)		\
	((MODE) != BLKmode							\
		? GET_MODE_SIZE(MODE)					\
		: (unsigned) int_size_in_bytes(TYPE))

/*
	Updates the summarizer variable pointed to by ca to advance past an
	argument in the argument list. The values mode, type and named describe that argument.
	Once this is done, the variable cum is suitable for analyzing the following
	argument with TARGET_FUNCTION_ARG, etc.
*/
static void
hip_function_arg_advance(
	CUMULATIVE_ARGS *cum,
	enum machine_mode mode,
	const_tree type,
	bool named ATTRIBUTE_UNUSED
)
{
	*cum = (
		*cum <= HIP_P2
			? *cum + ((3 + HIP_FUNCTION_ARG_SIZE(mode, type)) / 4)
			: *cum
	);
}

/* Return non-zero if the function argument described by TYPE is to be
	 passed by reference.  */

static bool
hip_pass_by_reference(
	CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
	enum machine_mode mode,
	const_tree type,
	bool named ATTRIBUTE_UNUSED
)
{
	unsigned HOST_WIDE_INT size;

	if(type)
	{
		if(AGGREGATE_TYPE_P (type))
			return true;
		size = int_size_in_bytes (type);
	}
	else
		size = GET_MODE_SIZE (mode);

	return size > UNITS_PER_WORD;
}

/* Some function arguments will only partially fit in the registers
	 that hold arguments.  Given a new arg, return the number of bytes
	 that fit in argument passing registers.  */

static int
hip_arg_partial_bytes(
	CUMULATIVE_ARGS *cum,
	enum machine_mode mode,
	tree type, bool named
)
{
	int bytes_left, size;

	if(*cum >= 8)
	return 0;

	if(hip_pass_by_reference (cum, mode, type, named))
		size = 4;
	else if(type)
	{
		if(AGGREGATE_TYPE_P (type))
			return 0;
		size = int_size_in_bytes (type);
	}
	else
		size = GET_MODE_SIZE (mode);

	bytes_left = (4 * 6) - ((*cum - 2) * 4);

	if(size > bytes_left)
		return bytes_left;
	else
		return 0;
}

/* Worker function for TARGET_STATIC_CHAIN.  */

static rtx
hip_static_chain(const_tree fndecl, bool incoming_p)
{
	rtx addr, mem;

	if(!DECL_STATIC_CHAIN (fndecl))
		return NULL;

	if(incoming_p)
		addr = plus_constant (arg_pointer_rtx, 2 * UNITS_PER_WORD);
	else
		addr = plus_constant (stack_pointer_rtx, -UNITS_PER_WORD);

	mem = gen_rtx_MEM (Pmode, addr);
	MEM_NOTRAP_P (mem) = 1;

	return mem;
}

/* Worker function for TARGET_ASM_TRAMPOLINE_TEMPLATE.  */

static void
hip_asm_trampoline_template (FILE *f)
{
	fprintf(f, "\tpush  sp, r0\n");
	fprintf(f, "\tldi.l r0, 0x0\n");
	fprintf(f, "\tsto.l 0x8(fp), r0\n");
	fprintf(f, "\tpop   sp, r0\n");
	fprintf(f, "\tnop\n");
	fprintf(f, "\tjmpa  0x0\n");
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
hip_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
	rtx mem, fnaddr = XEXP(DECL_RTL (fndecl), 0);

	emit_block_move(
		m_tramp,
		assemble_trampoline_template(),
		GEN_INT(TRAMPOLINE_SIZE),
		BLOCK_OP_NORMAL
	);

	mem = adjust_address (m_tramp, SImode, 4);
	emit_move_insn (mem, chain_value);
	mem = adjust_address (m_tramp, SImode, 20);
	emit_move_insn (mem, fnaddr);
}

/* The Global `targetm' Variable.  */

/* Initialize the GCC target structure.  */

#undef	TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES	hook_bool_const_tree_true

#undef	TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY		hip_return_in_memory
#undef	TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK	must_pass_in_stack_var_size
#undef	TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE	hip_pass_by_reference
#undef	TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES	hip_arg_partial_bytes
#undef	TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG			hip_function_arg
#undef	TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE	hip_function_arg_advance

#undef	TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS 	hip_setup_incoming_varargs

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START hip_file_start


/* Define this to return an RTX representing the place where a
	 function returns or receives a value of data type RET_TYPE, a tree
	 node node representing a data type.  */
#undef	TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE hip_function_value
#undef	TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE hip_libcall_value
#undef	TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P hip_function_value_regno_p

#undef	TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED hook_bool_void_true

#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER hip_assemble_integer

#undef	TARGET_STATIC_CHAIN
#define TARGET_STATIC_CHAIN hip_static_chain
#undef	TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE hip_asm_trampoline_template
#undef	TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT hip_trampoline_init

#undef	TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE hip_option_override

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-hip.h"
