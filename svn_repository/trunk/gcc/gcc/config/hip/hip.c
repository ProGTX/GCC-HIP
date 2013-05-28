/* Definitions of target machine for GNU compiler, for HIP.
 Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
 2010
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

#include <stdio.h>
#include <string.h>

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "hashtab.h"
#include "insn-config.h"
#include "output.h"
#include "basic-block.h"
#include "flags.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "diagnostic-core.h"
#include "recog.h"
#include "ggc.h"
#include "dwarf2.h"
#include "debug.h"
#include "tm_p.h"
#include "integrate.h"
#include "target.h"
#include "target-def.h"
#include "df.h"

#ifdef __CDT_PARSER__
#include "hip.h"
#endif

/* First some local helper definitions.  */
#define HIP_FIRST_GLOBAL_REGNUM 1

/* We'd need a current_function_has_landing_pad.  It's marked as such when
 a nonlocal_goto_receiver is expanded.  Not just a C++ thing, but
 mostly.  */
#define HIP_CFUN_HAS_LANDING_PAD (cfun->machine->has_landing_pad != 0)

/* We have no means to tell DWARF 2 about the register stack, so we need
 to store the return address on the stack if an exception can get into
 this function.  FIXME: Narrow condition.  Before any whole-function
 analysis, df_regs_ever_live_p () isn't initialized.  We know it's up-to-date
 after reload_completed; it may contain incorrect information some time
 before that.  Within a RTL sequence (after a call to start_sequence,
 such as in RTL expanders), leaf_function_p doesn't see all insns
 (perhaps any insn).  But regs_ever_live is up-to-date when
 leaf_function_p () isn't, so we "or" them together to get accurate
 information.  FIXME: Some tweak to leaf_function_p might be
 preferable.  */
#define HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS			\
 (flag_exceptions						\
  && ((reload_completed /*&& df_regs_ever_live_p (HIP_rJ_REGNUM)*/)	\
      || !leaf_function_p ()))

#define IS_HIP_EH_RETURN_DATA_REG(REGNO)	\
 (crtl->calls_eh_return		\
  && (EH_RETURN_DATA_REGNO (0) == REGNO		\
      || EH_RETURN_DATA_REGNO (1) == REGNO	\
      || EH_RETURN_DATA_REGNO (2) == REGNO	\
      || EH_RETURN_DATA_REGNO (3) == REGNO))

/* For the default ABI, we rename registers at output-time to fill the gap
 between the (statically partitioned) saved registers and call-clobbered
 registers.  In effect this makes unused call-saved registers to be used
 as call-clobbered registers.  The benefit comes from keeping the number
 of local registers (value of rL) low, since there's a cost of
 increasing rL and clearing unused (unset) registers with lower numbers.
 Don't translate while outputting the prologue.  */
#define HIP_OUTPUT_REGNO(N)					\
 ( 						\
  (int) (N) < HIP_RETURN_VALUE_REGNUM			\
  || (int) (N) > HIP_NUMBER_OF_REGISTERS		\
  || cfun == NULL 						\
  || cfun->machine == NULL 					\
  || cfun->machine->in_prologue					\
  ? (N) : ((N) - HIP_RETURN_VALUE_REGNUM			\
	   + cfun->machine->highest_saved_stack_register + 1))

/* The %d in "POP %d,0".  */
#define HIP_POP_ARGUMENT()						\
 ((							\
   crtl->return_rtx != NULL				\
   && ! cfun->returns_struct)				\
  ? (GET_CODE (crtl->return_rtx) == PARALLEL			\
     ? GET_NUM_ELEM (XVEC (crtl->return_rtx, 0)) : 1)	\
  : 0)

/* The canonical saved comparison operands for non-cc0 machines, set in
 the compare expander.  */
rtx hip_compare_op0;
rtx hip_compare_op1;

/* Declarations of locals.  */

/* Intermediate for insn output.  */
static int hip_output_destination_register;

static void hip_option_override(void);
static void hip_asm_output_source_filename(FILE *, const char *);
static void hip_output_shiftvalue_op_from_str(FILE *, const char *,
		HOST_WIDEST_INT);
static void hip_output_shifted_value(FILE *, HOST_WIDEST_INT);
static void hip_output_condition(FILE *, rtx, int);
static HOST_WIDEST_INT hip_intval(rtx);
static void hip_output_octa(FILE *, HOST_WIDEST_INT, int);
static bool hip_assemble_integer(rtx, unsigned int, int);
static struct machine_function *hip_init_machine_status(void);
static void hip_encode_section_info(tree, rtx, int);
static const char *hip_strip_name_encoding(const char *);
static void hip_emit_sp_add(HOST_WIDE_INT offset);
static void hip_target_asm_function_prologue(FILE *, HOST_WIDE_INT);
static void hip_target_asm_function_end_prologue(FILE *);
static void hip_target_asm_function_epilogue(FILE *, HOST_WIDE_INT);
static bool hip_legitimate_address_p(enum machine_mode, rtx, bool);
static void hip_reorg(void);
static void hip_asm_output_mi_thunk(FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT,
		tree);
static void hip_setup_incoming_varargs(CUMULATIVE_ARGS *, enum machine_mode,
		tree, int *, int);
static void hip_file_start(void);
static void hip_file_end(void);
static bool hip_rtx_costs(rtx, int, int, int *, bool);
static rtx hip_struct_value_rtx(tree, int);
static enum machine_mode hip_promote_function_mode(const_tree,
		enum machine_mode, int *, const_tree, int);
static void hip_function_arg_advance(CUMULATIVE_ARGS *, enum machine_mode,
		const_tree, bool);
static rtx hip_function_arg_1(const CUMULATIVE_ARGS *, enum machine_mode,
		const_tree, bool, bool);
static rtx hip_function_incoming_arg(CUMULATIVE_ARGS *, enum machine_mode,
		const_tree, bool);
static rtx hip_function_arg(CUMULATIVE_ARGS *, enum machine_mode, const_tree,
		bool);
static rtx hip_function_value(const_tree, const_tree, bool);
static rtx hip_libcall_value(enum machine_mode, const_rtx);
static bool hip_function_value_regno_p(const unsigned int);
static bool hip_pass_by_reference(CUMULATIVE_ARGS *, enum machine_mode,
		const_tree, bool);
static bool hip_frame_pointer_required(void);
static void hip_asm_trampoline_template(FILE *);
static void hip_trampoline_init(rtx, tree, rtx);
static void hip_conditional_register_usage(void);

/* TARGET_OPTION_OPTIMIZATION_TABLE.  */

static const struct default_options hip_option_optimization_table[] =
{
	{ OPT_LEVELS_1_PLUS, OPT_fregmove, NULL, 1 },
	{ OPT_LEVELS_2_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
	{ OPT_LEVELS_NONE, 0, NULL, 0 }
};

/* Target structure macros.  Listed by node.  See `Using and Porting GCC'
 for a general description.  */

/* Node: Function Entry */

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP NULL
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP NULL
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP NULL
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER hip_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE hip_target_asm_function_prologue

#undef TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE hip_target_asm_function_end_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE hip_target_asm_function_epilogue

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO  hip_encode_section_info
#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING  hip_strip_name_encoding

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK hip_asm_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall
#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START hip_file_start
#undef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END hip_file_end
#undef TARGET_ASM_OUTPUT_SOURCE_FILENAME
#define TARGET_ASM_OUTPUT_SOURCE_FILENAME hip_asm_output_source_filename

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE hip_conditional_register_usage

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS hip_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_bool_0

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG hip_reorg

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE hip_promote_function_mode

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE hip_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE hip_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P hip_function_value_regno_p

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG hip_function_arg
#undef TARGET_FUNCTION_INCOMING_ARG
#define TARGET_FUNCTION_INCOMING_ARG hip_function_incoming_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE hip_function_arg_advance
#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX hip_struct_value_rtx
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS hip_setup_incoming_varargs
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE hip_pass_by_reference
#undef TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES hook_bool_CUMULATIVE_ARGS_mode_tree_bool_true
#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS TARGET_DEFAULT

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	hip_legitimate_address_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED hip_frame_pointer_required

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE hip_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT hip_trampoline_init

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE hip_option_override
#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE hip_option_optimization_table



/* Strictly debugging */

/*#define HIP_DEBUG*/

static void HIP_WARNING(const char* STRING)
{
#ifdef HIP_DEBUG
	warning(0, STRING);
#endif
}

static void HIP_PRINT_INT(int INTEGER)
{
#ifdef HIP_DEBUG
	char __hip_str[15];
	sprintf(__hip_str, "%d", INTEGER);
	HIP_WARNING(__hip_str);
#endif
}


#define HIP_FUNCTION_ENTRY() \
	HIP_WARNING(__FUNCTION__)


struct gcc_target targetm = TARGET_INITIALIZER;

/* Functions that are expansions for target macros.
 See Target Macros in `Using and Porting GCC'.  */

/* TARGET_OPTION_OVERRIDE.  */

static void hip_option_override(void)
{
	HIP_FUNCTION_ENTRY();
	/* Should we err or should we warn?  Hmm.  At least we must neutralize
	 it.  For example the wrong kind of case-tables will be generated with
	 PIC; we use absolute address items for hipal compatibility.  FIXME:
	 They could be relative if we just elide them to after all pertinent
	 labels.  */
	if(flag_pic)
	{
		warning(
			0,
			"-f%s not supported: ignored",
			(flag_pic > 1) ? "PIC" : "pic"
		);
		flag_pic = 0;
	}

}

/* INIT_EXPANDERS.  */

void hip_init_expanders(void)
{
	HIP_FUNCTION_ENTRY();
	init_machine_status = hip_init_machine_status;

}

/*
	Set the per-function data.
*/
static struct machine_function * hip_init_machine_status(void)
{
	HIP_FUNCTION_ENTRY();
	return ggc_alloc_cleared_machine_function();

}

/*	DATA_ALIGNMENT
	We have trouble getting the address of stuff that is located at other
	than 32-bit alignments (GETA requirements), so try to give everything
	at least 32-bit alignment.
*/
int hip_data_alignment(tree type ATTRIBUTE_UNUSED, int basic_align)
{
	HIP_FUNCTION_ENTRY();
	if(basic_align < HIP_WORD_SIZE)
		return HIP_WORD_SIZE;
	return basic_align;
}

/* CONSTANT_ALIGNMENT.  */
int hip_constant_alignment(tree constant ATTRIBUTE_UNUSED, int basic_align)
{
	HIP_FUNCTION_ENTRY();
	if(basic_align < HIP_WORD_SIZE)
		return HIP_WORD_SIZE;
	return basic_align;
}

/* LOCAL_ALIGNMENT.  */
unsigned hip_local_alignment(tree type ATTRIBUTE_UNUSED, unsigned basic_align)
{
	HIP_FUNCTION_ENTRY();
	if(basic_align < HIP_WORD_SIZE)
		return HIP_WORD_SIZE;
	return basic_align;
}

/* TARGET_CONDITIONAL_REGISTER_USAGE.  */

static void hip_conditional_register_usage(void)
{
	HIP_FUNCTION_ENTRY();
	int i;

	/* Step over the ":" in special register names.  */
	if(!TARGET_TOPLEVEL_SYMBOLS)
		for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
			if(reg_names[i][0] == ':')
				reg_names[i]++;

}

/*
	TODO: Only for architectures with register windows.
*/
int hip_opposite_regno(int regno, int incoming)
{
	HIP_FUNCTION_ENTRY();
	return regno;
}

/*
	TODO: Only for architectures with register windows.
*/
int hip_local_regno(int regno)
{
	HIP_FUNCTION_ENTRY();
	return regno <= HIP_NUMBER_OF_REGISTERS && !call_used_regs[regno];
}

/* PREFERRED_RELOAD_CLASS.
*/

enum reg_class hip_preferred_reload_class(
	rtx x ATTRIBUTE_UNUSED, enum reg_class rclass
)
{
	/* FIXME: Revisit.  */
	return rclass;
}

/* PREFERRED_OUTPUT_RELOAD_CLASS.*/

enum reg_class hip_preferred_output_reload_class(
	rtx x ATTRIBUTE_UNUSED, enum reg_class rclass
)
{
	/* FIXME: Revisit.  */
	return rclass;
}

/* SECONDARY_RELOAD_CLASS.
 We need to reload regs of REMAINDER_REG and HIMULT_REG elsewhere.  */

enum reg_class hip_secondary_reload_class(
	enum reg_class rclass,
	enum machine_mode mode ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED,
	int in_p ATTRIBUTE_UNUSED
)
{
	return NO_REGS;
}

/*
	CONST_OK_FOR_LETTER_P.
	TODO: Not done, need to connect to .md file.
	MIPS -> http://gcc.gnu.org/onlinedocs/gcc/Machine-Constraints.html
*/

int hip_const_ok_for_letter_p(HOST_WIDE_INT value, int c)
{
	HIP_FUNCTION_ENTRY();
	switch(c)
	{
		case 'I':
			/* A signed 16-bit constant (for arithmetic instructions). */
			return value >= -32768 && value <= 32767;
		case 'J':
			/* Integer zero. */
			return value == 0;
		case 'K':
			/* An unsigned 16-bit constant. */
			return value >= 0 && value <= 65535;
		case 'L':
			/*	A signed 32-bit constant in which the lower 16 bits are zero.
				Such constants can be loaded using lhi.
				TODO: Check. */
			return (value & 255) == 0;
			/*return hip_shiftable_wyde_value(value);*/
		/*case 'M':
			TODO: What is this supposed to be?
			/* A constant that cannot be loaded using lhi, addiu.
			/*return hip_shiftable_wyde_value(~value);*/
		case 'N':
			/* Valid register number. */
			return value >= 0 && value <= 31;
		case 'P':
			/* Temporary. */
			return hip_shiftable_wyde_value(value);
		case 'G':
			/* An unsigned 8-bit constant. */
			return value >= 0 && value <= 255;
		default:
			return 0;
	}
}

/*
	CONST_DOUBLE_OK_FOR_LETTER_P.
	TODO: Remove.
*/

int hip_const_double_ok_for_letter_p(rtx value, int c)
{
	HIP_FUNCTION_ENTRY();
	return 0;
}

/*
	TODO: Not really needed?
*/
int hip_extra_constraint(rtx x, int c, int strict)
{
	HIP_FUNCTION_ENTRY();
	return hip_intval(x);
}

/*
	DYNAMIC_CHAIN_ADDRESS.
	Dynamic link, location of the previous frame pointer.
*/

rtx hip_dynamic_chain_address(rtx frame)
{
	HIP_FUNCTION_ENTRY();
	/* FIXME: the frame-pointer is stored at offset -4 from the current
	 frame-pointer.  Unfortunately, the caller assumes that a
	 frame-pointer is present for *all* previous frames.  There should be
	 a way to say that that cannot be done, like for RETURN_ADDR_RTX.  */
	return plus_constant(frame, -UNITS_PER_WORD);
}

/*
	TODO: First parameter is 12 bytes away from FP.
*/
int hip_starting_frame_offset(void)
{
	HIP_FUNCTION_ENTRY();
	return UNITS_PER_WORD * 3;
}

/* RETURN_ADDR_RTX.  */

rtx hip_return_addr_rtx(int count, rtx frame ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	/*
		FIXME: Set frame_alias_set on the following.  (Why?)
		See hip_initial_elimination_offset for the reason we can't use
		get_hard_reg_initial_val for both.  Always using a stack slot
		and not a register would be suboptimal.
	*/

	return (
		count == 0 ?
			(
			HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS ?
				validize_mem(
					gen_rtx_MEM(Pmode, plus_constant(frame_pointer_rtx, -UNITS_PER_WORD))
				) :
				get_hard_reg_initial_val(Pmode, HIP_INCOMING_RETURN_ADDRESS_REGNUM)
			) :
			NULL_RTX
	);
}

/*
	SETUP_FRAME_ADDRESSES.
*/
void hip_setup_frame_addresses(void)
{
	HIP_FUNCTION_ENTRY();
	/* Nothing needed at the moment.  */
}

/* The difference between the (imaginary) frame pointer and the stack
 pointer.  Used to eliminate the frame pointer.  */

int hip_initial_elimination_offset(int fromreg, int toreg)
{
	HIP_FUNCTION_ENTRY();
	int regno;

	/* TODO: WTF is this? */
	int fp_sp_offset = (get_frame_size() + crtl->outgoing_args_size + 7) & ~7;

	/* There is no actual offset between these two virtual values, but for
	 the frame-pointer, we have the old one in the stack position below
	 it, so the offset for the frame-pointer to the stack-pointer is one
	 octabyte larger.  */
	if(fromreg == HIP_ARG_POINTER_REGNUM && toreg == HIP_FRAME_POINTER_REGNUM)
		return 0;

	/* The difference is the size of local variables plus the size of
	 outgoing function arguments that would normally be passed as
	 registers but must be passed on stack because we're out of
	 function-argument registers.  Only global saved registers are
	 counted; the others go on the register stack.

	 The frame-pointer is counted too if it is what is eliminated, as we
	 need to balance the offset for it from STARTING_FRAME_OFFSET.

	 Also add in the slot for the register stack pointer we save if we
	 have a landing pad.

	 Unfortunately, we can't access r0..r14, from unwinder code easily, so
	 store the return address in a frame slot too.  FIXME: Only for
	 non-leaf functions.  FIXME: Always with a landing pad, because it's
	 hard to know whether we need the other at the time we know we need
	 the offset for one (and have to state it).  It's a kludge until we
	 can express the register stack in the EH frame info.

	 We have to do alignment here; get_frame_size will not return a
	 multiple of STACK_BOUNDARY.  FIXME: Add note in manual.  */

	for (regno = HIP_FIRST_GLOBAL_REGNUM; regno <= 31; regno++)
		if((df_regs_ever_live_p(regno) && !call_used_regs[regno])
				|| IS_HIP_EH_RETURN_DATA_REG (regno))
			fp_sp_offset += 2 * UNITS_PER_WORD;

	return fp_sp_offset +
		(
			HIP_CFUN_HAS_LANDING_PAD ?
				4 * UNITS_PER_WORD :
				(HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS ? 2 * UNITS_PER_WORD : 0)
		)
		+ (fromreg == HIP_ARG_POINTER_REGNUM ? 0 : 2 * UNITS_PER_WORD);

}

static void hip_function_arg_advance(CUMULATIVE_ARGS *argsp,
		enum machine_mode mode, const_tree type, bool named ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	int arg_size = HIP_FUNCTION_ARG_SIZE(mode, type);

	argsp->regs = (
		(
			targetm.calls.must_pass_in_stack(mode, type)
			||
			(arg_size > 2 * UNITS_PER_WORD && !TARGET_LIBFUNC && !argsp->lib)) ?
				(HIP_MAX_ARGS_IN_REGS) + 1 :
				argsp->regs + ((2 * UNITS_PER_WORD - 1) + arg_size) / (2 * UNITS_PER_WORD)
	);

}

/* Helper function for hip_function_arg and hip_function_incoming_arg.  */

static rtx hip_function_arg_1(const CUMULATIVE_ARGS *argsp,
		enum machine_mode mode, const_tree type, bool named ATTRIBUTE_UNUSED,
		bool incoming)
{
	HIP_FUNCTION_ENTRY();
	/* Last-argument marker.  */
	if(type == void_type_node)
	{
		return
			(argsp->regs < HIP_MAX_ARGS_IN_REGS) ?
				gen_rtx_REG(
					mode,
					(incoming ? HIP_FIRST_ARG_REGNUM : HIP_FIRST_ARG_REGNUM) + argsp->regs
				) :
				NULL_RTX;
	}

	return(
		(
			argsp->regs < HIP_MAX_ARGS_IN_REGS
			&& !targetm.calls.must_pass_in_stack(mode, type)
			&& (GET_MODE_BITSIZE (mode) <= HIP_WORD_SIZE || argsp->lib || TARGET_LIBFUNC)
		) ?
			gen_rtx_REG(
				mode,
				(incoming ? HIP_FIRST_ARG_REGNUM : HIP_FIRST_ARG_REGNUM) + argsp->regs
			) :
			NULL_RTX
	);

}

	/* Return an rtx for a function argument to go in a register, and 0 for
	 one that must go on stack.  */

static rtx hip_function_arg(CUMULATIVE_ARGS *argsp, enum machine_mode mode,
		const_tree type, bool named)
{
	HIP_FUNCTION_ENTRY();
	return hip_function_arg_1(argsp, mode, type, named, false);

}

static rtx hip_function_incoming_arg(CUMULATIVE_ARGS *argsp,
		enum machine_mode mode, const_tree type, bool named)
{
	HIP_FUNCTION_ENTRY();
	return hip_function_arg_1(argsp, mode, type, named, true);

}

/* Returns nonzero for everything that goes by reference, 0 for
 everything that goes by value.  */

static bool hip_pass_by_reference(
	CUMULATIVE_ARGS *argsp, enum machine_mode mode, const_tree type, bool named ATTRIBUTE_UNUSED
)
{
	HIP_FUNCTION_ENTRY();
	/* FIXME: Check: I'm not sure the must_pass_in_stack check is
	 necessary.  */
	if(targetm.calls.must_pass_in_stack(mode, type))
		return true;

	if(HIP_FUNCTION_ARG_SIZE(mode, type) > 2 * UNITS_PER_WORD && !TARGET_LIBFUNC
			&& (!argsp || !argsp->lib))
		return true;

	return false;

}

/* Return nonzero if regno is a register number where a parameter is
 passed, and 0 otherwise.  */

int hip_function_arg_regno_p(int regno, int incoming)
{
	HIP_FUNCTION_ENTRY();

	return
		regno >= HIP_FIRST_ARG_REGNUM &&
		regno < HIP_FIRST_ARG_REGNUM + HIP_MAX_ARGS_IN_REGS;

}

/* Implements TARGET_FUNCTION_VALUE.  */

static rtx hip_function_value(const_tree valtype,
		const_tree func ATTRIBUTE_UNUSED, bool outgoing)
{
	HIP_FUNCTION_ENTRY();
	enum machine_mode mode = TYPE_MODE (valtype);
	enum machine_mode cmode;
	int first_val_regnum = HIP_RETURN_VALUE_REGNUM;
	rtx vec[HIP_MAX_REGS_FOR_VALUE];
	int i;
	int nregs;

	if(!outgoing)
		return gen_rtx_REG(mode, HIP_RETURN_VALUE_REGNUM);

	/* Return values that fit in a register need no special handling.
	 There's no register hole when parameters are passed in global
	 registers.  */
	if(GET_MODE_BITSIZE (mode) <= BITS_PER_WORD)
		return gen_rtx_REG(mode, HIP_RETURN_VALUE_REGNUM);

	if(COMPLEX_MODE_P (mode))
		/* A complex type, made up of components.  */
		cmode = TYPE_MODE (TREE_TYPE (valtype));
	else
	{
		/* Of the other larger-than-register modes, we only support
		 scalar mode TImode.  (At least, that's the only one that's
		 been rudimentally tested.)  Make sure we're alerted for
		 unexpected cases.  */
		if(mode != TImode)
			sorry("support for mode %qs", GET_MODE_NAME (mode));

			/* In any case, we will fill registers to the natural size.  */
			cmode = DImode;
		}

	nregs = ((GET_MODE_BITSIZE (mode) + BITS_PER_WORD - 1) / BITS_PER_WORD);

	/* We need to take care of the effect of the register hole on return
	 values of large sizes; the last register will appear as the first
	 register, with the rest shifted.  (For complex modes, this is just
	 swapped registers.)  */

	if(nregs > HIP_MAX_REGS_FOR_VALUE)
		internal_error(
				"too large function value type, needs %d registers,\
 have only %d registers for this",
				nregs, HIP_MAX_REGS_FOR_VALUE);

	/* FIXME: Maybe we should handle structure values like this too
	 (adjusted for BLKmode), perhaps for both ABI:s.  */
	for (i = 0; i < nregs - 1; i++)
		vec[i] = gen_rtx_EXPR_LIST(VOIDmode,
				gen_rtx_REG(cmode, first_val_regnum + i),
				GEN_INT ((i + 1) * BITS_PER_UNIT));

	vec[nregs - 1] = gen_rtx_EXPR_LIST(VOIDmode,
			gen_rtx_REG(cmode, first_val_regnum + nregs - 1), const0_rtx);

	return gen_rtx_PARALLEL(VOIDmode, gen_rtvec_v(nregs, vec));

}

/* Implements TARGET_LIBCALL_VALUE.  */

static rtx hip_libcall_value(enum machine_mode mode,
		const_rtx fun ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	return gen_rtx_REG(mode, HIP_RETURN_VALUE_REGNUM);

}

/* Implements TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool hip_function_value_regno_p(const unsigned int regno)
{
	HIP_FUNCTION_ENTRY();
	return regno == HIP_RETURN_VALUE_REGNUM;

}

/* EH_RETURN_DATA_REGNO. */

int hip_eh_return_data_regno(int n)
{
	HIP_FUNCTION_ENTRY();
	if(n >= 0 && n < 4)
		return HIP_EH_RETURN_DATA_REGNO_START + n;

	return INVALID_REGNUM ;

}

/* EH_RETURN_STACKADJ_RTX. */

rtx hip_eh_return_stackadj_rtx(void)
{
	HIP_FUNCTION_ENTRY();
	return gen_rtx_REG(Pmode, HIP_EH_RETURN_STACKADJ_REGNUM);

}

/* EH_RETURN_HANDLER_RTX.  */

rtx hip_eh_return_handler_rtx(void)
{
	HIP_FUNCTION_ENTRY();
	return gen_rtx_REG(Pmode, HIP_INCOMING_RETURN_ADDRESS_REGNUM);

}

/* ASM_PREFERRED_EH_DATA_FORMAT. */

int hip_asm_preferred_eh_data_format(int code ATTRIBUTE_UNUSED,
		int global ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	/* This is the default (was at 2001-07-20).  Revisit when needed.  */
	return DW_EH_PE_absptr;

}

/*
	Make a note that we've seen the beginning of the prologue.
	This matters to whether we'll translate register numbers as calculated by hip_reorg.
	Outputs the assembler code for entry to a function. The prologue is responsible for
	setting up the stack frame, initializing the frame pointer register,
	saving registers that must be saved, and allocating framesize additional bytes of
	storage for the local variables.
*/
static void hip_target_asm_function_prologue(
	FILE *stream ATTRIBUTE_UNUSED, HOST_WIDE_INT framesize ATTRIBUTE_UNUSED
)
{
	HIP_FUNCTION_ENTRY();
	cfun->machine->in_prologue = 1;

}

/*
	Make a note that we've seen the end of the prologue.
	Outputs assembler code at the end of a prologue.
*/
static void hip_target_asm_function_end_prologue(FILE *stream ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	cfun->machine->in_prologue = 0;

}

/* Implement TARGET_MACHINE_DEPENDENT_REORG.  No actual rearrangements
 done here; just virtually by calculating the highest saved stack
 register number used to modify the register numbers at output time.  */

static void hip_reorg(void)
{
	HIP_FUNCTION_ENTRY();
	int regno;

	/* We put the number of the highest saved register-file register in a
	 location convenient for the call-patterns to output.  Note that we
	 don't tell dwarf2 about these registers, since it can't restore them
	 anyway.  */
	for (regno = HIP_NUMBER_OF_REGISTERS; regno >= 0; regno--)
		if((df_regs_ever_live_p(regno) && !call_used_regs[regno])
				|| (regno == HIP_FRAME_POINTER_REGNUM && frame_pointer_needed))
			break;

	/* Regardless of whether they're saved (they might be just read), we
	 mustn't include registers that carry parameters.  We could scan the
	 insns to see whether they're actually used (and indeed do other less
	 trivial register usage analysis and transformations), but it seems
	 wasteful to optimize for unused parameter registers.  As of
	 2002-04-30, df_regs_ever_live_p (n) seems to be set for only-reads too, but
	 that might change.  */
	if(regno < crtl->args.info.regs - 1)
	{
		regno = crtl->args.info.regs - 1;

		/* We don't want to let this cause us to go over the limit and make
		 incoming parameter registers be misnumbered and treating the last
		 parameter register and incoming return value register call-saved.
		 Stop things at the unmodified scheme.  */
		if(regno > HIP_RETURN_VALUE_REGNUM - 1)
			regno = HIP_RETURN_VALUE_REGNUM - 1;
	}

	cfun->machine->highest_saved_stack_register = regno;

}

/*
	TARGET_ASM_FUNCTION_EPILOGUE.
	Outputs assembler code at the start of an epilogue.
*/
static void hip_target_asm_function_epilogue(FILE *stream,
		HOST_WIDE_INT locals_size ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	/* Emit an \n for readability of the generated assembly.  */
	fputc('\n', stream);

}

/* TARGET_ASM_OUTPUT_MI_THUNK.  */

static void hip_asm_output_mi_thunk(FILE *stream, tree fndecl ATTRIBUTE_UNUSED,
		HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
		tree func)
{
	HIP_FUNCTION_ENTRY();
	/* If you define TARGET_STRUCT_VALUE_RTX that returns 0 (i.e. pass
	 location of structure to return as invisible first argument), you
	 need to tweak this code too.  */
	const char *regname = reg_names[HIP_FIRST_ARG_REGNUM];

	if(delta >= 0 && delta < 65536)
		fprintf(stream, "\tINCL %s,%d\n", regname, (int) delta);
	else if(delta < 0 && delta >= -31)
		fprintf(stream, "\tsubu %s,%s,%d\n", regname, regname, (int) -delta);
	else
	{
		hip_output_register_setting(stream, 31, delta, 1);
		fprintf(stream, "\taddu %s,%s,r31\n", regname, regname);
	}

	fprintf(stream, "\tJMP ");
	assemble_name(stream, XSTR (XEXP (DECL_RTL (func), 0), 0));
	fprintf(stream, "\n");

}

/*
	FUNCTION_PROFILER.
	TODO: This could be useful.
*/
void hip_function_profiler(
	FILE *stream ATTRIBUTE_UNUSED, int labelno ATTRIBUTE_UNUSED
)
{
	HIP_FUNCTION_ENTRY();
	sorry("function_profiler support for HIP");

}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  For the moment,
 let's stick to pushing argument registers on the stack.  Later, we
 can parse all arguments in registers, to improve performance.  */

static void hip_setup_incoming_varargs(CUMULATIVE_ARGS *args_so_farp,
		enum machine_mode mode, tree vartype, int *pretend_sizep,
		int second_time ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	/* The last named variable has been handled, but
	 args_so_farp has not been advanced for it.  */
	if(args_so_farp->regs + 1 < HIP_MAX_ARGS_IN_REGS)
		*pretend_sizep = (HIP_MAX_ARGS_IN_REGS - (args_so_farp->regs + 1)) * UNITS_PER_WORD;

	/* We assume that one argument takes up one register here.  That should
	 be true until we start messing with multi-reg parameters.  */
	if((7 + (HIP_FUNCTION_ARG_SIZE(mode, vartype))) / UNITS_PER_WORD != 1)
		internal_error(
				"HIP Internal: Last named vararg would not fit in a register");

}

/*
	TODO: Probably not needed.
	TARGET_ASM_TRAMPOLINE_TEMPLATE.
*/

static void hip_asm_trampoline_template(FILE *stream)
{
	HIP_FUNCTION_ENTRY();
	/* Read a value into the static-chain register and jump somewhere.  The
	 static chain is stored at offset 16, and the function address is
	 stored at offset 24.  */

	fprintf(stream, "\tGETA r31,1F\n\t");
	fprintf(stream, "LDOU %s,r31,0\n\t", reg_names[HIP_STATIC_CHAIN_REGNUM]);
	fprintf(stream, "LDOU r31,r31,4\n\t");
	fprintf(stream, "GO r31,r31,0\n");
	fprintf(stream, "1H\tOCTA 0\n\t");
	fprintf(stream, "OCTA 0\n");

}

/* TARGET_TRAMPOLINE_INIT.  */
/* Set the static chain and function pointer field in the trampoline.
 We also SYNCID here to be sure (doesn't matter in the simulator, but
 some day it will).  */

static void hip_trampoline_init(rtx m_tramp, tree fndecl, rtx static_chain)
{
	HIP_FUNCTION_ENTRY();
	rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
	rtx mem;

	emit_block_move (m_tramp, assemble_trampoline_template (),
			GEN_INT (2*UNITS_PER_WORD), BLOCK_OP_NORMAL);

	mem = adjust_address (m_tramp, DImode, 2*UNITS_PER_WORD);
	emit_move_insn (mem, static_chain);
	mem = adjust_address (m_tramp, DImode, 3*UNITS_PER_WORD);
	emit_move_insn (mem, fnaddr);

	mem = adjust_address (m_tramp, DImode, 0);
	emit_insn (gen_sync_icache (mem, GEN_INT (TRAMPOLINE_SIZE - 1)));

}

/*
	TODO: Need to see which expressions are valid constant addresses.
	We must exclude constant addresses that have an increment that is not a
	multiple of four bytes because of restrictions of the GETA
	instruction, unless TARGET_BASE_ADDRESSES.
*/
int hip_constant_address_p(rtx x)
{
	HIP_FUNCTION_ENTRY();
	RTX_CODE code = GET_CODE (x);
	int addend = 0;
	/* When using "base addresses", anything constant goes.  */
	int constant_ok = TARGET_BASE_ADDRESSES != 0;

	switch (code)
	{
		case LABEL_REF:
		case SYMBOL_REF:
			return 1;

		case HIGH:
			/* FIXME: Don't know how to dissect these.  Avoid them for now,
			 except we know they're constants.  */
			return constant_ok;

		case CONST_INT:
			addend = INTVAL (x);
			warning(0, "Constant address: %d", addend);
			break;

		case CONST:
			if(GET_CODE (XEXP (x, 0)) == PLUS)
			{
				rtx x0 = XEXP (XEXP (x, 0), 0);
				rtx x1 = XEXP (XEXP (x, 0), 1);

				if(
					(GET_CODE (x0) == SYMBOL_REF || GET_CODE (x0) == LABEL_REF)
					&&
					(
						GET_CODE (x1) == CONST_INT ||
						(GET_CODE (x1) == CONST_DOUBLE && GET_MODE (x1) == VOIDmode)
					)
				)
					addend = hip_intval (x1);
				else
					return constant_ok;
			}
			else
				return constant_ok;
			break;

		default:
			return 0;
	}

	return constant_ok || (addend & 3) == 0;

}

/*
	TODO: Not done.
	Returns whether x (an RTX) is a legitimate memory address on the target machine
	for a memory operand of mode mode.
	Legitimate addresses are defined in two variants: a strict variant and a non-strict one.
	The strict parameter chooses which variant is desired by the caller.
	The strict variant is used in the reload pass. It must be defined so that any pseudoregister
	that has not been allocated a hard register is considered a memory reference.
*/
bool hip_legitimate_address_p(
	enum machine_mode mode ATTRIBUTE_UNUSED, rtx x, bool strict_checking
)
{
	HIP_FUNCTION_ENTRY();

#define HIP_REG_OK(X)												\
(																	\
	(																\
		strict_checking	&&											\
		(															\
			REGNO(X) <= HIP_LAST_GENERAL_REGISTER ||				\
			(														\
				reg_renumber[REGNO(X)] > 0 &&						\
				reg_renumber[REGNO(X)] <= HIP_LAST_GENERAL_REGISTER	\
			)														\
		)															\
	)																\
	||																\
	(																\
		!strict_checking &&											\
		(															\
			REGNO(X) <= HIP_LAST_GENERAL_REGISTER ||				\
			REGNO(X) >= FIRST_PSEUDO_REGISTER						\
		)															\
	)																\
)

	/* We only accept:
		(mem reg)
		(mem (plus reg reg))
		(mem (plus reg 0..31)).
		unless TARGET_BASE_ADDRESSES, in which case we accept all
		(mem constant_address) too.
	*/

	/* (mem reg) */

	if(REG_P(x) && HIP_REG_OK(x))
		return 1;

	if(GET_CODE(x) == PLUS)
	{
		rtx x1 = XEXP (x, 0);
		rtx x2 = XEXP (x, 1);

		/* Try swapping the order.  FIXME: Do we need this?  */
		if(! REG_P (x1))
		{
			rtx tem = x1;
			x1 = x2;
			x2 = tem;
		}

		/* (mem (plus (reg?) (?))) */
		if(!REG_P (x1) || !HIP_REG_OK (x1))
			return TARGET_BASE_ADDRESSES && hip_constant_address_p (x);

		/* (mem (plus (reg) (reg?))) */
		if(REG_P (x2) && HIP_REG_OK (x2))
			return 1;

		/* (mem (plus (reg) (0..31?))) */
		if(GET_CODE (x2) == CONST_INT && CONST_OK_FOR_LETTER_P(INTVAL(x2), 'N'))
			return 1;

		return 0;
	}

	return TARGET_BASE_ADDRESSES && hip_constant_address_p(x);

}

/*
	LEGITIMATE_CONSTANT_P.
*/
int hip_legitimate_constant_p(rtx x)
{
	HIP_FUNCTION_ENTRY();
	/*
		We must allow any number due to the way the cse passes works; if we
		do not allow any number here, general_operand will fail, and insns
		will fatally fail recognition instead of "softly".
	*/
	if(GET_CODE (x) == CONST_INT)
		return 1;

	return CONSTANT_ADDRESS_P(x);

}

/*
	TODO: Not sure about this.
*/
enum machine_mode hip_select_cc_mode(RTX_CODE op, rtx x, rtx y ATTRIBUTE_UNUSED)
{
	/* We use CCmode, CC_UNSmode, CC_FPmode, CC_FPEQmode and CC_FUNmode to
	 output different compare insns.  Note that we do not check the
	 validity of the comparison here.  */

	if(GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	{
		if(
			op == ORDERED	|| op == UNORDERED	|| op == UNGE	||
			op == UNGT		|| op == UNLE		|| op == UNLT
		)
			return CC_FUNmode;

		if(op == EQ || op == NE)
			return CC_FPEQmode;

		return CC_FPmode;
	}

	if(op == GTU || op == LTU || op == GEU || op == LEU)
		return CC_UNSmode;

	return CCmode;

}

/*
	TODO: Don't think this is needed since HIP doesn't support floating point.
*/
int hip_reversible_cc_mode(enum machine_mode mode)
{
	HIP_FUNCTION_ENTRY();
	/* That is, all integer and the EQ, NE, ORDERED and UNORDERED float
	 compares.  */
	return mode != CC_FPmode;

}

/* TARGET_RTX_COSTS.  */

static bool hip_rtx_costs(
	rtx x			ATTRIBUTE_UNUSED,
	int code		ATTRIBUTE_UNUSED,
	int outer_code	ATTRIBUTE_UNUSED,
	int *total		ATTRIBUTE_UNUSED,
	bool speed		ATTRIBUTE_UNUSED
)
{
	HIP_FUNCTION_ENTRY();
	/* FIXME: For the time being, this is just a stub and we'll accept the
	 generic calculations, until we can do measurements, at least.
	 Say we did not modify any calculated costs.  */
	return false;

}

/* REGISTER_MOVE_COST.  */

int hip_register_move_cost(
	enum machine_mode mode ATTRIBUTE_UNUSED, enum reg_class from, enum reg_class to
)
{
	HIP_FUNCTION_ENTRY();
	return (from == GENERAL_REGS && from == to) ? 2 : 3;

}

/* Note that we don't have a TEXT_SECTION_ASM_OP, because it has to be a
 compile-time constant; it's used in an asm in crtstuff.c, compiled for
 the target.  */

/*
	TODO: Probably don't need this, a simple macro is already defined.
*/
const char *
hip_data_section_asm_op(void)
{
	HIP_FUNCTION_ENTRY();
	return "\t.data";

}

static void hip_encode_section_info(tree decl, rtx rtl, int first)
{
	HIP_FUNCTION_ENTRY();
	/* Test for an external declaration, and do nothing if it is one.  */
	if
	(
		(
			TREE_CODE (decl) == VAR_DECL &&
			(DECL_EXTERNAL (decl) || TREE_PUBLIC (decl))
		)
		||
		(TREE_CODE (decl) == FUNCTION_DECL && TREE_PUBLIC (decl))
	)
	{}
	else if(first && DECL_P (decl))
	{
		/* For non-visible declarations, add a "@" prefix, which we skip
		 when the label is output.  If the label does not have this
		 prefix, a ":" is output if -mtoplevel-symbols.

		 Note that this does not work for data that is declared extern and
		 later defined as static.  If there's code in between, that code
		 will refer to the extern declaration, and vice versa.  This just
		 means that when -mtoplevel-symbols is in use, we can just handle
		 well-behaved ISO-compliant code.  */

		const char *str = XSTR (XEXP (rtl, 0), 0);
		int len = strlen (str);
		char *newstr = XALLOCAVEC (char, len + 2);
		newstr[0] = '@';
		strcpy (newstr + 1, str);
		XSTR (XEXP (rtl, 0), 0) = ggc_alloc_string (newstr, len + 1);
	}

	/* Set SYMBOL_REF_FLAG for things that we want to access with GETA.  We
	 may need different options to reach for different things with GETA.
	 For now, functions and things we know or have been told are constant.  */
	if(TREE_CODE (decl) == FUNCTION_DECL
			|| TREE_CONSTANT (decl)
			|| (TREE_CODE (decl) == VAR_DECL
					&& TREE_READONLY (decl)
					&& !TREE_SIDE_EFFECTS (decl)
					&& (!DECL_INITIAL (decl)
							|| TREE_CONSTANT (DECL_INITIAL (decl)))))
	SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;

}

static const char *
hip_strip_name_encoding(const char *name)
{
	HIP_FUNCTION_ENTRY();

	for(; (*name == '@' || *name == '*'); name++)
		;

	return name;
}

/*
	TARGET_ASM_FILE_START.
	We just emit a little comment for the time being.
*/
static void hip_file_start(void)
{
	HIP_FUNCTION_ENTRY();
	default_file_start();

	fputs("#\tData section\n", asm_out_file);

	/* Make sure each file starts with the text section.  */
	switch_to_section(text_section);

}

/* TARGET_ASM_FILE_END.  */

static void hip_file_end(void)
{
	HIP_FUNCTION_ENTRY();
	/* Make sure each file ends with the data section.  */
	switch_to_section(data_section);

}

/* TARGET_ASM_OUTPUT_SOURCE_FILENAME.  */

static void hip_asm_output_source_filename(FILE *stream, const char *name)
{
	HIP_FUNCTION_ENTRY();
	fprintf(stream, "#\t");
	OUTPUT_QUOTED_STRING(stream, name);
	fprintf(stream, "\n");

}

/*
	OUTPUT_QUOTED_STRING.
*/
void hip_output_quoted_string(FILE *stream, const char *string, int length)
{
	HIP_FUNCTION_ENTRY();
	const char * string_end = string + length;
	static const char * const unwanted_chars = "\"[]\\";

	/* Output "any character except newline and double quote character".  We
	 play it safe and avoid all control characters too.  We also do not
	 want [] as characters, should input be passed through m4 with [] as
	 quotes.  Further, we avoid "\", because the GAS port handles it as a
	 quoting character.  */
	while (string < string_end)
	{
		if(*string && (unsigned char) *string < 128 && !ISCNTRL (*string)
		&& strchr (unwanted_chars, *string) == NULL){
			fputc('"', stream);
			while(
				*string
				&& (unsigned char) *string < 128
				&& !ISCNTRL (*string)
				&& strchr(unwanted_chars, *string) == NULL
				&& string < string_end
			)
			{
				fputc(*string, stream);
				string++;
			}
			fputc('"', stream);
			if(string < string_end)
				fprintf(stream, ",");
		}
		if(string < string_end)
		{
			fprintf(stream, "#%x", *string & 31);
			string++;
			if(string < string_end)
				fprintf(stream, ",");
		}
	}

}

/*
	Target hook for assembling integer objects.  Use hip_print_operand
	for WYDE and TETRA.  Use hip_output_octa to output 4-byte
	CONST_DOUBLEs.
	TODO: Need to change.
*/

static bool hip_emit_integer(rtx x, char* instruction, char letter, int* aligned_p)
{
	if(GET_CODE(x) != CONST_INT)
	{
		*aligned_p = 0;
		return false;
	}

	fputs(instruction, asm_out_file);
	hip_print_operand(asm_out_file, x, letter);
	fputc('\n', asm_out_file);

	return true;
}

static bool hip_assemble_integer(rtx x, unsigned int size, int aligned_p)
{
	HIP_FUNCTION_ENTRY();

	if(aligned_p)
	{
		switch (size)
		{
			case 1:
				if(hip_emit_integer(x, "\t.byte\t", 'B', &aligned_p))
				{
					return true;
				}
				break;

			case 2:
				if(hip_emit_integer(x, "\t.word16\t", 'H', &aligned_p))
				{
					return true;
				}
				break;

			case 4:
				if(hip_emit_integer(x, "\t.word\t", 'W', &aligned_p))
				{
					return true;
				}
				break;
		}
	}

	return default_assemble_integer(x, size, aligned_p);
}

/*
	ASM_OUTPUT_ASCII.
	TODO: Need to change.
*/
void hip_asm_output_ascii(FILE *stream, const char *string, int length)
{
	HIP_FUNCTION_ENTRY();
	while (length > 0)
	{
		int chunk_size = length > 60 ? 60 : length;
		fprintf(stream, "\tBYTE ");
		hip_output_quoted_string(stream, string, chunk_size);
		string += chunk_size;
		length -= chunk_size;
		fprintf(stream, "\n");
	}

}

/*
	ASM_OUTPUT_ALIGNED_COMMON.
	TODO: Need to change.
*/
void hip_asm_output_aligned_common(
	FILE *stream, const char *name, int size, int align
)
{
	HIP_FUNCTION_ENTRY();

	fprintf(stream, "\t.align %d\n", align / BITS_PER_UNIT);
	assemble_name(stream, name);
	fprintf(stream, "\t.space %d", size);

	/* This is mostly the elfos.h one.  There doesn't seem to be a way to
	 express this in a hipal-compatible way.  */
	/*fprintf(stream, "\t.comm\t");
	assemble_name(stream, name);
	fprintf(
		stream, ",%u,%u ! hipal-incompatible COMMON\n", size, align / BITS_PER_UNIT
	);*/

}

/*
	ASM_OUTPUT_ALIGNED_LOCAL.
	TODO: Need to change.
*/
void hip_asm_output_aligned_local(
	FILE *stream, const char *name, int size, int align
)
{
	HIP_FUNCTION_ENTRY();
	switch_to_section(data_section);

	ASM_OUTPUT_ALIGN(stream, exact_log2(align / BITS_PER_UNIT));
	assemble_name(stream, name);
	fprintf(stream, "\tLOC @+%d\n", size);

}

/*
	ASM_OUTPUT_LABEL.
	TODO: Need to change.
*/
void hip_asm_output_label(FILE *stream, const char *name)
{
	HIP_FUNCTION_ENTRY();
	assemble_name(stream, name);
	/*fprintf(stream, "\tIS @\n");*/

}

/*
	ASM_OUTPUT_INTERNAL_LABEL.
	TODO: Need to change.
*/
void hip_asm_output_internal_label(FILE *stream, const char *name)
{
	HIP_FUNCTION_ENTRY();
	assemble_name_raw(stream, name);
	/*fprintf(stream, "\tIS @\n");*/

}

/*
	ASM_DECLARE_REGISTER_GLOBAL.
*/
void hip_asm_declare_register_global(
	FILE *stream		ATTRIBUTE_UNUSED,
	tree decl			ATTRIBUTE_UNUSED,
	int regno			ATTRIBUTE_UNUSED,
	const char *name	ATTRIBUTE_UNUSED
)
{
	HIP_FUNCTION_ENTRY();
	/* Nothing to do here, but there *will* be, therefore the framework is
	 here.  */

}

/*
	ASM_WEAKEN_LABEL.
	TODO: Need to change.
*/

void hip_asm_weaken_label(FILE *stream ATTRIBUTE_UNUSED,
		const char *name ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	fprintf(stream, "\t.weak ");
	assemble_name(stream, name);
	fprintf(stream, " ! hipal-incompatible\n");

}

/*
	MAKE_DECL_ONE_ONLY.
*/
void hip_make_decl_one_only(tree decl)
{
	HIP_FUNCTION_ENTRY();
	DECL_WEAK (decl) = 1;

}

/*
	ASM_OUTPUT_LABELREF.
	Strip GCC's '*' and our own '@'.  No order is assumed.
	TODO: Need to change.
*/
void hip_asm_output_labelref(FILE *stream, const char *name)
{
	HIP_FUNCTION_ENTRY();
	int is_extern = 1;

	for (; (*name == '@' || *name == '*'); name++)
	{
		if(*name == '@')
			is_extern = 0;
	}

	asm_fprintf(
		stream,
		"%s%U%s",
			is_extern && TARGET_TOPLEVEL_SYMBOLS ? ":" : "",
			name
	);

}

/*
	ASM_OUTPUT_DEF.
	TODO: Need to change.
*/
void hip_asm_output_def(FILE *stream, const char *name, const char *value)
{
	HIP_FUNCTION_ENTRY();
	assemble_name(stream, name);
	fprintf(stream, "\tIS ");
	assemble_name(stream, value);
	fputc('\n', stream);

}

/*
	PRINT_OPERAND.
	TODO: Need to change.
*/
void hip_print_operand(FILE *stream, rtx x, int code)
{
	HIP_FUNCTION_ENTRY();
	/* When we add support for different codes later, we can, when needed,
	 drop through to the main handler with a modified operand.  */
	rtx modified_x = x;
	int regno = x != NULL_RTX && REG_P (x) ? REGNO (x) : 0;

	switch (code)
	{
	/* Unrelated codes are in alphabetic order.  */

	case '+':
		/* For conditional branches, output "P" for a probable branch.  */
		if(TARGET_BRANCH_PREDICT)
		{
			x = find_reg_note(current_output_insn, REG_BR_PROB, 0);
			if(x && INTVAL (XEXP (x, 0))> REG_BR_PROB_BASE / 2)
			putc ('P', stream);
		}
		return;

	case '.':
		/* For the %d in POP %d,0.  */
		fprintf (stream, "%d", HIP_POP_ARGUMENT ());
		return;

	case 'B':
		if(GET_CODE (x) != CONST_INT)
			fatal_insn ("HIP Internal: Expected a CONST_INT, not this", x);
		fprintf (stream, "%d", (int) (INTVAL (x) & 0xff));
		return;

	case 'H':
		/* Highpart.  Must be general register, and not the last one, as
		 that one cannot be part of a consecutive register pair.  */
		if(regno > HIP_LAST_GENERAL_REGISTER - 1)
			internal_error ("HIP Internal: Bad register: %d", regno);

		/* This is big-endian, so the high-part is the first one.  */
		fprintf (stream, "%s", reg_names[HIP_OUTPUT_REGNO (regno)]);
		return;

	case 'L':
		/* Lowpart.  Must be CONST_INT or general register, and not the last
		 one, as that one cannot be part of a consecutive register pair.  */
		if(GET_CODE (x) == CONST_INT)
		{
			fprintf (stream, "#%lx",
					(unsigned long) (INTVAL (x)
							& ((unsigned int) 0x7fffffff * 2 + 1)));
			return;
		}

		if(GET_CODE (x) == SYMBOL_REF)
		{
			output_addr_const (stream, x);
			return;
		}

		if(regno > HIP_LAST_GENERAL_REGISTER - 1)
			internal_error ("HIP Internal: Bad register: %d", regno);

		/* This is big-endian, so the low-part is + 1.  */
		fprintf (stream, "%s", reg_names[HIP_OUTPUT_REGNO (regno) + 1]);
		return;

		/* Can't use 'a' because that's a generic modifier for address
		 output.  */
	case 'A':
		hip_output_shiftvalue_op_from_str (stream, "ANDN",
				~(unsigned HOST_WIDEST_INT)
				hip_intval (x));
		return;

	case 'i':
		hip_output_shiftvalue_op_from_str (stream, "INC",
				(unsigned HOST_WIDEST_INT)
				hip_intval (x));
		return;

	case 'o':
		hip_output_shiftvalue_op_from_str (stream, "OR",
				(unsigned HOST_WIDEST_INT)
				hip_intval (x));
		return;

	case 's':
		hip_output_shiftvalue_op_from_str (stream, "SET",
				(unsigned HOST_WIDEST_INT)
				hip_intval (x));
		return;

	case 'd':
	case 'D':
		hip_output_condition (stream, x, (code == 'D'));
		return;

	case 'e':
		/* Output an extra "e" to make fcmpe, fune.  */
		if(TARGET_FCMP_EPSILON)
		fprintf (stream, "e");
		return;

	case 'm':
		/* Output the number minus 1.  */
		if(GET_CODE (x) != CONST_INT)
		{
			fatal_insn ("HIP Internal: Bad value for 'm', not a CONST_INT",
					x);
		}
		fprintf (stream, HOST_WIDEST_INT_PRINT_DEC,
				(HOST_WIDEST_INT) (hip_intval (x) - 1));
		return;

	case 'p':
		/* Store the number of registers we want to save.  This was setup
		 by the prologue.  The actual operand contains the number of
		 registers to pass, but we don't use it currently.  Anyway, we
		 need to output the number of saved registers here.  */
		fprintf (stream, "%d",
				cfun->machine->highest_saved_stack_register + 1);
		return;

	case 'r':
		/* Store the register to output a constant to.  */
		if(! REG_P (x))
		fatal_insn ("HIP Internal: Expected a register, not this", x);
		hip_output_destination_register = HIP_OUTPUT_REGNO (regno);
		return;

	case 'I':
		/* Output the constant.  Note that we use this for floats as well.  */
		if(GET_CODE (x) != CONST_INT
				&& (GET_CODE (x) != CONST_DOUBLE
						|| (GET_MODE (x) != VOIDmode && GET_MODE (x) !=DFmode
		  && GET_MODE (x) != SFmode)))
	fatal_insn ("HIP Internal: Expected a constant, not this", x);
      hip_output_register_setting (stream,
				    hip_output_destination_register,
				    hip_intval (x), 0);
      return;

    case 'U':
      /* An U for unsigned, if TARGET_ZERO_EXTEND.  Ignore the operand.  */
      if(TARGET_ZERO_EXTEND)
	putc ('U', stream);
      return;

    case 'v':
		hip_output_shifted_value (stream, (HOST_WIDEST_INT) hip_intval (x));
		return;

    case 'V':
		hip_output_shifted_value (stream, (HOST_WIDEST_INT) ~hip_intval (x));
		return;

    case 'W':
		if(GET_CODE(x) != CONST_INT)
		{
			fatal_insn("HIP Internal: Expected a CONST_INT, not this", x);
		}
		fprintf(stream, "%d\t", (int) INTVAL(x));
		fprintf(stream, "#%x\t", (int) (INTVAL(x) & 0xffff));
		fprintf(stream, "#%x", INTVAL(x));
		return;

    case 0:
      /* Nothing to do.  */
      break;

    default:
      /* Presumably there's a missing case above if we get here.  */
      internal_error ("HIP Internal: Missing %qc case in hip_print_operand", code);
    }

	switch (GET_CODE (modified_x) )
	{
	case REG:
		regno = REGNO (modified_x);
		if(regno >= FIRST_PSEUDO_REGISTER)
			internal_error("HIP Internal: Bad register: %d", regno);
		fprintf(stream, "%s", reg_names[HIP_OUTPUT_REGNO (regno)]);
		return;

	case MEM:
		output_address(XEXP (modified_x, 0));
		return;

	case CONST_INT:
		/* For -2147483648, hipal complains that the constant does not fit
		 in 4 bytes, so let's output it as hex.  Take care to handle hosts
		 where HOST_WIDE_INT is longer than an int.

		 Print small constants +-31 using decimal.  */

		if(INTVAL (modified_x) > -HIP_WORD_SIZE && INTVAL (modified_x) < HIP_WORD_SIZE)
		fprintf (stream, "%d", (int) (INTVAL (modified_x)));
		else
		fprintf (stream, "#%x",
				(int) (INTVAL (modified_x)) & (unsigned int) ~0);
		return;

	case CONST:
		output_addr_const (stream, modified_x);
		return;

	default:
		/* No need to test for all strange things.  Let output_addr_const do
		 it for us.  */
		if(CONSTANT_P (modified_x)
				/* Strangely enough, this is not included in CONSTANT_P.
				 FIXME: Ask/check about sanity here.  */
				|| GET_CODE (modified_x) == CODE_LABEL)
		{
			output_addr_const (stream, modified_x);
			return;
		}

		/* We need the original here.  */
		fatal_insn ("HIP Internal: Cannot decode this operand", x);
	}

}

/*
	PRINT_OPERAND_PUNCT_VALID_P.
	TODO: Need to change.
*/
int hip_print_operand_punct_valid_p(int code ATTRIBUTE_UNUSED)
{
	HIP_FUNCTION_ENTRY();
	/*
		A '+' is used for branch prediction, similar to other ports.
		A '.' is used for the %d in the POP %d,0 return insn.
	*/
	return code == '+' || code == '.';

}

/*
	PRINT_OPERAND_ADDRESS.
	TODO: Need to change.
*/
void hip_print_operand_address(FILE *stream, rtx x)
{
	HIP_FUNCTION_ENTRY();
	if(REG_P (x))
	{
		/* I find the generated assembly code harder to read without
		 the ",0".  */
		fprintf(stream, "%s,0", reg_names[HIP_OUTPUT_REGNO (REGNO (x))]);
		return;
	}
	else if(GET_CODE (x) == PLUS)
	{
		rtx x1 = XEXP (x, 0);
		rtx x2 = XEXP (x, 1);

		if(REG_P (x1))
		{
			fprintf (stream, "%s,", reg_names[HIP_OUTPUT_REGNO (REGNO (x1))]);

			if(REG_P (x2))
			{
				fprintf (stream, "%s", reg_names[HIP_OUTPUT_REGNO (REGNO (x2))]);
				return;
			}
			else if(GET_CODE (x2) == CONST_INT && CONST_OK_FOR_LETTER_P (INTVAL (x2), 'N'))
			{
				output_addr_const (stream, x2);
				return;
			}
		}
	}

	if(TARGET_BASE_ADDRESSES && hip_legitimate_constant_p (x))
	{
		output_addr_const (stream, x);
		return;
	}

	fatal_insn ("HIP Internal: This is not a recognized address", x);

}

/*
	ASM_OUTPUT_REG_PUSH.
*/
void hip_asm_output_reg_push(FILE *stream, int regno)
{
	HIP_FUNCTION_ENTRY();
	const char* sp = reg_names[HIP_STACK_POINTER_REGNUM];

	fprintf(
		stream,
		"\tsw 0(%s), %s\n\tsubui %s, %s, %d",
			sp,
			reg_names[HIP_OUTPUT_REGNO(regno)],
			sp,
			sp,
			UNITS_PER_WORD
	);

}

/*
	ASM_OUTPUT_REG_POP.
*/
void hip_asm_output_reg_pop(FILE *stream, int regno)
{
	HIP_FUNCTION_ENTRY();
	const char* sp = reg_names[HIP_STACK_POINTER_REGNUM];

	fprintf(
		stream,
		"\taddui %s, %s, %d\n\tlw %s, 0(%s)",
			sp,
			sp,
			UNITS_PER_WORD,
			reg_names[HIP_OUTPUT_REGNO(regno)],
			sp
	);

}

/*
	ASM_OUTPUT_ADDR_DIFF_ELT.
	TODO: Need to change.
*/
void hip_asm_output_addr_diff_elt(
	FILE *stream, rtx body ATTRIBUTE_UNUSED, int value, int rel
)
{
	HIP_FUNCTION_ENTRY();
	fprintf(stream, "\tTETRA L%d-L%d\n", value, rel);

}

/*
	ASM_OUTPUT_ADDR_VEC_ELT.
	TODO: Need to change.
*/
void hip_asm_output_addr_vec_elt(FILE *stream, int value)
{
	HIP_FUNCTION_ENTRY();
	fprintf(stream, "\tOCTA L:%d\n", value);

}

/*
	ASM_OUTPUT_SKIP.
	TODO: Need to change.
*/
void hip_asm_output_skip(FILE *stream, int nbytes)
{
	HIP_FUNCTION_ENTRY();
	fprintf(stream, "\tLOC @+%d\n", nbytes);

}

/*
	ASM_OUTPUT_ALIGN.
	TODO: Probably not needed.
*/
void hip_asm_output_align(FILE *stream, int power)
{
	HIP_FUNCTION_ENTRY();
	/* We need to record the needed alignment of this section in the object,
	 so we have to output an alignment directive.  Use a .p2align (not
	 .align) so people will never have to wonder about whether the
	 argument is in number of bytes or the log2 thereof.  We do it in
	 addition to the LOC directive, so nothing needs tweaking when
	 copy-pasting assembly into hipal.  */
	/*fprintf(stream, "\t.p2align %d\n", power);
	fprintf(stream, "\tLOC @+(%d-@)&%d\n", 1 << power, (1 << power) - 1);*/

}

/*
	DBX_REGISTER_NUMBER.
	A C expression that returns the DBX register number for the compiler register number regno.
*/
unsigned hip_dbx_register_number(unsigned regno)
{
	HIP_FUNCTION_ENTRY();
	/* Adjust the register number to the one it will be output as, dammit.
	 It'd be nice if we could check the assumption that we're filling a
	 gap, but every register between the last saved register and parameter
	 registers might be a valid parameter register.  */
	regno = HIP_OUTPUT_REGNO (regno);

	/* We need to renumber registers to get the number of the return address
	 register in the range 0..31.  It is also space-saving if registers
	 mentioned in the call-frame information (which uses this function by
	 defaulting DWARF_FRAME_REGNUM to DBX_REGISTER_NUMBER) are numbered
	 0 .. 63.  So map 224 .. 32+15 -> 0 .. 47 and 0 .. 223 -> 48..223+48.  */
	/*return regno >= 224 ? (regno - 224) : (regno + 48);*/
	return regno;

}

/* End of target macro support functions.

 Now the HIP port's own functions.  First the exported ones.  */

/* Wrapper for get_hard_reg_initial_val since integrate.h isn't included
 from insn-emit.c.  */

rtx hip_get_hard_reg_initial_val(enum machine_mode mode, int regno)
{
	HIP_FUNCTION_ENTRY();
	return get_hard_reg_initial_val(mode, regno);

}

/* Nonzero when the function epilogue is simple enough that a single
 "POP %d,0" should be used even within the function.  */

int hip_use_simple_return(void)
{
	HIP_FUNCTION_ENTRY();
	int regno;

	int stack_space_to_allocate = (crtl->outgoing_args_size
			+ crtl->args.pretend_args_size + get_frame_size() + 7) & ~7;

	if(!TARGET_USE_RETURN_INSN || !reload_completed)
		return 0;

	for (regno = 31; regno >= HIP_FIRST_GLOBAL_REGNUM; regno--)
		/* Note that we assume that the frame-pointer-register is one of these
		 registers, in which case we don't count it here.  */
		if((((regno != HIP_FRAME_POINTER_REGNUM || !frame_pointer_needed)&& df_regs_ever_live_p (regno) && !call_used_regs[regno]))
		|| IS_HIP_EH_RETURN_DATA_REG (regno))
		return 0;

	if(frame_pointer_needed)
		stack_space_to_allocate += UNITS_PER_WORD;

	if(HIP_CFUN_HAS_LANDING_PAD)
		stack_space_to_allocate += 2 * UNITS_PER_WORD;
	else if(HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
		stack_space_to_allocate += UNITS_PER_WORD;

	return stack_space_to_allocate == 0;

}

/* Expands the function prologue into RTX.  */

void hip_expand_prologue(void)
{
	HIP_FUNCTION_ENTRY();
	HOST_WIDE_INT locals_size = get_frame_size();
	int regno;
	HOST_WIDE_INT stack_space_to_allocate = (crtl->outgoing_args_size
			+ crtl->args.pretend_args_size + locals_size + 7) & ~7;
	HOST_WIDE_INT offset = -UNITS_PER_WORD;

	/* Add room needed to save global non-register-stack registers.  */
	for (regno = 31; regno >= HIP_FIRST_GLOBAL_REGNUM; regno--)
		/* Note that we assume that the frame-pointer-register is one of these
		 registers, in which case we don't count it here.  */
		if((((regno != HIP_FRAME_POINTER_REGNUM || !frame_pointer_needed)&& df_regs_ever_live_p (regno) && !call_used_regs[regno]))
		|| IS_HIP_EH_RETURN_DATA_REG (regno))
		stack_space_to_allocate += UNITS_PER_WORD;

		/* If we do have a frame-pointer, add room for it.  */
	if(frame_pointer_needed)
		stack_space_to_allocate += UNITS_PER_WORD;

	/* If we have a non-local label, we need to be able to unwind to it, so
	 store the current register stack pointer.  Also store the return
	 address if we do that.  */
	if(HIP_CFUN_HAS_LANDING_PAD)
		stack_space_to_allocate += 2 * UNITS_PER_WORD;
	else if(HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
		/* If we do have a saved return-address slot, add room for it.  */
		stack_space_to_allocate += UNITS_PER_WORD;

	/* Make sure we don't get an unaligned stack.  */
	if((stack_space_to_allocate % UNITS_PER_WORD) != 0)
		internal_error("stack frame not a multiple of 4 bytes: %wd",
				stack_space_to_allocate);

	if(crtl->args.pretend_args_size)
	{
		int hip_first_vararg_reg = (HIP_FIRST_ARG_REGNUM
				+ (HIP_MAX_ARGS_IN_REGS - crtl->args.pretend_args_size / UNITS_PER_WORD));

		for (regno = HIP_FIRST_ARG_REGNUM + HIP_MAX_ARGS_IN_REGS - 1;
				regno >= hip_first_vararg_reg; regno--)
		{
			if(offset < 0)
			{
				HOST_WIDE_INT stack_chunk =
						stack_space_to_allocate > (HIP_WORD_SIZE - UNITS_PER_WORD) ?
								(HIP_WORD_SIZE - UNITS_PER_WORD) : stack_space_to_allocate;

				hip_emit_sp_add(-stack_chunk);
				offset += stack_chunk;
				stack_space_to_allocate -= stack_chunk;
			}

			/* These registers aren't actually saved (as in "will be
			 restored"), so don't tell DWARF2 they're saved.  */
			emit_move_insn(gen_rtx_MEM(DImode, plus_constant(stack_pointer_rtx,
			offset)),
			gen_rtx_REG (DImode, regno));
			offset -= UNITS_PER_WORD;
		}
	}

	/* Store the frame-pointer.  */

	if(frame_pointer_needed)
	{
		rtx insn;

		if(offset < 0)
		{
			/* Get 4 less than otherwise, since we need to reach offset + 4.  */
			HOST_WIDE_INT stack_chunk =
					stack_space_to_allocate > (HIP_WORD_SIZE - (2 * UNITS_PER_WORD)) ?
							(HIP_WORD_SIZE - (2 * UNITS_PER_WORD)) : stack_space_to_allocate;

			hip_emit_sp_add(-stack_chunk);

			offset += stack_chunk;
			stack_space_to_allocate -= stack_chunk;
		}

		insn = emit_move_insn(
				gen_rtx_MEM(DImode, plus_constant(stack_pointer_rtx,
				offset)),
				hard_frame_pointer_rtx);
		RTX_FRAME_RELATED_P (insn) = 1;
		insn = emit_insn(gen_adddi3(hard_frame_pointer_rtx,
		stack_pointer_rtx,
		GEN_INT (offset + UNITS_PER_WORD)));
		RTX_FRAME_RELATED_P (insn) = 1;
		offset -= UNITS_PER_WORD;
	}

	if(HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
	{
		rtx tmpreg, retreg;
		rtx insn;

		/* Store the return-address, if one is needed on the stack.  We
		 usually store it in a register when needed, but that doesn't work
		 with -fexceptions.  */

		if(offset < 0)
		{
			/* Get 4 less than otherwise, since we need to reach offset + 4.  */
			HOST_WIDE_INT stack_chunk =
					stack_space_to_allocate > (HIP_WORD_SIZE - (2 * UNITS_PER_WORD)) ?
							(HIP_WORD_SIZE - (2 * UNITS_PER_WORD)) : stack_space_to_allocate;

			hip_emit_sp_add(-stack_chunk);

			offset += stack_chunk;
			stack_space_to_allocate -= stack_chunk;
		}

		tmpreg = gen_rtx_REG(DImode, 31);
		/*retreg = gen_rtx_REG (DImode, HIP_rJ_REGNUM);*/
		retreg = gen_rtx_REG(DImode, 22);

		/* Dwarf2 code is confused by the use of a temporary register for
		 storing the return address, so we have to express it as a note,
		 which we attach to the actual store insn.  */
		emit_move_insn(tmpreg, retreg);

		insn = emit_move_insn(
				gen_rtx_MEM(DImode, plus_constant(stack_pointer_rtx,
				offset)),
				tmpreg);
		RTX_FRAME_RELATED_P (insn) = 1;
		add_reg_note(insn, REG_FRAME_RELATED_EXPR,
				gen_rtx_SET(VOIDmode,
						gen_rtx_MEM(DImode, plus_constant(stack_pointer_rtx,
						offset)),
						retreg));

		offset -= UNITS_PER_WORD;
	}
	else if(HIP_CFUN_HAS_LANDING_PAD)
		offset -= UNITS_PER_WORD;

	if(HIP_CFUN_HAS_LANDING_PAD)
	{
		/* Store the register defining the numbering of local registers, so
		 we know how long to unwind the register stack.  */

		if(offset < 0)
		{
			/* Get 4 less than otherwise, since we need to reach offset + 4.  */
			HOST_WIDE_INT stack_chunk =
					stack_space_to_allocate > (HIP_WORD_SIZE - (2 * UNITS_PER_WORD)) ?
							(HIP_WORD_SIZE - (2 * UNITS_PER_WORD)) : stack_space_to_allocate;

			hip_emit_sp_add(-stack_chunk);

			offset += stack_chunk;
			stack_space_to_allocate -= stack_chunk;
		}

		/* We don't tell dwarf2 about this one; we just have it to unwind
		 the register stack at landing pads.  FIXME: It's a kludge because
		 we can't describe the effect of the PUSHJ and PUSHGO insns on the
		 register stack at the moment.  Best thing would be to handle it
		 like stack-pointer offsets.  Better: some hook into dwarf2out.c
		 to produce DW_CFA_expression:s that specify the increment of rO,
		 and unwind it at eh_return (preferred) or at the landing pad.
		 Then saves to r0..rG-1 could be specified through that register.  */

		/*emit_move_insn (gen_rtx_REG (DImode, 31),
		 gen_rtx_REG (DImode,
		 HIP_rO_REGNUM));*/
		emit_move_insn(gen_rtx_MEM(DImode, plus_constant(stack_pointer_rtx, offset) ),
		gen_rtx_REG (DImode, 31));
		offset -= UNITS_PER_WORD;
	}

	/* After the return-address and the frame-pointer, we have the local
	 variables.  They're the ones that may have an "unaligned" size.  */
	offset -= (locals_size + 7) & ~7;

	/* Now store all registers that are global, i.e. not saved by the
	 register file machinery.

	 It is assumed that the frame-pointer is one of these registers, so it
	 is explicitly excluded in the count.  */

	for (regno = 31; regno >= HIP_FIRST_GLOBAL_REGNUM; regno--)
		if(((regno != HIP_FRAME_POINTER_REGNUM || !frame_pointer_needed)&& df_regs_ever_live_p (regno) && ! call_used_regs[regno])|| IS_HIP_EH_RETURN_DATA_REG (regno))
		{
			rtx insn;

			if(offset < 0)
			{
				HOST_WIDE_INT stack_chunk
				= (stack_space_to_allocate > (HIP_WORD_SIZE - offset - UNITS_PER_WORD)
				? (HIP_WORD_SIZE - offset - UNITS_PER_WORD) : stack_space_to_allocate);

				hip_emit_sp_add (-stack_chunk);
				offset += stack_chunk;
				stack_space_to_allocate -= stack_chunk;
			}

			insn = emit_move_insn (gen_rtx_MEM (DImode,
					plus_constant (stack_pointer_rtx,
							offset)),
			gen_rtx_REG (DImode, regno));
			RTX_FRAME_RELATED_P (insn) = 1;
			offset -= UNITS_PER_WORD;
		}

		/* Finally, allocate room for outgoing args and local vars if room
		 wasn't allocated above.  */
	if(stack_space_to_allocate)
		hip_emit_sp_add(-stack_space_to_allocate);

}

/* Expands the function epilogue into RTX.  */

void hip_expand_epilogue(void)
{
	HIP_FUNCTION_ENTRY();
	HOST_WIDE_INT locals_size = get_frame_size();
	int regno;
	HOST_WIDE_INT stack_space_to_deallocate = (crtl->outgoing_args_size
			+ crtl->args.pretend_args_size + locals_size + 7) & ~7;

	/* The first address to access is beyond the outgoing_args area.  */HOST_WIDE_INT offset =
			crtl->outgoing_args_size;

	/* Add the space for global non-register-stack registers.
	 It is assumed that the frame-pointer register can be one of these
	 registers, in which case it is excluded from the count when needed.  */
	for (regno = 31; regno >= HIP_FIRST_GLOBAL_REGNUM; regno--)
		if(((regno != HIP_FRAME_POINTER_REGNUM || !frame_pointer_needed)&& df_regs_ever_live_p (regno) && !call_used_regs[regno])|| IS_HIP_EH_RETURN_DATA_REG (regno))
		stack_space_to_deallocate += UNITS_PER_WORD;

		/* Add in the space for register stack-pointer.  If so, always add room
		 for the saved PC.  */
	if(HIP_CFUN_HAS_LANDING_PAD)
		stack_space_to_deallocate += 16;
	else if(HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
		/* If we have a saved return-address slot, add it in.  */
		stack_space_to_deallocate += UNITS_PER_WORD;

	/* Add in the frame-pointer.  */
	if(frame_pointer_needed)
		stack_space_to_deallocate += UNITS_PER_WORD;

	/* Make sure we don't get an unaligned stack.  */
	if((stack_space_to_deallocate % UNITS_PER_WORD) != 0)
		internal_error("stack frame not a multiple of octabyte: %wd",
				stack_space_to_deallocate);

	/* We will add back small offsets to the stack pointer as we go.
	 First, we restore all registers that are global, i.e. not saved by
	 the register file machinery.  */

	for (regno = HIP_FIRST_GLOBAL_REGNUM; regno <= 31; regno++)
		if(((regno != HIP_FRAME_POINTER_REGNUM || !frame_pointer_needed)&& df_regs_ever_live_p (regno) && !call_used_regs[regno])|| IS_HIP_EH_RETURN_DATA_REG (regno))
		{
			if(offset > 31)
			{
				hip_emit_sp_add (offset);
				stack_space_to_deallocate -= offset;
				offset = 0;
			}

			emit_move_insn (gen_rtx_REG (DImode, regno),
			gen_rtx_MEM (DImode,
					plus_constant (stack_pointer_rtx,
							offset)));
			offset += UNITS_PER_WORD;
		}

		/* Here is where the local variables were.  As in the prologue, they
		 might be of an unaligned size.  */
	offset += (locals_size + 7) & ~7;

	/* The saved register stack pointer is just below the frame-pointer
	 register.  We don't need to restore it "manually"; the POP
	 instruction does that.  */
	if(HIP_CFUN_HAS_LANDING_PAD)
		offset += 16;
	else if(HIP_CFUN_NEEDS_SAVED_EH_RETURN_ADDRESS)
		/* The return-address slot is just below the frame-pointer register.
		 We don't need to restore it because we don't really use it.  */
		offset += UNITS_PER_WORD;

	/* Get back the old frame-pointer-value.  */
	if(frame_pointer_needed)
	{
		if(offset > 31)
		{
			hip_emit_sp_add(offset);

			stack_space_to_deallocate -= offset;
			offset = 0;
		}

		emit_move_insn(hard_frame_pointer_rtx,
		gen_rtx_MEM (DImode,
				plus_constant (stack_pointer_rtx,
						offset)));
		offset += UNITS_PER_WORD;
	}

	/* We do not need to restore pretended incoming args, just add back
	 offset to sp.  */
	if(stack_space_to_deallocate != 0)
		hip_emit_sp_add(stack_space_to_deallocate);

	if(crtl->calls_eh_return)
		/* Adjust the (normal) stack-pointer to that of the receiver.
		 FIXME: It would be nice if we could also adjust the register stack
		 here, but we need to express it through DWARF 2 too.  */
		emit_insn(gen_adddi3(stack_pointer_rtx, stack_pointer_rtx,
		gen_rtx_REG (DImode,
				HIP_EH_RETURN_STACKADJ_REGNUM)));
	}

		/* Output an optimal sequence for setting a register to a specific
		 constant.  Used in an alternative for const_ints in movdi, and when
		 using large stack-frame offsets.

		 Use do_begin_end to say if a line-starting TAB and newline before the
		 first insn and after the last insn is wanted.  */

void hip_output_register_setting(FILE *stream, int regno, HOST_WIDEST_INT value,
		int do_begin_end)
{
	HIP_FUNCTION_ENTRY();
	if(do_begin_end)
		fprintf(stream, "\t");

	if(hip_shiftable_wyde_value((unsigned HOST_WIDEST_INT) value))
	{
		/* First, the one-insn cases.  */
		hip_output_shiftvalue_op_from_str(stream, "SET",
				(unsigned HOST_WIDEST_INT) value);
		fprintf(stream, " %s,", reg_names[regno]);
		hip_output_shifted_value(stream, (unsigned HOST_WIDEST_INT) value);
	}
	else if(hip_shiftable_wyde_value(-(unsigned HOST_WIDEST_INT) value))
	{
		/* We do this to get a bit more legible assembly code.  The next
		 alternative is mostly redundant with this.  */

		hip_output_shiftvalue_op_from_str(stream, "SET",
				-(unsigned HOST_WIDEST_INT) value);
		fprintf(stream, " %s,", reg_names[regno]);
		hip_output_shifted_value(stream, -(unsigned HOST_WIDEST_INT) value);
		fprintf(stream, "\n\tNEGU %s,0,%s", reg_names[regno], reg_names[regno]);
	}
	else if(hip_shiftable_wyde_value(~(unsigned HOST_WIDEST_INT) value))
	{
		/* Slightly more expensive, the two-insn cases.  */

		/* FIXME: We could of course also test if 0..31-N or ~(N | 1..31)
		 is shiftable, or any other one-insn transformation of the value.
		 FIXME: Check first if the value is "shiftable" by two loading
		 with two insns, since it makes more readable assembly code (if
		 anyone else cares).  */

		hip_output_shiftvalue_op_from_str(stream, "SET",
				~(unsigned HOST_WIDEST_INT) value);
		fprintf(stream, " %s,", reg_names[regno]);
		hip_output_shifted_value(stream, ~(unsigned HOST_WIDEST_INT) value);
		fprintf(stream, "\n\tNOR %s,%s,0", reg_names[regno], reg_names[regno]);
	}
	else
	{
		/* The generic case.  2..4 insns.  */
		static const char * const higher_parts[] =
		{ "L", "ML", "MH", "H" };
		const char *op = "SET";
		const char *line_begin = "";
		int insns = 0;
		int i;
		HOST_WIDEST_INT tmpvalue = value;

		/* Compute the number of insns needed to output this constant.  */
		for (i = 0; i < 4 && tmpvalue != 0; i++)
		{
			if(tmpvalue & 65535)
				insns++;
			tmpvalue >>= 16;
		}
		if(TARGET_BASE_ADDRESSES && insns == 3)
		{
			/* The number three is based on a static observation on
			 ghostscript-6.52.  Two and four are excluded because there
			 are too many such constants, and each unique constant (maybe
			 offset by 1..31) were used few times compared to other uses,
			 e.g. addresses.

			 We use base-plus-offset addressing to force it into a global
			 register; we just use a "LDA reg,VALUE", which will cause the
			 assembler and linker to DTRT (for constants as well as
			 addresses).  */
			fprintf(stream, "LDA %s,", reg_names[regno]);
			hip_output_octa(stream, value, 0);
		}
		else
		{
			/* Output pertinent parts of the 4-wyde sequence.
			 Still more to do if we want this to be optimal, but hey...
			 Note that the zero case has been handled above.  */
			for (i = 0; i < 4 && value != 0; i++)
			{
				if(value & 65535)
				{
					fprintf(stream, "%s%s%s %s,#%x", line_begin, op,
							higher_parts[i], reg_names[regno],
							(int) (value & 65535));
					/* The first one sets the rest of the bits to 0, the next
					 ones add set bits.  */
					op = "INC";
					line_begin = "\n\t";
				}

				value >>= 16;
			}
		}
	}

	if(do_begin_end)
		fprintf(stream, "\n");

}

/*
	Return 1 if value is 0..65535*2**(16*N) for N=0..3. else return 0.
*/
int hip_shiftable_wyde_value(unsigned HOST_WIDEST_INT value)
{
	HIP_FUNCTION_ENTRY();
	/* Shift by 16 bits per group, stop when we've found two groups with
	 nonzero bits.  */
	int i;
	int has_candidate = 0;

	for (i = 0; i < 4; i++)
	{
		if(value & 65535)
		{
			if(has_candidate)
				return 0;
			else
				has_candidate = 1;
		}

		value >>= 16;
	}

	return 1;

}

/* X and Y are two things to compare using CODE.  Return the rtx for
 the cc-reg in the proper mode.  */

rtx hip_gen_compare_reg(RTX_CODE code, rtx x, rtx y)
{
	HIP_FUNCTION_ENTRY();
	enum machine_mode ccmode = SELECT_CC_MODE(code, x, y);
	return gen_reg_rtx(ccmode);

}

/*
	Local (static) helper functions.
*/
static void hip_emit_sp_add(HOST_WIDE_INT offset)
{
	HIP_FUNCTION_ENTRY();
	rtx insn;

	if(offset < 0)
	{
		/* Negative stack-pointer adjustments are allocations and appear in
		 the prologue only.  We mark them as frame-related so unwind and
		 debug info is properly emitted for them.  */
		if(offset > -31)
		{
			insn = emit_insn(
				gen_adddi3(
				stack_pointer_rtx, stack_pointer_rtx, GEN_INT (offset)
				)
			);
		}
		else
		{
			HIP_WARNING("Offset is smaller than 31");
			rtx tmpr = gen_rtx_REG(DImode, 31);
			RTX_FRAME_RELATED_P( emit_move_insn(tmpr, GEN_INT(offset)) ) = 1;
			insn = emit_insn(
				gen_adddi3(stack_pointer_rtx, stack_pointer_rtx, tmpr)
			);
		}
		RTX_FRAME_RELATED_P (insn) = 1;
	}
	else
	{
		/* Positive adjustments are in the epilogue only.  Don't mark them
		 as "frame-related" for unwind info.  */
		if(CONST_OK_FOR_LETTER_P(offset, 'P'))
		{
			emit_insn(
				gen_adddi3(stack_pointer_rtx, stack_pointer_rtx, GEN_INT(offset))
			);
		}
		else
		{
			HIP_WARNING("Offset is not OK for letter P");
			rtx tmpr = gen_rtx_REG(DImode, 31);
			emit_move_insn(tmpr, GEN_INT (offset));
			insn = emit_insn(gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx, tmpr));
		}
	}

}

	/* Print operator suitable for doing something with a shiftable
	 wyde.  The type of operator is passed as an asm output modifier.  */

static void hip_output_shiftvalue_op_from_str(FILE *stream, const char *mainop,
		HOST_WIDEST_INT value)
{
	HIP_FUNCTION_ENTRY();
	static const char * const op_part[] =
	{ "L", "ML", "MH", "H" };
	int i;

	if(!hip_shiftable_wyde_value(value))
	{
		char s[sizeof("0xffffffffffffffff")];
		sprintf(s, HOST_WIDEST_INT_PRINT_HEX, value);
		internal_error("HIP Internal: %s is not a shiftable int", s);
	}

	for (i = 0; i < 4; i++)
	{
		/* We know we're through when we find one-bits in the low
		 16 bits.  */
		if(value & 0xffff)
		{
			fprintf(stream, "%s%s", mainop, op_part[i]);
			return;
		}
		value >>= 16;
	}

	/* No bits set?  Then it must have been zero.  */
	fprintf(stream, "%sL", mainop);

}

/* Print a 64-bit value, optionally prefixed by assembly pseudo.  */

static void hip_output_octa(FILE *stream, HOST_WIDEST_INT value,
		int do_begin_end)
{
	HIP_FUNCTION_ENTRY();
	/* Snipped from final.c:output_addr_const.  We need to avoid the
	 presumed universal "0x" prefix.  We can do it by replacing "0x" with
	 "#0" here; we must avoid a space in the operands and no, the zero
	 won't cause the number to be assumed in octal format.  */
	char hex_format[sizeof(HOST_WIDEST_INT_PRINT_HEX)];

	if(do_begin_end)
		fprintf(stream, "\tOCTA ");

	strcpy(hex_format, HOST_WIDEST_INT_PRINT_HEX);
	hex_format[0] = '#';
	hex_format[1] = '0';

	/* Provide a few alternative output formats depending on the number, to
	 improve legibility of assembler output.  */
	if((value < (HOST_WIDEST_INT) 0 && value > (HOST_WIDEST_INT) -10000)
			|| (value >= (HOST_WIDEST_INT) 0 && value <= (HOST_WIDEST_INT) 16384))
		fprintf(stream, "%d", (int) value);
	else if(value > (HOST_WIDEST_INT) 0
			&& value < ((HOST_WIDEST_INT) 1 << 31) * 2)
		fprintf(stream, "#%x", (unsigned int) value);
	else
		fprintf(stream, hex_format, value);

	if(do_begin_end)
		fprintf(stream, "\n");

}

/* Print the presumed shiftable wyde argument shifted into place (to
 be output with an operand).  */

static void hip_output_shifted_value(FILE *stream, HOST_WIDEST_INT value)
{
	HIP_FUNCTION_ENTRY();
	int i;

	if(!hip_shiftable_wyde_value(value))
	{
		char s[16 + 2 + 1];
		sprintf(s, HOST_WIDEST_INT_PRINT_HEX, value);
		internal_error("HIP Internal: %s is not a shiftable int", s);
	}

	for (i = 0; i < 4; i++)
	{
		/* We know we're through when we find one-bits in the low 16 bits.  */
		if(value & 0xffff)
		{
			fprintf(stream, "#%x", (int) (value & 0xffff));
			return;
		}

		value >>= 16;
	}

	/* No bits set?  Then it must have been zero.  */
	fprintf(stream, "0");

}

/* Output an HIP condition name corresponding to an operator
 and operands:
 (comparison_operator [(comparison_operator ...) (const_int 0)])
 which means we have to look at *two* operators.

 The argument "reversed" refers to reversal of the condition (not the
 same as swapping the arguments).  */

static void hip_output_condition(FILE *stream, rtx x, int reversed)
{
	HIP_FUNCTION_ENTRY();
	struct cc_conv
	{
		RTX_CODE cc;

		/* The normal output cc-code.  */
		const char * const normal;

		/* The reversed cc-code, or NULL if invalid.  */
		const char * const reversed;
	};

	struct cc_type_conv
	{
		enum machine_mode cc_mode;

		/* Terminated with {UNKNOWN, NULL, NULL} */
		const struct cc_conv * const convs;
	};

#undef CCEND
#define CCEND {UNKNOWN, NULL, NULL}

	static const struct cc_conv cc_fun_convs[] =
	{
	{ ORDERED, "Z", "P" },
	{ UNORDERED, "P", "Z" }, CCEND };
	static const struct cc_conv cc_fp_convs[] =
	{
	{ GT, "P", NULL },
	{ LT, "N", NULL }, CCEND };
	static const struct cc_conv cc_fpeq_convs[] =
	{
	{ NE, "Z", "P" },
	{ EQ, "P", "Z" }, CCEND };
	static const struct cc_conv cc_uns_convs[] =
	{
	{ GEU, "NN", "N" },
	{ GTU, "P", "NP" },
	{ LEU, "NP", "P" },
	{ LTU, "N", "NN" }, CCEND };
	static const struct cc_conv cc_signed_convs[] =
	{
	{ NE, "NZ", "Z" },
	{ EQ, "Z", "NZ" },
	{ GE, "NN", "N" },
	{ GT, "P", "NP" },
	{ LE, "NP", "P" },
	{ LT, "N", "NN" }, CCEND };
	static const struct cc_conv cc_di_convs[] =
	{
	{ NE, "NZ", "Z" },
	{ EQ, "Z", "NZ" },
	{ GE, "NN", "N" },
	{ GT, "P", "NP" },
	{ LE, "NP", "P" },
	{ LT, "N", "NN" },
	{ GTU, "NZ", "Z" },
	{ LEU, "Z", "NZ" }, CCEND };
#undef CCEND

	static const struct cc_type_conv cc_convs[] =
	{
	{ CC_FUNmode, cc_fun_convs },
	{ CC_FPmode, cc_fp_convs },
	{ CC_FPEQmode, cc_fpeq_convs },
	{ CC_UNSmode, cc_uns_convs },
	{ CCmode, cc_signed_convs },
	{ DImode, cc_di_convs } };

	size_t i;
	int j;

	enum machine_mode mode = GET_MODE (XEXP (x, 0));
	RTX_CODE cc = GET_CODE (x);

	for (i = 0; i < ARRAY_SIZE (cc_convs); i++)
	{
		if(mode == cc_convs[i].cc_mode)
		{
			for (j = 0; cc_convs[i].convs[j].cc != UNKNOWN; j++)
				if(cc == cc_convs[i].convs[j].cc)
				{
					const char *hip_cc = (
							reversed ?
									cc_convs[i].convs[j].reversed :
									cc_convs[i].convs[j].normal);

					if(hip_cc == NULL )
						fatal_insn(
							"HIP Internal: Trying to output invalidly reversed condition:",
							x
						);

					fprintf(stream, "%s", hip_cc);
					return;
				}

			fatal_insn("HIP Internal: What's the CC of this?", x);
		}
	}

	fatal_insn("HIP Internal: What is the CC of this?", x);

}

/* Return the bit-value for a const_int.  */

static HOST_WIDEST_INT hip_intval(rtx x)
{
	HIP_FUNCTION_ENTRY();
	unsigned HOST_WIDEST_INT retval;

	if(GET_CODE (x) == CONST_INT)
		return INTVAL (x);

	if(GET_CODE (x) == CONST_DOUBLE)
	{
		/* TODO: This shouldn't happen. */
		fatal_insn("HIP Internal: Floats are not supported:", x);
	}
	else
	{
		fatal_insn("HIP Internal: This is not a constant:", x);
	}

}

/* Worker function for TARGET_PROMOTE_FUNCTION_MODE.  */

enum machine_mode hip_promote_function_mode(const_tree type ATTRIBUTE_UNUSED,
		enum machine_mode mode, int *punsignedp ATTRIBUTE_UNUSED,
		const_tree fntype ATTRIBUTE_UNUSED, int for_return)
{
	/* Apparently not doing TRT if int < register-size.  FIXME: Perhaps
	 FUNCTION_VALUE and LIBCALL_VALUE needs tweaking as some ports say.  */
	if(for_return == 1)
		return mode;

	/* Promotion of modes currently generates slow code, extending before
	 operation, so we do it only for arguments.  */
	if(GET_MODE_CLASS (mode) == MODE_INT && GET_MODE_SIZE (mode) < UNITS_PER_WORD)
		return DImode;
	else
		return mode;

}
/* Worker function for TARGET_STRUCT_VALUE_RTX.  */

static rtx hip_struct_value_rtx(
	tree fntype ATTRIBUTE_UNUSED, int incoming ATTRIBUTE_UNUSED
)
{
	HIP_FUNCTION_ENTRY();
	return gen_rtx_REG(Pmode, HIP_STRUCT_VALUE_REGNUM);

}

/* Worker function for TARGET_FRAME_POINTER_REQUIRED.

 FIXME: Is this requirement built-in?  Anyway, we should try to get rid
 of it; we can deduce the value.  */

bool hip_frame_pointer_required(void)
{
	HIP_FUNCTION_ENTRY();
	return (cfun->has_nonlocal_label);

}

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
