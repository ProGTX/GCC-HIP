/* Definitions of target machine for GNU compiler, for HIP.
 Copyright (C) 2000, 2001, 2002, 2004, 2005, 2007, 2008, 2009, 2010
 Free Software Foundation, Inc.

 This file is based on the MMIX port of GCC, which was
 contributed by Hans-Peter Nilsson (hp@bitrange.com)

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

#ifndef GCC_HIP_H
#define GCC_HIP_H

/*
	Very important number.
*/
#define HIP_WORD_SIZE 32

/*
	Some local helper macros.  Note that the "default" value of
	FIXED_REGISTERS, CALL_USED_REGISTERS, REG_ALLOC_ORDER and
	REG_CLASS_CONTENTS depend on these values.
*/
#define HIP_FIRST_ARG_REGNUM 24
#define HIP_MAX_ARGS_IN_REGS 2

/*
	TODO: Some old values are just commented out instead of being removed.
	This is just temporal for debugging reasons.
*/

/*
	FIXME: This one isn't fully implemented yet.  Return values larger than
	one register are passed by reference in HIP_STRUCT_VALUE_REGNUM by the
	caller, except for return values of type "complex".
*/
#define HIP_NUMBER_OF_REGISTERS 32
#define HIP_MAX_REGS_FOR_VALUE 16	/* TODO: Not sure. */
#define HIP_RETURN_VALUE_REGNUM 28
#define HIP_STRUCT_VALUE_REGNUM HIP_RETURN_VALUE_REGNUM /*251*/	/* TODO: Not sure. */
#define HIP_STATIC_CHAIN_REGNUM 23 /*252*/	/* TODO: Not sure. */
#define HIP_FRAME_POINTER_REGNUM 29
#define HIP_STACK_POINTER_REGNUM 30
#define HIP_LAST_GENERAL_REGISTER 31
#define HIP_INCOMING_RETURN_ADDRESS_REGNUM 31
#define HIP_ARG_POINTER_REGNUM HIP_FIRST_ARG_REGNUM	/* TODO: Not sure. */

/*
	TODO: Something needs to be done about this.
	Most likely removal, since it deals with exception handling.

	Four registers; "ideally, these registers should be call-clobbered", so
	just grab a bunch of the common clobbered registers.
	FIXME: Last registers of return-value should be used,
	with an error if there's a return-value (that collides in size).
*/
#define HIP_EH_RETURN_DATA_REGNO_START (HIP_STRUCT_VALUE_REGNUM - 4)

/* Try to keep the definitions from running away on their own.  */
/*#if (HIP_EH_RETURN_DATA_REGNO_START
 != HIP_RESERVED_GNU_ARG_0_REGNUM + HIP_MAX_ARGS_IN_REGS)
 #error HIP register definition inconsistency
 #endif*/

#if (HIP_MAX_REGS_FOR_VALUE + HIP_MAX_ARGS_IN_REGS > HIP_NUMBER_OF_REGISTERS)
#error HIP parameters and return values bad, more than HIP_NUMBER_OF_REGISTERS registers
#endif

/*
	TODO: Probably remove.
	This chosen as "a call-clobbered hard register that is otherwise
	untouched by the epilogue".
*/
#define HIP_EH_RETURN_STACKADJ_REGNUM HIP_STATIC_CHAIN_REGNUM

#ifdef REG_OK_STRICT
# define HIP_REG_OK_STRICT 1
#else
# define HIP_REG_OK_STRICT 0
#endif

#define HIP_FUNCTION_ARG_SIZE(MODE, TYPE)	\
	((MODE) != BLKmode ? GET_MODE_SIZE (MODE) : int_size_in_bytes (TYPE))

/*
	Per-function machine data.
	This is normally an opaque type just defined and used in the tm.c file,
	but we need to see the definition in hip.md too.
*/
struct GTY(()) machine_function
{
	int has_landing_pad;
	int highest_saved_stack_register;
	int in_prologue;
};

/*
	This file contains comments with some information from the manual for GCC 4.9.0.
	The manual may change, but not by much.
	Original MMIX port was written for a much older version of GCC.
	Probably a good read is 'Using and Porting GCC' by Richard M. Stallman,
	even though it's quite old by now.

	Macros are grouped by nodes as they appear in the manual
	and tend to be alphabetized within each node.
*/


/* Node: Driver */


/* Node: Run-time Target */

/*
	Define HIP preprocessor macros.
*/
#define TARGET_CPU_CPP_BUILTINS()				\
	if(1)										\
	{											\
		builtin_define ("__hip__");				\
		builtin_define ("__HIP__");				\
		builtin_define ("__HIP_ABI_HIPWARE__");	\
	}

/*
	TODO: Can't find this in the manual.
	Could be TARGET_DEFAULT_TARGET_FLAGS.
*/
#define TARGET_DEFAULT \
 (MASK_BRANCH_PREDICT | MASK_BASE_ADDRESSES | MASK_USE_RETURN_INSN)

/*
	Also not in the manual, but it's pretty straightforward.
*/
#define TARGET_VERSION \
  fprintf (stderr, " (HIP)")


/* Node: Per-Function Data */

/*
	Macro called to initialize any target specific information. This macro is called once
	per function, before generation of any RTL has begun. The intention of this macro
	is to allow the initialization of the function pointer init_machine_status.
*/
#define INIT_EXPANDERS hip_init_expanders ()


/* Node: Storage Layout */

#define BITS_BIG_ENDIAN 1
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1
#define UNITS_PER_WORD 4
#define POINTER_SIZE 4

/*
	Set everything to maximum alignement.
*/
#define PARM_BOUNDARY		HIP_WORD_SIZE
#define STACK_BOUNDARY		HIP_WORD_SIZE
#define FUNCTION_BOUNDARY	HIP_WORD_SIZE
#define BIGGEST_ALIGNMENT	HIP_WORD_SIZE

/*
	Compute the alignment for a variable in the static store.
	TYPE is the data type, and BASIC_ALIGN is the alignment that the object would ordinarily have.
	One use of this macro is to increase alignment of medium-size data to make it all fit
	in fewer cache lines. Another is to cause character arrays to be word-aligned so that
	strcpy calls that copy constants to character arrays can be done inline.
*/
#define DATA_ALIGNMENT(TYPE, BASIC_ALIGN)	\
	hip_data_alignment (TYPE, BASIC_ALIGN)

/*
	Compute the alignment given to a constant that is being	placed in memory.
	CONSTANT is the constant and BASIC_ALIGN is the alignment that the object would ordinarily have.
	The typical use of this macro is to increase alignment for string constants to be word
	aligned so that strcpy calls that copy constants can be done inline.
*/
#define CONSTANT_ALIGNMENT(CONSTANT, BASIC_ALIGN)	\
	hip_constant_alignment (CONSTANT, BASIC_ALIGN)

/*
	Compute the alignment for a variable in the local store.
	TYPE is the data type, and basic-align is the alignment that the object would ordinarily have.
	One use of this macro is to increase alignment of medium-size data to make it all fit
	in fewer cache lines.
	If the value of this macro has a type, it should be an unsigned type.
*/
#define LOCAL_ALIGNMENT(TYPE, BASIC_ALIGN)	\
	hip_local_alignment (TYPE, BASIC_ALIGN)

/*
	Alignment in bits to be given to a structure bit-field that follows an empty field,
	such as int : 0;.
	Following other ports, this seems to most commonly be the word-size.
*/
#define EMPTY_FIELD_BOUNDARY HIP_WORD_SIZE

/*
	TODO: Try removing this.
	Number of bits which any structure or union’s size must be a multiple of. Each
	structure or union’s size is rounded up to a multiple of this.
	We chose to have this low solely for similarity with the alpha.
	It has nothing to do with passing the tests dg/c99-scope-2 and execute/align-1.c.
	Though the tests seem wrong. Padding of the structure is automatically added
	to get alignment when needed if we set this to just byte-boundary.
*/
#define STRUCTURE_SIZE_BOUNDARY 4

/*
	Data must be aligned.
*/
#define STRICT_ALIGNMENT 1


/* Node: Type Layout */

/*
	TODO: Not quite sure about long values.
	The default for LONG is same as INT.
*/
#define INT_TYPE_SIZE HIP_WORD_SIZE
#define SHORT_TYPE_SIZE 16
#define LONG_LONG_TYPE_SIZE 64

/*
	Can be overriden using -fsigned-char or -funsigned-char.
*/
#define DEFAULT_SIGNED_CHAR 1


/* Node: Register Basics */

/*
	Number of hardware registers known to the compiler.
	HIP has 32 general registers, PC, EPC and I.
*/
#define FIRST_PSEUDO_REGISTER 35

/*
	What is important with the following two macros is that each register
	must be non-fixed or call used (it can also be non-fixed and call used).
*/

/*
	Which registers aren't allowed to be freely assignable.
	Include stack pointer register and assign when needed.
	Special registers are not directly addressable.
*/
#define FIXED_REGISTERS \
{ \
	1,	/* 0 - Constant zero */						\
	0, 0, 0, 0, 0, 0, 0, 0,	/* 1-8, general */		\
	0, 0, 0, 0, 0, 0, 0, 0,	/* 9-16, general */		\
	0, 0, 0, 0, 0, 0, 0,	/* 17-23, general */	\
	0, /* 24 - First parameter */					\
	0, /* 25 - Second parameter */					\
	0, /* 26 - Long jump base register */			\
	0, /* 27 - Long call base register */			\
	0, /* 28 - Return value */						\
	0, /* 29 - Frame pointer */						\
	1, /* 30 - Stack pointer */						\
	0, /* 31 - Return address */					\
	1, /* PC */										\
	1, /* EPC */									\
	1  /* I */										\
}

/*
	Which registers can get clobbered during a subprogram call.
	Fixing is another thing that prevents clobbering.
	Frame pointer must be marked as 0.
*/
#define CALL_USED_REGISTERS \
{ \
	1,	/* 0 - Constant zero */						\
	1, 1, 1, 1, 1, 1, 1, 1,	/* 1-8, general */		\
	1, 1, 1, 1, 1, 1, 1, 1,	/* 9-16, general */		\
	1, 1, 1, 1, 1, 1, 1,	/* 17-23, general */	\
	1, /* 24 - First parameter */					\
	1, /* 25 - Second parameter */					\
	1, /* 26 - Long jump base register */			\
	1, /* 27 - Long call base register */			\
	1, /* 28 - Return value */						\
	0, /* 29 - Frame pointer */						\
	1, /* 30 - Stack pointer */						\
	1, /* 31 - Return address */					\
	1, /* PC */										\
	1, /* EPC */									\
	1  /* I */										\
}


/* Node: Allocation Order */


/* Node: Values in Registers */

/*
	A C expression for the number of consecutive hard registers, starting at register
	number REGNO, required to hold a value of mode MODE. This macro must never return zero.
	Definition taken from manual.
*/
#define HARD_REGNO_NREGS(REGNO, MODE)	\
	((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/*
	On HIP it is always allowed to store a value of mode MODE
	in hard register number REGNO (or in several registers starting with that one).
*/
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/*
	Nonzero if a value of mode MODE1 is accessible in mode MODE2 without copying.
	TODO: Seems to be right.
*/
#define MODES_TIEABLE_P(MODE1, MODE2) 1


/* Node: Leaf Functions */


/* Node: Register Classes */

/*
	TODO: Registers are not all equivalent.
	Probably need to create a special class for PC, EPC and I.
	For now these special registers just don't belong to GENERAL_REGS.
*/

/*
	The bare minimum of classes.
	LIM_REG_CLASSES is not a register class but rather tells how many classes there are.
*/
enum reg_class
{
	NO_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

/*
	The number of distinct register classes.
*/
#define N_REG_CLASSES (int) LIM_REG_CLASSES

/*
	These names are used in writing some of the debugging dumps.
*/
#define REG_CLASS_NAMES	\
	{"NO_REGS", "GENERAL_REGS", "ALL_REGS"}

/*
	An initializer containing the contents of the register classes as integers which are bit masks.
	The nth integer specifies the contents of class n. The way the integer mask is
	interpreted is that register r is in the class if mask & (1 << r) is 1.
	Each sub-initializer must be suitable as an initializer for the type HARD_REG_SET
	which is defined in 'hard-reg-set.h'. In this situation, the first integer in each subinitializer
	corresponds to registers 0 through 31, the second integer to registers 32 through 63, and so on.
*/
#define REG_CLASS_CONTENTS			\
{									\
	{0, 0},		/* NO_REGS */		\
	{~0, 0},	/* GENERAL_REGS */	\
	{~0, ~0}	/* ALL_REGS */		\
}

/*
	Returns minimal class that contains register REGNO.
*/
#define REGNO_REG_CLASS(REGNO)	\
	((REGNO) <= HIP_LAST_GENERAL_REGISTER ? GENERAL_REGS : ALL_REGS)

/*
	General registers are valid for base addressing.
*/
#define BASE_REG_CLASS GENERAL_REGS

/*
	General registers are valid for index addressing.
	TODO: HIP doesn't support index addressing, so maybe this can be removed.
*/
#define INDEX_REG_CLASS GENERAL_REGS

/*
	Nonzero if register number REGNO is suitable for use as a base
	register in operand addresses.
*/
#define REGNO_OK_FOR_BASE_P(REGNO)	\
	(	\
		(REGNO) <= HIP_LAST_GENERAL_REGISTER	\
		||	\
		(reg_renumber[REGNO] > 0 && reg_renumber[REGNO] <= HIP_LAST_GENERAL_REGISTER)	\
	)

/*
	Nonzero if register number REGNO is suitable for use as a base
	register in operand addresses.
	TODO: Try to remove this.
*/
#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P (REGNO)

/*
	TODO: Probably not needed on HIP.
	Places additional restrictions on the register class to use when it is
	necessary to copy value X into a register in class CLASS. The value is a register class;
	perhaps CLASS, or perhaps another, smaller class.
*/
#define PREFERRED_RELOAD_CLASS(X, CLASS)	\
	hip_preferred_reload_class (X, CLASS)

/*
	TODO: Probably not needed on HIP.
	Like PREFERRED_RELOAD_CLASS, but for output reloads instead of input reloads.
*/
#define PREFERRED_OUTPUT_RELOAD_CLASS(X, CLASS)	\
	hip_preferred_output_reload_class (X, CLASS)
/*
	Maximum number of consecutive registers of class CLASS needed to hold a value of mode MODE.
	This is closely related to the macro HARD_REGNO_NREGS. In fact, the value of the
	macro CLASS_MAX_NREGS (CLASS, MODE) should be the maximum value of HARD_REGNO_NREGS (REGNO, MODE)
	for all REGNO values in the class CLASS.
	This macro helps control the handling of multiple-word values in the reload pass.
*/
#define CLASS_MAX_NREGS(CLASS, MODE) HARD_REGNO_NREGS (CLASS, MODE)



/* Node: !!! 17.9 Obsolete Macros for Defining Constraints */

/*
	TODO: These constraints are all wrong for HIP.
*/

/*
	TODO: HIP doesn't support floats.
*/
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)	\
	hip_const_double_ok_for_letter_p (VALUE, C)

/*
	Machine-dependent operand constraint letters that specify particular ranges of integer values.
*/
#define CONST_OK_FOR_LETTER_P(VALUE, C)	\
	hip_const_ok_for_letter_p (VALUE, C)

/*
	Optional machine-dependent constraint letters
	that can be used to segregate specific types of operands,
	usually memory references, for the target machine.
	Any letter that is not elsewhere defined and not matched by
	REG_CLASS_FROM_LETTER or REG_CLASS_FROM_CONSTRAINT may be used.
*/
/*
#define EXTRA_CONSTRAINT(VALUE, C)	\
	hip_extra_constraint (VALUE, C, HIP_REG_OK_STRICT)
*/

/*
	The register letter 'r', corresponding to class GENERAL_REGS,
	will not be passed to this macro, so there is not need to handle it.
*/
#define REG_CLASS_FROM_LETTER(CHAR)	NO_REGS

/*
	These macros are obsolete, new ports should use the target hook
	TARGET_SECONDARY_ RELOAD instead.
	This indicates to the reload phase that it may need to allocate
	at least one register for a reload in addition to the register to contain the data.
*/
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)	\
	hip_secondary_reload_class (CLASS, MODE, X, 1)
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X)	\
	hip_secondary_reload_class (CLASS, MODE, X, 0)


/* Node: Frame Layout */

/*
	Stacks grows downward, stack pointer decremented after storing register.
*/
#define STACK_GROWS_DOWNWARD
#define STACK_PUSH_CODE POST_DEC
#define FRAME_GROWS_DOWNWARD 1

/*
	Offset from the frame pointer to the first local variable slot to be allocated.
*/
#define STARTING_FRAME_OFFSET \
	hip_starting_frame_offset ()

/*
	Offset from the argument pointer register to the first argument's address.
*/
#define FIRST_PARM_OFFSET(FUNDECL) 0

/*
	RTL representing the address in a stack frame where the
	pointer to the caller’s frame is stored. Assume that FRAMEADDR is an RTL expression
	for the address of the stack frame itself.
*/
#define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR)	\
	hip_dynamic_chain_address (FRAMEADDR)

/*
	TODO: Don't seem to need this.
	Produces the machine-specific code to setup the stack
	so that arbitrary frames can be accessed.
*/
#define SETUP_FRAME_ADDRESSES()	\
	hip_setup_frame_addresses ()

/*
	RTL representing the value of the return address for the frame COUNT steps up
	from the current frame, after the prologue.
	FRAME is the frame pointer of the COUNT frame.
*/
#define RETURN_ADDR_RTX(COUNT, FRAME)	\
	hip_return_addr_rtx (COUNT, FRAME)

/*
	RTL representing the location of the incoming return
	address at the beginning of any function, before the prologue. This RTL is either a
	REG, indicating that the return value is saved in ‘REG’, or a MEM representing a location
	in the stack.
	To support DWARF 2.
*/
#define INCOMING_RETURN_ADDR_RTX \
	gen_rtx_REG (Pmode, HIP_INCOMING_RETURN_ADDRESS_REGNUM)

/*
	This needs to be defined like this because INCOMING_RETURN_ADDR_RTX is REG.
*/
#define DWARF_FRAME_RETURN_COLUMN \
	DWARF_FRAME_REGNUM (HIP_INCOMING_RETURN_ADDRESS_REGNUM)

/*
	To support DWARF 2.
	Offset, in bytes, from the value of the stack pointer register
	to the top of the stack frame at the beginning of any function, before the prologue.
	The top of the frame is defined to be the value of the
	stack pointer in the previous frame, just before the call instruction.
*/
#define INCOMING_FRAME_SP_OFFSET 0


/* Node: Stack Checking */
/* (empty) */


/* Node: Exception Handling */
/*
	These macros mainly support DWARF 2.
	TODO: Probably best to remove them.
*/

#define EH_RETURN_DATA_REGNO(N)	\
	hip_eh_return_data_regno (N)

#define EH_RETURN_STACKADJ_RTX \
	hip_eh_return_stackadj_rtx ()

#define EH_RETURN_HANDLER_RTX \
	hip_eh_return_handler_rtx ()

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)	\
	hip_asm_preferred_eh_data_format (CODE, GLOBAL)


/* Node: Frame Registers */

/*
	Register used for the stack pointer. Must be fixed.
*/
#define STACK_POINTER_REGNUM HIP_STACK_POINTER_REGNUM

/*
	Register used for the frame pointer.
*/
#define FRAME_POINTER_REGNUM HIP_FRAME_POINTER_REGNUM

/*
	The register number of the arg pointer register, which is used to access the function's
	argument list. On some machines, this is the same as the frame pointer register.
*/
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM /* HIP_ARG_POINTER_REGNUM */

/*
	Register numbers used for passing a function's static chain pointer.
	TOOD: If the static chain is passed in memory, these macros should not be defined;
	instead, the TARGET_STATIC_CHAIN hook should be used.
*/
#define STATIC_CHAIN_REGNUM HIP_STATIC_CHAIN_REGNUM


/* Node: Eliminating Frame Pointer and Arg Pointer */

/*
	TODO: Probably isn't right.
	Stores in the variable DEPTH the difference between the frame
	pointer and the stack pointer values immediately after the function prologue.
*/
#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0


/* Node: Stack Arguments */

/*
	TODO: Perhaps replace this with PUSH_ARGS 0 ?
	HIP does not implement push and pop instructions.
 */
/* #define ACCUMULATE_OUTGOING_ARGS 1 */

/*
	If nonzero, push insns will be used to pass outgoing arguments.
	If the target machine does not have a push instruction, set it to zero.
*/
#define PUSH_ARGS 0


/* Node: Register Arguments */

/*
	Type for declaring a variable that is used as the first argument of TARGET_FUNCTION_ARG
	and other related values.
*/
typedef struct
{
	int regs;
	int lib;
} CUMULATIVE_ARGS;

/*
	17.10.7 Passing Arguments in Registers

	TODO: Probably don't need this.
	Initializes variable CUM for the state at the beginning of the argument list.
*/
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS)	\
	((CUM).regs = 0, (CUM).lib = ((LIBNAME) != 0))

/*
	Nonzero if REGNO is the number of a hard register in which
	function arguments are sometimes passed. This does not include implicit arguments
	such as the static chain and the structure-value address.
*/
#define FUNCTION_ARG_REGNO_P(REGNO)		\
	hip_function_arg_regno_p (REGNO, 0)


/* Node: Caller Saves */
/*
	(empty)
*/


/* Node: Function Entry */

/*
	See hip.c
	TARGET_ASM_FUNCTION_PROLOGUE		hip_target_asm_function_prologue
	TARGET_ASM_FUNCTION_END_PROLOGUE	hip_target_asm_function_end_prologue
	TARGET_ASM_FUNCTION_EPILOGUE		hip_target_asm_function_epilogue

	TODO: Not sure if these are needed, since they don't do anything useful.
*/

/*
	Nonzero for registers that are used by the epilogue or the 'return' pattern.
	The stack and frame pointer registers are already assumed to be used as needed.

	We need to say that the epilogue uses the return address, so the
	initial-value machinery restores it.  FIXME: Some targets
	conditionalize on "reload_completed &&".  Investigate difference.
	FIXME: Not needed if nonlocal_goto_stack_level.
*/
#define EPILOGUE_USES(REGNO)	\
	((REGNO) == HIP_INCOMING_RETURN_ADDRESS_REGNUM)


/* Node: Profiling */

/*
	Output to FILE some assembler code to call the profiling subroutine mcount.
*/
#define FUNCTION_PROFILER(FILE, LABELNO)	\
	hip_function_profiler (FILE, LABELNO)


/* Node: Trampolines */

/*
	A trampoline is a small piece of code that is created at run time when the address of
	a nested function is taken. It normally resides on the stack, in the stack frame of the
	containing function. These macros tell GCC how to generate code to allocate and initialize
	a trampoline.

	TODO: May need to be a little more complex since HIP is RISC.
*/

/*
	Size in bytes of the trampoline.
*/
#define TRAMPOLINE_SIZE (4*UNITS_PER_WORD)

/*
	Alignment required for trampolines, in bits.
*/
#define TRAMPOLINE_ALIGNMENT BITS_PER_WORD


/* Node: Addressing Modes */

/*
	1 if the RTX X is a constant which is a valid address.
*/
#define CONSTANT_ADDRESS_P(X)	\
	hip_constant_address_p (X)

/*
	TODO: Is 2 correct?
	Maximum number of registers that can appear in a valid memory address.
	Should be equal to the maximum number that TARGET_LEGITIMATE_ADDRESS_P would ever accept.

	Also see TARGET_LEGITIMATE_ADDRESS_P hip_legitimate_address_p.
*/
#define MAX_REGS_PER_ADDRESS 2

/*
	TODO: Manual doesn't seem to include REG_OK_FOR_BASE_P.
*/
#ifndef REG_OK_STRICT
# define REG_OK_FOR_BASE_P(X)	\
	(REGNO (X) <= HIP_LAST_GENERAL_REGISTER || REGNO (X) >= FIRST_PSEUDO_REGISTER)
#else
# define REG_OK_FOR_BASE_P(X)	\
	REGNO_OK_FOR_BASE_P (REGNO (X))
#endif /* REG_OK_STRICT */

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

/*
	Manual doesn't include this, but has TARGET_LEGITIMATE_CONSTANT_P.
	Returns true if x is a legitimate constant for a mode-mode immediate
	operand on the target machine. You can assume that x satisfies CONSTANT_P, so you
	need not check this.
*/
#define LEGITIMATE_CONSTANT_P(X)	\
	hip_legitimate_constant_p (X)


/* Node: Condition Code */
/*
	TODO: Not sure about conditions codes.
*/

/*
	Selects modes from the ones defined in hip-modes.def.
*/
#define SELECT_CC_MODE(OP, X, Y)	\
	hip_select_cc_mode (OP, X, Y)

/*
	Return true if it is sage to reverse a comparison whose mode is MODE.
*/
#define REVERSIBLE_CC_MODE(MODE)	\
	hip_reversible_cc_mode (MODE)


/* Node: Costs */

/*
	TODO: For now this seems to slightly punish special registers.
	Cost of moving data of mode MODE from a register in class FROM to one in class TO.
*/
#define REGISTER_MOVE_COST(MODE, FROM, TO)	\
	hip_register_move_cost (MODE, FROM, TO)

/*
	TODO: 0 seems to be OK.
	Indicates if accessing one byte from memory is slower than accessing a full word.
*/
#define SLOW_BYTE_ACCESS 0

/*
	TODO: Perhaps define MEMORY_MOVE_COST? Default is 4.
*/


/* Node: Sections */

/*
	A C expression whose value is a string, including spacing, containing the assembler
	operation that should precede instructions and read-only data. Normally "\t.text"
	is right.
*/
#define TEXT_SECTION_ASM_OP \
	"\t.text"

/*
	A C expression whose value is a string, including spacing, containing the assembler
	operation to identify the following data as writable initialized data. Normally
	"\t.data" is right.
	TODO: Remove hip_data_section_asm_op()
*/
#define DATA_SECTION_ASM_OP \
	"\t.data"


/* Node: 17.20 Position Independent Code PIC */

/*
	This section describes macros that help implement generation of position independent code.
	Currently empty.
*/


/* Node: File Framework */

/*
	How to begin a comment in the target assembler language.
*/
#define ASM_COMMENT_START "#"

/*
	These aren't currently functional. We just keep them as markers.
	String constant for text to be output before each asm statement or group of consecutive ones.
*/
#define ASM_APP_ON "%APP\n"
#define ASM_APP_OFF "%NO_APP\n"

/*
	Statement to output the string STRING to the stdio stream STREAM
*/
#define OUTPUT_QUOTED_STRING(STREAM, STRING)	\
	hip_output_quoted_string (STREAM, STRING, strlen (STRING))

/*
	Output assembly directives to switch to section name.
	Just use ELF format.
*/
#define TARGET_ASM_NAMED_SECTION default_elf_asm_named_section


/* Node: Data Output */

/*
	Output to the stdio stream STREAM an assembler instruction
	to assemble a string constant containing the LEN bytes at PTR.
*/
#define ASM_OUTPUT_ASCII(STREAM, PTR, LEN)	\
	hip_asm_output_ascii (STREAM, PTR, LEN)


/* Node: Uninitialized Data */

/*
	Output to the stdio stream ST the assembler definition
	of a common-label named N whose size is S bytes.
	The alignment A is specified as the number of bits.
*/
#define ASM_OUTPUT_ALIGNED_COMMON(ST, N, S, A)	\
	hip_asm_output_aligned_common (ST, N, S, A)

/*
	Output to the stdio stream ST the assembler definition
	of a local-common-label named N whose size is S bytes.
	The alignment A is specified as the number of bits.
*/
#define ASM_OUTPUT_ALIGNED_LOCAL(ST, N, S, A)	\
	hip_asm_output_aligned_local (ST, N, S, A)


/* Node: Label Output */

#define ASM_OUTPUT_LABEL(STREAM, NAME)	\
	hip_asm_output_label (STREAM, NAME)

/*
	More efficient label output for compiler-generated labels.
*/
#define ASM_OUTPUT_INTERNAL_LABEL(STREAM, NAME)	\
	hip_asm_output_internal_label (STREAM, NAME)

/*
	TODO: The MMIX comment suggest this may be needed, but now it is empty.
	Output to the stdio stream STREAM any text necessary for claiming a register REGNO
	for a global variable DECL with name NAME.
*/
#define ASM_DECLARE_REGISTER_GLOBAL(STREAM, DECL, REGNO, NAME)	\
	hip_asm_declare_register_global (STREAM, DECL, REGNO, NAME)

/*
	Manual mentions this in the context of TARGET_ASM_GLOBALIZE_LABEL,
	but does not provide any explanation.
*/
#define GLOBAL_ASM_OP "\t.global "

/*
	Output to the stdio stream STREAM some commands that will make the label NAME weak;
	that is, available for reference from other files
	but only used if no other definition is available.
*/
#define ASM_WEAKEN_LABEL(STREAM, NAME)	\
	hip_asm_weaken_label (STREAM, NAME)

/*
	TODO: Probably don't need this.
	Mark decl to be emitted as a public symbol such that
	extra copies in multiple translation units will be discarded by the linker.
*/
#define MAKE_DECL_ONE_ONLY(DECL)	\
	hip_make_decl_one_only (DECL)

/*
	Output to the stdio stream STREAM a reference in assembler syntax to a label named NAME.
*/
#define ASM_OUTPUT_LABELREF(STREAM, NAME)	\
	hip_asm_output_labelref (STREAM, NAME)

/*
	TODO: We insert a "_" to disambiguate against user symbols like L5.
*/
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
	sprintf (LABEL, "*%s_%ld", PREFIX, (long)(NUM))

/*
	TODO: Probably not needed, manual does not ducument.
*/
/* Insert "::"; these are rarer than internal labels.  FIXME: Make sure no
 ":" is seen in the object file; we don't really want that hipal
 feature visible there.  We don't want the default, which uses a dot;
 that'd be incompatible with hipal.  */
/*#define ASM_PN_FORMAT "%s::%lu"*/

/*
	A C statement to output to the stdio stream STREAM assembler code
	which defines (equates) the symbol NAME to have the value VALUE.
*/
#define ASM_OUTPUT_DEF(STREAM, NAME, VALUE)	\
	hip_asm_output_def (STREAM, NAME, VALUE)


/* Node: Macros for Initialization */

/*
	TODO: Probably not needed on HIP.
*/

#define INIT_SECTION_ASM_OP "\t.section .init,\"ax\""
#define FINI_SECTION_ASM_OP "\t.section .fini,\"ax\""

#define OBJECT_FORMAT_ELF


/* Node: Instruction Output */

/*
	The special register names must be prefixed with ":", since they're affected by PREFIX.
	We provide the non-colon names as additional names.
	This is what translates register numbers in the compiler into assembler language.
*/
#define REGISTER_NAMES \
{ \
	"r0",	"r1",	"r2",	"r3",	"r4",	"r5",	"r6",	"r7",	\
	"r8",	"r9",	"r10",	"r11",	"r12",	"r13",	"r14",	"r15",	\
	"r16",	"r17",	"r18",	"r19",	"r20",	"r21",	"r22",	"r23",	\
	"r24",	"r25",	"r26",	"r27",	"r28",	"r29",	"r30",	"r31",	\
	":pc1",	":epc",	":i" \
}

/*
	This macro defines additional names for hard registers.
*/
#define ADDITIONAL_REGISTER_NAMES	\
{ \
	{"pc2", 32},	{":pc3", 32},	\
	{"epc", 33},	{":epc", 33},	\
	{"i", 34},		{":i", 34}		\
}

/*
	Output to stdio stream STREAM the assembler syntax for an instruction operand X.
	X is an RTL expression.
	CODE is a value that can be used to specify one of several ways of printing the operand.
*/
#define PRINT_OPERAND(STREAM, X, CODE)	\
	hip_print_operand (STREAM, X, CODE)

/*
	Evaluates to true if code is a valid punctuation character for
	use in the PRINT_OPERAND macro.
*/
#define PRINT_OPERAND_PUNCT_VALID_P(CODE)	\
	hip_print_operand_punct_valid_p (CODE)

/*
	Output to stdio stream STREAM the assembler syntax for
	an instruction operand that is a memory reference whose address is X.
*/
#define PRINT_OPERAND_ADDRESS(STREAM, X)	\
	hip_print_operand_address (STREAM, X)

/*
	Code for pushing and popping registers.
*/
#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)	\
	hip_asm_output_reg_push (STREAM, REGNO)
#define ASM_OUTPUT_REG_POP(STREAM, REGNO)	\
	hip_asm_output_reg_pop (STREAM, REGNO)


/* Node: Dispatch Tables */

/*
	Output to the stdio stream STREAM an assembler pseudo-instruction
	to generate a difference between two labels.
	VALUE and REL are the numbers of two internal labels.
*/
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)	\
	hip_asm_output_addr_diff_elt (STREAM, BODY, VALUE, REL)

/*
	Output to the stdio stream STREAM an
	assembler pseudo-instruction to generate a reference to a label.
	VALUE is the number of an internal label.
	This macro should be provided on machines where the addresses in a dispatch table
	are absolute.
*/
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)	\
	hip_asm_output_addr_vec_elt (STREAM, VALUE)


/* Node: Exception Region Output */
/* (empty) */


/* Node: Alignment Output */

/*
	Generate assembly directives for alignment.
	TODO: HIP doesn't have these directives, alignment is mandatory.
*/
#define ASM_OUTPUT_SKIP(STREAM, NBYTES)	\
	hip_asm_output_skip (STREAM, NBYTES)
#define ASM_OUTPUT_ALIGN(STREAM, POWER)	\
	hip_asm_output_align (STREAM, POWER)


/* Node: All Debuggers */

/*
	Returns the DBX register number for the compiler register number REGNO.
	TODO: Not sure about this.
*/
#define DBX_REGISTER_NUMBER(REGNO)	\
	hip_dbx_register_number (REGNO)


/* Node: DBX Options */


/* Node: DBX Hooks */


/* Node: File Names and DBX */


/* Node: SDB and DWARF */

#define DWARF2_DEBUGGING_INFO 1
#define DWARF2_ASM_LINE_DEBUG_INFO 1


/* Node: Misc */

/*
	An alias for a machine mode name.
	This is the machine mode that elements of a jump-table should have.

	FIXME: There's no way to get a PC-relative offset into tables for SImode,
	so for the moment we have absolute entries in DImode.
	When we're going ELF, these should be SImode and 1.
*/
#define CASE_VECTOR_MODE DImode
#define CASE_VECTOR_PC_RELATIVE 0

/*
	Define this macro if operations between registers with integral mode smaller than a word
	are always performed on the entire register.
	Most RISC machines have this property and most CISC machines do not.
*/
#define WORD_REGISTER_OPERATIONS

/*
	This regards isns that read memory in mem_mode.
	FIXME: We have a choice, which makes this yet another parameter to tweak.
	The gut feeling is currently that SIGN_EXTEND wins;
	"int" is more frequent than "unsigned int", and we have signed characters.
*/
#define LOAD_EXTEND_OP(MODE)	\
	(TARGET_ZERO_EXTEND ? ZERO_EXTEND : SIGN_EXTEND)

/*
	The maximum number of bytes that a single instruction can move quickly
	between memory and registers or between two memory locations.
*/
#define MOVE_MAX UNITS_PER_WORD

/*
	Safe to convert integer of INPREC bits to OUTPREC bits.
	*(Whatever is that supposed to mean.)
	http://elias.rhi.hi.is/gccint/gccint_165.html#IDX1717
*/
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/*
	32 bit machine mode for pointers.
*/
#define Pmode SImode

/*
	Machine mode used for memory references to functions being called,
	in call RTL expressions.
*/
#define FUNCTION_MODE QImode

/*
	C++ aid.
*/
#define NO_IMPLICIT_EXTERN_C

/*
	These instruct GCC on the use of label names.
*/
#define DOLLARS_IN_IDENTIFIERS 0
#define NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

#endif /* GCC_HIP_H */
