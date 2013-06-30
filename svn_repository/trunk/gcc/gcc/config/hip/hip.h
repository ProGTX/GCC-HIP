/* Target Definitions for hip.
	Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.
	Contributed by Anthony Green.

	This file is part of GCC.

	GCC is free software; you can redistribute it and/or modify it
	under the terms of the GNU General Public License as published
	by the Free Software Foundation; either version 3, or (at your
	option) any later version.

	GCC is distributed in the hope that it will be useful, but WITHOUT
	ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
	or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
	License for more details.

	You should have received a copy of the GNU General Public License
	along with GCC; see the file COPYING3. If not see
	<http://www.gnu.org/licenses/>.
*/

#ifndef GCC_HIP_H
#define GCC_HIP_H

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s crti.o%s crtbegin.o%s"

/*
	Provide an ENDFILE_SPEC appropriate for svr4. Here we tack on our own
	magical crtend.o file (see crtstuff.c) which provides part of the
	support for getting C++ file-scope static object constructed before
	entering `main', followed by the normal svr3/svr4 "finalizer" file,
	which is either `gcrtn.o' or `crtn.o'.
*/

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/*
	Provide a LIB_SPEC appropriate for svr4. Here we tack on the default
	standard C library (unless we are building a shared library) and
	the simulator BSP code.
*/

#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

#undef LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} \
	%{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"

/*
	Layout of Source Language Data Types
*/

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64

#define FLOAT_TYPE_SIZE			32
#define DOUBLE_TYPE_SIZE		64
#define LONG_DOUBLE_TYPE_SIZE	64

#define DEFAULT_SIGNED_CHAR 1

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/*
	Registers...

	r0			- constant zero 32-bit register
	r0 to r23	- general purpose 32-bit registers
	r24, r25	- first and second argument 32-bit registers
	r26			- long jump base 32-bit register
	r27			- long call base 32-bit register
	r28			- return value 32-bit register
	r31			- return address 32-bit register

	qfp
	qap

	Special Registers...

	pc	- 32-bit program counter
	epc	- 32-bit exception return address
	i	- 1-bit exception enabler

*/

#define REGISTER_NAMES {											\
	"r0",	"r1",	"r2",	"r3",	"r4",	"r5",	"r6",	"r7",	\
	"r8",	"r9",	"r10",	"r11",	"r12",	"r13",	"r14",	"r15",	\
	"r16",	"r17",	"r18",	"r19",	"r20",	"r21",	"r22",	"r23",	\
	"r24",	"r25",	"r26",	"r27",	"r28",	"r29",	"r30",	"r31",	\
	"?fp",	"?ap",	"pc",	"epc",	"i" }

enum HIP_REGNUMS
{
	HIP_R0,		HIP_R1,		HIP_R2,		HIP_R3,
	HIP_R4,		HIP_R5,		HIP_R6,		HIP_R7,
	HIP_R8,		HIP_R9,		HIP_R10,	HIP_R11,
	HIP_R12,	HIP_R13,	HIP_R14,	HIP_R15,
	HIP_R16,	HIP_R17,	HIP_R18,	HIP_R19,
	HIP_R20,	HIP_R21,	HIP_R22,	HIP_R23,
	HIP_R24,	HIP_R25,	HIP_R26,	HIP_R27,
	HIP_R28,	HIP_R29,	HIP_R30,	HIP_R31,
	HIP_NUMBER_HARD_REGS
};

#define HIP_ZERO	0	/* Constant zero register */
#define HIP_CC		1	/* Condition code register */
#define HIP_P1		24	/* First parameter */
#define HIP_P2		25	/* Second parameter */
#define HIP_BLJ		26	/* Base long jump */
#define HIP_BLC		27	/* Base long call */
#define HIP_RV		28	/* Return value */
#define HIP_FP		29	/* Frame pointer */
#define HIP_SP		30	/* Stack pointer */
#define HIP_RA		31	/* Return address */
#define HIP_QFP		32
#define HIP_QAP		33
#define HIP_PC		34
#define HIP_EPC		35
#define HIP_I		36
#define HIP_LAST_SAVED_REGISTER 27

#define FIRST_PSEUDO_REGISTER 37

enum reg_class
{
	NO_REGS,
	GENERAL_REGS,
	SPECIAL_REGS,
	ALL_REGS,
	LIM_REG_CLASSES
};


/*
	The following macro defines cover classes for Integrated Register
	Allocator. Cover classes is a set of non-intersected register
	classes covering all hard registers used for register allocation
	purpose. Any move between two registers of a cover class should be
	cheaper than load or store of the registers. The macro value is
	array of register classes with LIM_REG_CLASSES used as the end
	marker.
*/
#define IRA_COVER_CLASSES { GENERAL_REGS, LIM_REG_CLASSES }

#define REG_CLASS_CONTENTS											\
{	{ 0x00000000, 0x00000000 },	/* Empty
*/							\
	{ 0xFFFFFFFF, 0x00000003 }, /* r0 to r31, ?fp, ?ap
*/			\
	{ 0x00000000, 0x0000001C },	/* pc, epc, i
*/					\
	{ 0xFFFFFFFF, 0x0000001F }	/* All registers
*/					\
}

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES {	\
	"NO_REGS",				\
	"GENERAL_REGS",			\
	"SPECIAL_REGS",			\
	"ALL_REGS" }

/*
	Initializer that says which registers are used for fixed purposes all throughout the
	compiled code and are therefore not available for general allocation.
*/
#define FIXED_REGISTERS	{				\
	1, 0, 0, 0, 0, 0, 0, 0,	/* r0-r7*/	\
	0, 0, 0, 0, 0, 0, 0, 0,	/* r8-r15*/	\
	0, 0, 0, 0, 0, 0, 0, 0,	/* r16-23*/	\
	0, 0, 0, 0, 0, 0, 1, 0,	/* r24-31*/	\
	1, 1, 1, 1, 1		}

/*
	1 indicates a register that is clobbered (in general) by function calls or is a fixed register.
	This identifies the registers that are not available
	for general allocation of values that must live across function calls.
	If a register has 0 in CALL_USED_REGISTERS, the compiler automatically saves it on
	function entry and restores it on function exit, if the register is used within the function.
*/
#define CALL_USED_REGISTERS {			\
	1, 0, 0, 0, 0, 0, 0, 0,	/* r0-r7*/	\
	0, 0, 0, 0, 0, 0, 0, 0,	/* r8-r15*/	\
	0, 0, 0, 0, 0, 0, 0, 0,	/* r16-23*/	\
	1, 1, 0, 0, 1, 0, 1, 0,	/* r24-31*/	\
	1, 1, 1, 1, 1			}


/*
	A C expression that is nonzero if it is permissible to store a
	value of mode MODE in hard register number REGNO (or in several
	registers starting with that one). All gstore registers are
	equivalent, so we can set this to 1.
*/
#define HARD_REGNO_MODE_OK(R,M) 1

/*
	A C expression whose value is a register class containing hard
	register REGNO.
*/
#define REGNO_REG_CLASS(R) \
	( (R < HIP_PC) ? GENERAL_REGS : SPECIAL_REGS )

/*
	A C expression for the number of consecutive hard registers,
	starting at register number REGNO, required to hold a value of mode
	MODE.
*/
#define HARD_REGNO_NREGS(REGNO, MODE)				\
(	(GET_MODE_SIZE(MODE) + UNITS_PER_WORD - 1)			\
	/ UNITS_PER_WORD	)

/*
	A C expression that is nonzero if a value of mode MODE1 is
	accessible in mode MODE2 without copying.
*/
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/*
	A C expression for the maximum number of consecutive registers of
	class CLASS needed to hold a value of mode MODE.
*/
#define CLASS_MAX_NREGS(CLASS, MODE) \
	((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/*
	The Overall Framework of an Assembler File
*/

#undef ASM_SPEC
#define ASM_COMMENT_START ";"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""

/*
	Switch to the text or data segment.
*/

/*
	If this isn't defined, main function will generate a call to itself.
	But this macro doesn't seem to be used in the assembled file,
	so it's OK to just return an empty string.
	Initialization is handled by hip_file_start()
*/
#define INIT_SECTION_ASM_OP	""

#define TEXT_SECTION_ASM_OP	"\n\t.text\n"
#define DATA_SECTION_ASM_OP	"\n\t.data\n"

/*
	Assembler commands
*/

#define	ASM_OUTPUT_SKIP(stream, nbytes)	\
	hip_asm_output_skip(stream, nbytes)

#define ASM_OUTPUT_LOCAL(stream, name, size, rounded)	\
	fprintf(stream, "\t.local %s, %d, %d\n", name, size, rounded)

#define ASM_OUTPUT_COMMON(stream, name, size, rounded)	\
	fprintf(stream, "%s:\t.space %d\n", name, size)

#define ASM_OUTPUT_LABEL(stream, name)	\
	fprintf(stream, "%s:", name)

#define ASM_OUTPUT_FUNCTION_LABEL(stream, name, decl)	\
	fprintf(stream, "%s:\n", name)

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
	sprintf(LABEL, "%s%u", PREFIX, (unsigned) (NUM))

/*
	Assembler Commands for Alignment
*/

/*
	TODO: Why doesn't WinHIP support the align pseudo-instruction?
*/
#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
	fprintf(STREAM, "");
/*	fprintf(STREAM, "\t.align\t%d\n", 1 << POWER);*/

/*
	A C compound statement to output to stdio stream STREAM the
	assembler syntax for an instruction operand X.
*/
#define PRINT_OPERAND(STREAM, X, CODE) hip_print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_ADDRESS(STREAM, X) hip_print_operand_address (STREAM, X)

/*
	Output and Generation of Labels
*/

/*
	TODO: Labels seem to be repeating, don't know the reason.
	Putting a comment here is a functional work-around
*/
#define GLOBAL_ASM_OP "; "

/*
	Passing Arguments in Registers
*/

/*
	A C type for declaring a variable that is used as the first
	argument of `FUNCTION_ARG' and other related values.
*/
#define CUMULATIVE_ARGS unsigned int

/*
	A C statement (sans semicolon) for initializing the variable CUM
	for the state at the beginning of the argument list.
	For hip, the first arg is passed in register 2 (aka r0).
*/
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
	(CUM = HIP_P1)

/*
	How Scalar Function Values Are Returned
*/

/*
	STACK AND CALLING
*/


#define STACK_PUSH_CODE POST_DEC

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0

/*
	Offset from the frame pointer to the first local variable slot to
	be allocated.
*/
#define STARTING_FRAME_OFFSET 0

/*
	Offset from the argument pointer register to the first argument's
	address. On some machines it may depend on the data type of the
	function.
*/
#define FIRST_PARM_OFFSET(F) 12

#define PUSH_ARGS 1
#define PUSH_ROUNDING(BYTES) (BYTES)

#define STACK_GROWS_DOWNWARD

/*#define ARGS_GROW_DOWNWARD*/

/*
	Define this macro to nonzero value if the addresses of local variable slots
	are at negative offsets from the frame pointer.
*/
#define FRAME_GROWS_DOWNWARD 1

/*
	Define this macro as a C expression that is nonzero for registers that are
	used by the epilogue or the return pattern. The stack and frame
	pointer registers are already assumed to be used as needed.
*/
#define EPILOGUE_USES(R) (R == HIP_RA)

/*
	A C expression whose value is RTL representing the location of the
	incoming return address at the beginning of any function, before
	the prologue.
*/
#define INCOMING_RETURN_ADDR_RTX					\
	gen_frame_mem(Pmode, plus_constant(stack_pointer_rtx, UNITS_PER_WORD))

/*
	Describe how we implement __builtin_eh_return.
*/
#define EH_RETURN_DATA_REGNO(N)	((N) < 4 ? (N+2) : INVALID_REGNUM)

/*
	Store the return handler into the call frame.
*/
#define EH_RETURN_HANDLER_RTX	\
	gen_frame_mem(Pmode, plus_constant(frame_pointer_rtx, UNITS_PER_WORD))

/*
	Storage Layout
*/

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1

/*
	Alignment required for a function entry point, in bits.
*/
#define FUNCTION_BOUNDARY 32

/*
	Define this macro as a C expression which is nonzero if accessing
	less than a word of memory (i.e. a `char' or a `short') is no
	faster than accessing a word of memory.
*/
#define SLOW_BYTE_ACCESS 1

/*
	Number of storage units in a word; normally the size of a
	general-purpose register, a power of two from 1 or 8.
*/
#define UNITS_PER_WORD 4

/*
	Define this macro to the minimum alignment enforced by hardware
	for the stack pointer on this machine. The definition is a C
	expression for the desired alignment (measured in bits).
*/
#define STACK_BOUNDARY 32

/*
	Normal alignment required for function parameters on the stack, in
	bits. All stack parameters receive at least this much alignment
	regardless of data type.
*/
#define PARM_BOUNDARY 32

/*
	Alignment of field after `int : 0' in a structure.
*/
#define EMPTY_FIELD_BOUNDARY 32

/*
	No data type wants to be aligned rounder than this.
*/
#define BIGGEST_ALIGNMENT 32

/*
	The best alignment to use in cases where we have a choice.
*/
#define FASTEST_ALIGNMENT 32

/*
	Every structures size must be a multiple of 8 bits.
*/
#define STRUCTURE_SIZE_BOUNDARY 8

/*
	Look at the fundamental type that is used for a bit-field and use
	that to impose alignment on the enclosing structure.
	struct s {int a:8}; should have same alignment as "int", not "char".
*/
#define	PCC_BITFIELD_TYPE_MATTERS	1

/*
	Largest integer machine mode for structures. If undefined, the default
	is GET_MODE_SIZE(DImode).
*/
#define MAX_FIXED_MODE_SIZE 32

/*
	Make strings word-aligned so strcpy from constants will be faster.
*/
#define CONSTANT_ALIGNMENT(EXP, ALIGN)								\
(	(TREE_CODE (EXP) == STRING_CST && (ALIGN) < FASTEST_ALIGNMENT)	\
	? FASTEST_ALIGNMENT : (ALIGN)	)

/*
	Make arrays of chars word-aligned for the same reasons.
*/
#define DATA_ALIGNMENT(TYPE, ALIGN)								\
(	TREE_CODE (TYPE) == ARRAY_TYPE			&&					\
	TYPE_MODE (TREE_TYPE (TYPE)) == QImode	&&					\
	(ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN)	)

/*
	Set this nonzero if move instructions will actually fail to work
	when given unaligned data.
*/
#define STRICT_ALIGNMENT 1

/*
	Generating Code for Profiling
*/
#define FUNCTION_PROFILER(FILE,LABELNO) (abort (), 0)

/*
	Trampolines for Nested Functions.
*/
#define TRAMPOLINE_SIZE (2 + 6 + 6 + 2 + 2 + 6)

/*
	Alignment required for trampolines, in bits.
*/
#define TRAMPOLINE_ALIGNMENT 32

/*
	An alias for the machine mode for pointers.
*/
#define Pmode SImode

/*
	An alias for the machine mode used for memory references to
	functions being called, in `call' RTL expressions.
*/
#define FUNCTION_MODE HImode

/*
	The register number of the stack pointer register, which must also
	be a fixed register according to `FIXED_REGISTERS'.
*/
#define STACK_POINTER_REGNUM HIP_SP

/*
	The register number of the frame pointer register, which is used to
	access automatic variables in the stack frame.
*/
#define FRAME_POINTER_REGNUM HIP_QFP

/*
	The register number of the arg pointer register, which is used to
	access the function's argument list.
*/
#define ARG_POINTER_REGNUM HIP_QAP

#define HARD_FRAME_POINTER_REGNUM HIP_FP

#define ELIMINABLE_REGS										\
{	{ FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },	\
	{ ARG_POINTER_REGNUM,	HARD_FRAME_POINTER_REGNUM }	}

/*
	This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'. It
	specifies the initial difference between the specified pair of
	registers. This macro must be defined if `ELIMINABLE_REGS' is
	defined.
	TODO: This is just a work-around.
*/
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)	\
	(OFFSET) = hip_initial_elimination_offset((FROM), (TO))

/*
	A C expression that is nonzero if REGNO is the number of a hard
	register in which function arguments are sometimes passed.
*/
#define FUNCTION_ARG_REGNO_P(r) (r >= HIP_P1 && r <= HIP_P2)

/*
	A macro whose definition is the name of the class to which a valid
	base register must belong. A base register is one used in an
	address which is the register value plus a displacement.
*/
#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS NO_REGS

#define HARD_REGNO_OK_FOR_BASE_P(NUM)			\
(	(unsigned) (NUM) < FIRST_PSEUDO_REGISTER &&	\
	(	REGNO_REG_CLASS(NUM) == GENERAL_REGS ||	\
		(NUM) == HARD_FRAME_POINTER_REGNUM		\
	)											\
)

/*
	A C expression which is nonzero if register number NUM is suitable
	for use as a base register in operand addresses.
*/
#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(NUM)		\
(	HARD_REGNO_OK_FOR_BASE_P(NUM) ||	\
	HARD_REGNO_OK_FOR_BASE_P(reg_renumber[(NUM)])	)
#else
#define REGNO_OK_FOR_BASE_P(NUM)		\
(	(NUM) >= FIRST_PSEUDO_REGISTER ||	\
	HARD_REGNO_OK_FOR_BASE_P(NUM)	)
#endif

/*
	A C expression which is nonzero if register number NUM is suitable
	for use as an index register in operand addresses.
*/
#define REGNO_OK_FOR_INDEX_P(NUM) HIP_FP

/*
	The maximum number of bytes that a single instruction can move
	quickly between memory and registers or between two memory
	locations.
*/
#define MOVE_MAX 4
#define TRULY_NOOP_TRUNCATION(op,ip) 1

/*
	All load operations zero extend.
*/
#define LOAD_EXTEND_OP(MEM) ZERO_EXTEND

/*
	A C expression that is nonzero if X is a legitimate constant for
	an immediate operand on the target machine.
*/
#define LEGITIMATE_CONSTANT_P(X) 1

/*
	A number, the maximum number of registers that can appear in a
	valid memory address.
*/
#define MAX_REGS_PER_ADDRESS 1

/*
	An alias for a machine mode name. This is the machine mode that
	elements of a jump-table should have.
*/
#define CASE_VECTOR_MODE SImode

/*
	A C compound statement with a conditional `goto LABEL;' executed
	if X (an RTX) is a legitimate memory address on the target machine
	for a memory operand of mode MODE.
*/
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)		\
	if(1) {												\
		if(GET_CODE(X) == PLUS)							\
		{												\
			rtx op1,op2;								\
			op1 = XEXP(X,0);							\
			op2 = XEXP(X,1);							\
			if(	GET_CODE(op1) == REG	&&				\
				CONSTANT_ADDRESS_P(op2)	&&				\
				REGNO_OK_FOR_BASE_P(REGNO(op1))	)		\
			goto LABEL;									\
		}												\
		if(REG_P (X) && REGNO_OK_FOR_BASE_P(REGNO(X)))	\
			goto LABEL;									\
		if(	GET_CODE (X) == SYMBOL_REF	||				\
			GET_CODE (X) == LABEL_REF	||				\
			GET_CODE (X) == CONST	)					\
		goto LABEL;										\
	}

/*
	Run-time Target Specification
*/

#define TARGET_CPU_CPP_BUILTINS()	\
{									\
	builtin_define_std ("hip");		\
	builtin_define_std ("HIP");		\
}

#define HAS_LONG_UNCOND_BRANCH true

#endif /* GCC_HIP_H */
