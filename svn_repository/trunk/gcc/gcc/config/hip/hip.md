;; Machine description for HIP
;; Copyright (C) 2009 Free Software Foundation, Inc.
;; Contributed by Anthony Green <green@hiplogic.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.	If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; HIP specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

(define_constants
	[
		(HIP_ZERO 0)
		(HIP_CC 1)
		(HIP_FP 29)
		(HIP_SP 30)
		(HIP_RA 31)
	]
)

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
	[(const_int 0)]
	""
	"nop"
)

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, =r, =r")
		(plus:SI
			(match_operand:SI 1 "register_operand"	"r, r, %r")
			(match_operand:SI 2 "hip_reg_or_int"	"I, N, r")
		)
	)]
	""
	"@
	addui	%0, %1, %2
	subui	%0, %1, %n2
	addu	%0, %1, %2"
)

(define_insn "subsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, =r, =r")
		(minus:SI
			(match_operand:SI 1 "register_operand"	"r, r, r")
			(match_operand:SI 2 "hip_reg_or_int"	"I, T, r")
		)
	)]
	""
	"@
	subui	%0, %1, %2
	subui	%0, %1, %2
	subu	%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "negsi2"
	[(set
		(match_operand:SI 0 "register_operand" "=r")
		(neg:SI (match_operand:SI 1 "register_operand" "r"))
	)]
	""
	"subu	%0, r0, %1"
)

(define_insn "one_cmplsi2"
	[(set
		(match_operand:SI 0 "register_operand" "=r")
		(not:SI (match_operand:SI 1 "register_operand" "r"))
	)]
	""
	"not	%0, %1, r0"
)

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(and:SI
			(match_operand:SI 1 "register_operand" "r, %r")
			(match_operand:SI 2 "register_operand" "I, r")
		)
	)]
	""
	"@
	andi	%0, %1, %2
	and		%0, %1, %2"
)

(define_insn "xorsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(xor:SI
			(match_operand:SI 1 "register_operand" "r, r")
			(match_operand:SI 2 "register_operand" "I, r")
		)
	)]
	""
	"@
	xori	%0, %1, %2
	xor		%0, %1, %2"
)

(define_insn "iorsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(ior:SI
			(match_operand:SI 1 "register_operand" "r, r")
			(match_operand:SI 2 "register_operand" "I, r")
		)
	)]
	""
	"@
	ori		%0, %1, %2
	or		%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Shifters
;; -------------------------------------------------------------------------

(define_insn "ashlsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(ashift:SI
			(match_operand:SI 1 "register_operand" "r, r")
			(match_operand:SI 2 "register_operand" "I, r")
		)
	)]
	""
	"@
	slli	%0, %1, %2
	sll		%0, %1, %2"
)

(define_insn "ashrsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(ashiftrt:SI
			(match_operand:SI 1 "register_operand" "r, r")
			(match_operand:SI 2 "register_operand" "I, r")
		)
	)]
	""
	"@
	srai	%0, %1, %2
	sra		%0, %1, %2"
)

(define_insn "lshrsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(lshiftrt:SI
			(match_operand:SI 1 "register_operand" "r, r")
			(match_operand:SI 2 "register_operand" "I, r")
		)
	)]
	""
	"@
	srli	%0, %1, %2
	srl		%0, %1, %2"
)

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_insn "push_parameter"
	[(set
		(mem:SI
			(post_dec:SI
				(match_operand:SI 0 "register_operand" "r")
			)
		)
		(match_operand:SI 1 "register_operand" "r")
	)]
	""
	"sw		0(%0), %1\\n	subui	%0, %0, 4"
)

;; Push a register onto the stack
(define_insn "movsi_push"
	[
		(set
			(mem:SI (reg:SI HIP_SP))
			(match_operand:SI 0 "register_operand" "r")
		)
		(post_dec:SI (reg:SI HIP_SP))
	]
	""
{
	operands[1] = stack_pointer_rtx;
	return "sw		0(%1), %0";
}
)

;; Pop a register from the stack
(define_insn "movsi_pop"
	[
		(pre_inc:SI (reg:SI HIP_SP))
		(set
			(match_operand:SI 1 "register_operand" "=r")
			(mem:SI (match_operand:SI 0 "register_operand" "r"))
		)
	]
	""
{
	operands[0] = stack_pointer_rtx;
	return "lw		%1, 0(%0)";
}
)

(define_expand "movsi"
	[(set
		(match_operand:SI 0 "general_operand" "")
		(match_operand:SI 1 "general_operand" "")
	)]
	""
{
	/* If this is a store, force the value into a register.	*/
	if(!(reload_in_progress || reload_completed))
	{
		if(MEM_P(operands[0]))
		{
			operands[1] = force_reg(SImode, operands[1]);
			if(MEM_P(XEXP(operands[0], 0)))
				operands[0] = gen_rtx_MEM(SImode, force_reg(SImode, XEXP (operands[0], 0)));
		}
		else if(MEM_P(operands[1]) && MEM_P(XEXP (operands[1], 0)))
		{
			operands[1] = gen_rtx_MEM(SImode, force_reg(SImode, XEXP(operands[1], 0)));
		}
	}
}
)

(define_insn "*movsi"
	[(set
		(match_operand:SI 0 "general_operand"				"=r, =r, =r, A, =r, B, =r, W, =r")
		(match_operand:SI 1 "hip_general_movsrc_operand"	" r,  T,  i, r,  A, r,  B, r,  W")
	)]
	"	register_operand(operands[0], SImode) ||
		register_operand(operands[1], SImode)"
	"@
	addu	%0, r0, %1
	lhi		%0, %1\\n	addui	%0, %0, %1
	addui	%0, r0, %1
	sw		%0(r0), %1
	lw		%0, %1(r0)
	sw		%0, %1
	lw		%0, %1
	sw		%0, %1
	lw		%0, %1"
)

(define_expand "movqi"
	[(set
		(match_operand:QI 0 "general_operand" "")
		(match_operand:QI 1 "general_operand" "")
	)]
	""
{
	/* If this is a store, force the value into a register.	*/
	if(MEM_P(operands[0]))
		operands[1] = force_reg(QImode, operands[1]);
}
)

(define_insn "*movqi"
	[(set
		(match_operand:QI 0 "general_operand"				"=r, =r, A, =r, B, =r, W, =r")
		(match_operand:QI 1 "hip_general_movsrc_operand"	" r,  i, r,  A, r,  B, r,  W")
	)]
	"	register_operand(operands[0], QImode) ||
		register_operand(operands[1], QImode)"
	"@
	addu	%0, r0, %1
	addui	%0, r0, %1
	sb		%0(r0), %1
	lb		%0, %1(r0)
	sb		%0, %1
	lb		%0, %1
	sb		%0, %1
	lb		%0, %1"
)

(define_expand "movhi"
	[(set
		(match_operand:HI 0 "general_operand" "")
		(match_operand:HI 1 "general_operand" "")
	)]
	""
{
	/* If this is a store, force the value into a register.	*/
	if(MEM_P(operands[0]))
		operands[1] = force_reg(HImode, operands[1]);
}
)

(define_insn "*movhi"
	[(set
		(match_operand:HI 0 "general_operand"				"=r, =r, A, =r, B, =r, W, =r")
		(match_operand:HI 1 "hip_general_movsrc_operand"	" r,  i, r,  A, r,  B, r,  W")
	)]
	"	register_operand(operands[0], HImode) ||
		register_operand(operands[1], HImode)"
	"@
	addu	%0, r0, %1
	addui	%0, r0, %1
	sh		%0(r0), %1
	lh		%0, %1(r0)
	sh		%0, %1
	lh		%0, %1
	sh		%0, %1
	lh		%0, %1"
)

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_code_iterator comp		[lt ltu gt gtu ne eq])
(define_code_iterator compnot	[ge geu le leu])

(define_code_attr CMP [
	(lt "lt") (ltu "ltu") (gt "gt") (gtu "gtu") (ne "ne") (eq "eq")
])

(define_code_attr CMPNOT [
	(ge "lt") (geu "ltu") (le "gt") (leu "gtu")
])

(define_expand "cstoresi4"
	[(set
		(match_operand:SI 0 "register_operand")
		(match_operator:SI 1 "comparison_operator" [
			(match_operand:SI 2 "register_operand")
			(match_operand:SI 3 "hip_reg_or_int")
		])
	)]
	""
)

(define_insn "*s<comp:code>"
	[(set
		(match_operand:SI 0 "register_operand" "=r, =r")
		(comp:SI
			(match_operand:SI 1 "register_operand"	"r, r")
			(match_operand:SI 2 "hip_reg_or_int"	"r, I")
		)
	)]
	""
	"@
	s<CMP>		%0, %1, %2
	s<CMP>i	%0, %1, %2"
)

(define_insn "*s<compnot:code>"
	[(set
		(match_operand:SI 0 "register_operand" "=r, =r")
		(compnot:SI
			(match_operand:SI 1 "register_operand"	"r, r")
			(match_operand:SI 2 "hip_reg_or_int"	"r, I")
		)
	)]
	""
{
	switch(which_alternative)
	{
		case 0:
			return "s<CMPNOT>		%0, %1, %2\n	xori	%0, %0, 1";
		case 1:
			/* TODO: Change constants accordingly? */
			return "s<CMPNOT>i	%0, %1, %2\n	xori	%0, %0, 1";
		default:
			abort();
	}
}
)

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_expand "cbranchsi4"
	[(set
		(pc)
		(if_then_else
			(match_operator 0 "comparison_operator" [
				(match_operand:SI 1 "register_operand")
				(match_operand:SI 2 "hip_reg_or_int")
			])
			(label_ref (match_operand 3 ""))
			(pc)
		)
	)]
	""
{
	rtx condition_reg = gen_rtx_REG(SImode, HIP_CC);
	emit_insn(gen_cstoresi4(condition_reg, operands[0], operands[1], operands[2]));
	emit_jump_insn(gen_condjump(condition_reg, operands[3]));
	DONE;
}
)

(define_insn "condjump"
	[(set
		(pc)
		(if_then_else
			(ne
				(match_operand:SI 0 "hip_reg_or_int"	"")
				(const_int 0)
			)
			(label_ref (match_operand 1))
			(pc)
		)
	)]
	""
	"bne		%0, %l1"
)

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

(define_expand "call"
	[
		(clobber (reg:SI HIP_RA))
		(call
			(match_operand:HI 0 "memory_operand" "")
			(match_operand 1 "general_operand" "")
		)
	]
	""
{
	gcc_assert(MEM_P(operands[0]));
}
)

(define_insn "*call"
	[(call
		(mem:HI
			(match_operand:SI 0 "nonmemory_operand" "i, r")
		)
		(match_operand 1 "" "")
	)]
	""
{
	operands[1] = gen_rtx_REG(SImode, HIP_RA);
	switch(which_alternative)
	{
		case 0:
			return "call	%1, %0(r0)";
		case 1:
			return "call	%1, 0(%0)";
		default:
			break;
	}
}
)

(define_expand "call_value"
	[
		(clobber (reg:SI HIP_RA))
		(set
			(match_operand 0 "" "")
			(call
				(match_operand:HI 1 "memory_operand" "")
				(match_operand 2 "" "")
			)
		)
	]
	""
{
	gcc_assert(MEM_P(operands[1]));
}
)

(define_insn "*call_value"
	[(set
		(match_operand 0 "register_operand" "=r, =r")
		(call
			(mem:HI (match_operand:SI 1 "general_operand" "i, r"))
			(match_operand 2 "" "")
		)
	)]
	""
{
	operands[2] = gen_rtx_REG(SImode, HIP_RA);
	switch(which_alternative)
	{
		case 0:
			return "call	%2, %1(r0)";
		case 1:
			return "call	%2, 0(%1)";
		default:
			break;
	}
}
)

(define_insn "indirect_jump"
	[(set
		(pc) (match_operand:SI 0 "nonimmediate_operand" "r")
	)]
	""
	"j		0(%0)"
)

(define_insn "jump"
	[(set
		(pc) (label_ref (match_operand 0 "" ""))
	)]
	""
	"j		%l0(r0)"
)


;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
	[(clobber (const_int HIP_FP))]
	""
{
	hip_expand_prologue();
	DONE;
}
)

(define_expand "epilogue"
	[(return)]
	""
{
	hip_expand_epilogue();
	DONE;
}
)

(define_insn "returner"
	[(return)]
	"reload_completed"
{
	operands[0] = gen_rtx_REG(SImode, HIP_RA);
	return "j		0(%0)";
}
)
