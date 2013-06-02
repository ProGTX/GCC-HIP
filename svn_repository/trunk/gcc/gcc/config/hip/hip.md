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

; All instructions are four bytes long.
(define_attr "length" "" (const_int 4))

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
		(match_operand:SI 0 "register_operand" "=r, r, r")
		(plus:SI
			(match_operand:SI 1 "register_operand"		"r, r, r")
			(match_operand:SI 2 "hip_offset_operand"	"I, N, r")
		)
	)]
	""
	"@
	addi	%0, %1, %2
	subi	%0, %1, -%2
	add		%0, %1, %2"
)

(define_insn "subsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(minus:SI
			(match_operand:SI 1 "register_operand"		"r, r")
			(match_operand:SI 2 "hip_offset_operand"	"I, r")
		)
	)]
	""
	"@
	subi	%0, %1, %2
	sub		%0, %1, %2"
)

;(define_insn "mulsi3"
;	[(set
;		(match_operand:SI 0 "register_operand" "=r")
;		(mult:SI
;			(match_operand:SI 1 "register_operand" "0")
;			(match_operand:SI 2 "register_operand" "r")
;		)
;	)]
;	""
;	"mul.l	%0, %2")

;(define_insn "divsi3"
;	[(set
;		(match_operand:SI 0 "register_operand" "=r")
;		(div:SI
;	 	(match_operand:SI 1 "register_operand" "0")
;	 	(match_operand:SI 2 "register_operand" "r"))
;	)]
;	""
;	"div.l	%0, %2")

;(define_insn "udivsi3"
;	[(set
;		(match_operand:SI 0 "register_operand" "=r")
;		(udiv:SI
;	 	(match_operand:SI 1 "register_operand" "0")
;	 	(match_operand:SI 2 "register_operand" "r"))
;	)]
;	""
;	"udiv.l %0, %2")

;(define_insn "modsi3"
;	[(set
;		(match_operand:SI 0 "register_operand" "=r")
;		(mod:SI
;	 	(match_operand:SI 1 "register_operand" "0")
;	 	(match_operand:SI 2 "register_operand" "r"))
;	)]
;	""
;	"mod.l	%0, %2")

;(define_insn "umodsi3"
;	[(set
;		(match_operand:SI 0 "register_operand" "=r")
;		(umod:SI
;	 	(match_operand:SI 1 "register_operand" "0")
;	 	(match_operand:SI 2 "register_operand" "r"))
;	)]
;	""
;	"umod.l %0, %2")

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "negsi2"
	[(set
		(match_operand:SI 0 "register_operand" "=r")
		(neg:SI (match_operand:SI 1 "register_operand" "r"))
	)]
	""
	"sub	%0, r0, %1"
)

(define_insn "one_cmplsi2"
	[(set
		(match_operand:SI 0 "register_operand" "=r")
		(not:SI (match_operand:SI 1 "register_operand" "r"))
	)]
	""
	"not	%0, %1"
)

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_insn "andsi3"
	[(set
		(match_operand:SI 0 "register_operand" "=r, r")
		(and:SI
			(match_operand:SI 1 "register_operand" "r, r")
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

(define_insn "lshlsi3"
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

;; SImode

;; Push a register onto the stack
(define_insn "movsi_push"
	[(set
		(mem:SI (pre_dec:SI (reg:SI 1)))
		(match_operand:SI 0 "register_operand" "r")
	)]
	""
	"sw 0(r30), %0"
)

;; Pop a register from the stack
(define_insn "movsi_pop"
	[(set
		(match_operand:SI 1 "register_operand" "=r")
		(mem:SI (post_inc:SI (match_operand:SI 0 "register_operand" "r")))
	)]
	""
	"lw %0, 0(r30)"
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
				operands[0] = gen_rtx_MEM(SImode, force_reg (SImode, XEXP (operands[0], 0)));
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
		(match_operand:SI 0 "general_operand"				"=r,r, r, W, r, A, r, B, r")
		(match_operand:SI 1 "hip_general_movsrc_operand"	"O, r, i, r, W, r, A, r, B")
	)]
	"	register_operand(operands[0], SImode) ||
		register_operand(operands[1], SImode)"
	"@
	 add		%0, r0, r0
	 add		%0, r0, %1
	 addui		%0, r0, %1
	 swW		%0, %1
	 lwW		%0, %1
	 swA		%0(r0), %1
	 lwA		%0, %1(r0)
	 swB		%0, %1
	 lwB		%0, 0(%1)"
	[(set_attr "length"	"4,4,4,4,4,4,4,4,4")]
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
		(match_operand:QI 0 "general_operand"				"=r,r, r, W, r, A, r, B, r")
		(match_operand:QI 1 "hip_general_movsrc_operand"	"O, r, i, r, W, r, A, r, B")
	)]
	"	register_operand(operands[0], QImode) ||
		register_operand(operands[1], QImode)"
	"@
	 add		%0, r0, r0
	 add		%0, %1, r0
	 addui		%0, r0, %1
	 sb		0(%0), %1
	 lb		%0, 0(%1)
	 sb		%0(r0), %1
	 lb		%0, %1(r0)
	 sb		0(%0), %1
	 lb		%0, 0(%1)"
	[(set_attr "length"	"4,4,4,4,4,4,4,4,4")]
)

(define_expand "movhi"
	[(set
		(match_operand:HI 0 "general_operand" "")
		(match_operand:HI 1 "general_operand" "")
	)]
	""
{
	/* If this is a store, force the value into a register.	*/
	if(MEM_P (operands[0]))
		operands[1] = force_reg(HImode, operands[1]);
}
)

(define_insn "*movhi"
	[(set
		(match_operand:HI 0 "general_operand"				"=r,r, r, W, r, A, r, B, r")
		(match_operand:HI 1 "hip_general_movsrc_operand"	"O, r, i, r, W, r, A, r, B")
	)]
	"	register_operand(operands[0], HImode) ||
		register_operand(operands[1], HImode)"
	"@
	 add		%0, r0, r0
	 add		%0, %1, r0
	 addui		%0, r0, %1
	 sh		0(%0), %1
	 lh		%0, 0(%1)
	 sh		%0(r0), %1
	 lh		%0, %1(r0)
	 sh		0(%0), %1
	 lh		%0, 0(%1)"
	[(set_attr "length"	"4,4,4,4,4,4,4,4,4")]
)

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

(define_constants
	[(CC_REG 11)]
)

(define_expand "cbranchsi4"
	[(set
		(reg:CC CC_REG)
		(compare:CC
			(match_operand:SI 1 "general_operand" "")
			(match_operand:SI 2 "general_operand" "")
		)
	)
 	(set
		(pc)
		(if_then_else
			(match_operator:CC 0 "comparison_operator"
				[(reg:CC CC_REG) (const_int 0)]
			)
			(label_ref (match_operand 3 "" ""))
			(pc)
		)
	)]
	""
{
	/* Force the compare operands into registers.	*/
	if (GET_CODE (operands[1]) != REG)
		operands[1] = force_reg (SImode, operands[1]);
	if (GET_CODE (operands[2]) != REG)
		operands[2] = force_reg (SImode, operands[2]);
}
)

(define_insn "*cmpsi"
	[(set
		(reg:CC CC_REG)
		(compare
			(match_operand:SI 0 "register_operand" "r")
			(match_operand:SI 1 "register_operand"	"r")
		)
	)]
	""
	"cmp	%0, %1"
)


;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])

(define_code_attr CC
[
	(ne "ne")	(eq "eq")	(lt "lt")	(ltu "ltu")
	(gt "gt")	(gtu "gtu")	(ge "ge")	(le "le")
	(geu "geu")	(leu "leu")
]
)

(define_code_attr rCC
[
	(ne "eq")	(eq "ne")	(lt "ge")	(ltu "geu")
	(gt "le")	(gtu "leu")	(ge "lt")	(le "gt")
	(geu "ltu")	(leu "gtu")
]
)

(define_insn "*b<cond:code>"
	[(set
		(pc)
		(if_then_else
			(cond (reg:CC CC_REG) (const_int 0))
			(label_ref (match_operand 0 "" ""))
			(pc)
		)
	)]
	""
{
	if (get_attr_length(insn) == 4)
		return "b<CC>	 %l0";
	else
		return "b<rCC>	 .+6\n\tjmpa	 %l0";
}
	[(set
		(attr "length")
		(if_then_else
			(lt
				(abs (minus (pc) (match_dup 0)))
				(const_int 1022)
			)
			(const_int 4)
			(const_int 4)
		)
	)]
)

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

(define_expand "call"
	[(call
		(match_operand:QI 0 "memory_operand" "")
		(match_operand 1 "general_operand" "")
	)]
	""
{
	gcc_assert (MEM_P (operands[0]));
}
)

(define_insn "*call"
	[(call
		(mem:QI (match_operand:SI 0 "nonmemory_operand" "i, r"))
		(match_operand 1 "" "")
	)]
	""
	"@
	 jsra	%0
	 jsr	%0"
	[(set_attr "length"	"4,4")]
)

(define_expand "call_value"
	[(set
		(match_operand 0 "" "")
		(call
			(match_operand:QI 1 "memory_operand" "")
			(match_operand 2 "" "")
		)
	)]
	""
{
	gcc_assert (MEM_P (operands[1]));
}
)

(define_insn "*call_value"
	[(set
		(match_operand 0 "register_operand" "=r")
		(call
			(mem:QI (match_operand:SI 1 "immediate_operand" "i"))
			(match_operand 2 "" "")
		)
	)]
	""
	"jsra	 %1"
	[(set_attr "length"	"4")]
)

(define_insn "*call_value_indirect"
	[(set
		(match_operand 0 "register_operand" "=r")
		(call
			(mem:QI (match_operand:SI 1 "register_operand" "r"))
			(match_operand 2 "" "")
		)
	)]
	""
	"jsr	%1"
)

(define_insn "indirect_jump"
	[(set
		(pc) (match_operand:SI 0 "nonimmediate_operand" "r")
	)]
	""
	"j	0(%0)"
)

(define_insn "jump"
	[(set
		(pc) (label_ref (match_operand 0 "" ""))
	)]
	""
	"j	%l0(r0)"
	[(set_attr "length"	"4")]
)


;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
	[(clobber (const_int 0))]
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
	"j 0(r31)"
)
