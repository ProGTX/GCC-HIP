;; Constraint definitions for HIP
;; Copyright (C) 2009 Free Software Foundation, Inc.
;; Contributed by Anthony Green <green@hiplogic.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Constraints
;; -------------------------------------------------------------------------

(define_constraint "A"
	"An absolute address."
	(and
		(match_code "mem")
		(ior
			(match_test "GET_CODE(XEXP(op, 0)) == SYMBOL_REF")
			(match_test "GET_CODE(XEXP(op, 0)) == LABEL_REF")
			(match_test "GET_CODE(XEXP(op, 0)) == CONST")
		)
	)
)

(define_constraint "B"
	"A register indirect memory operand - base address."
	(and
		(match_code "mem")
		(match_test "REG_P(XEXP(op, 0)) && REGNO_OK_FOR_BASE_P(REGNO(XEXP(op, 0)))")
	)
)

(define_constraint "W"
	"An offset address."
	(and
		(match_code "mem")
		(match_test "GET_CODE(XEXP(op, 0)) == PLUS")
	)
)

(define_constraint "I"
	"A 16-bit constant (0..65535)"
	(and
		(match_code "const_int")
		(match_test "ival >= 0 && ival <= 65535")
	)
)

(define_constraint "N"
	"A 16-bit constant -(0..65535)"
	(and
		(match_code "const_int")
		(match_test "ival >= -65535 && ival <= 0")
	)
)

(define_constraint "T"
	"Integer constant that is too big to fit into offset"
	(and
		(match_code "const_int")
		(match_test "ival < -32768 || ival > 32767")
	)
)
