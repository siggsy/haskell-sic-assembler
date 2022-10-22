prog	START	0

loop	LDS	#in
	ADDR	X, S
	JSUB	poly

	ADD	#3
	RMO	A, X
	COMP	#len
	JLT	loop

halt	J	halt

.	Polynome calculation subroutine
.	S - where to get and store results
poly
.	Save used registers for later recovery
	STA	reg_a
	STS	reg_s
	STT	reg_t

.	Register setup
	LDS	@reg_s
	SUBR	A, A
	SUBR	T, T

.	+ x^4
	LDT	#1
	ADDR	T, A

.	+ 2x^3
	MULR	S, A
	LDT	#2
	ADDR	T, A

.	+ 3x^2
	MULR	S, A
	LDT	#3
	ADDR	T, A

.	+ 4x
	MULR	S, A
	LDT	#4
	ADDR	T, A

.	+ 5
	MULR	S, A
	LDT	#5
	ADDR	T, A

.	Store results
	STA	@reg_s

.	Recover registers
	LDA	reg_a
	LDS	reg_s
	LDT	reg_t

	RSUB

.	input
in	WORD	0
a	WORD	5
b	WORD	42
c	WORD	13
d	WORD	12
lastin	EQU	*
len	EQU	lastin - in

reg_a	RESW	1
reg_s	RESW	1
reg_t	RESW	1