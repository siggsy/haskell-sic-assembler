prog	START	0

.	Load to registers
	LDS	x

.	+ 5
	LDA	#5

.	+ 4x
	LDT	#4
	MULR	S, T
	ADDR	T, A

.	+ 3x^2
	LDT	#3
	MULR	S, T
	MULR	S, T
	ADDR	T, A

.	+ 2x^3
	LDT	#2
	MULR	S, T
	MULR	S, T
	MULR	S, T
	ADDR	T, A

.	+ x^4
	LDT	#1
	MULR	S, T
	MULR	S, T
	MULR	S, T
	MULR	S, T
	ADDR	T, A

.	store results
	STA	res

halt	J	halt

.	input
x	WORD	2

.	output
res	RESW	1