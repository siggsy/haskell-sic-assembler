prog	START	0

.	Load to registers
	LDS	x

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


.	store results
	STA	res

halt	J	halt

.	input
x	WORD	2

.	output
res	RESW	1