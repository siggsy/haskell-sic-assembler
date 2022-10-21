prog	START	0

.	Load to registers
	LDS	x
	LDT	y

.	sum
	ADDR	S, A
	ADDR	T, A
	STA	sum
	SUBR	A, A

.	diff
	ADDR	S, A
	SUBR	T, A
	STA	diff
	SUBR	A, A

.	prod
	ADDR	S, A
	MULR	T, A
	STA	prod
	SUBR	A, A

.	quot
	ADDR	S, A
	DIVR	T, A
	STA	quot

.	mod
	MULR	T, A
	SUBR	A, S
	STS	mod

halt	J	halt



x	WORD	4
y	WORD	6

sum	RESW	1
diff	RESW	1	
prod	RESW	1
quot	RESW	1
mod	RESW	1

NEG	WORD	-1