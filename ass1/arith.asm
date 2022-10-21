prog	START	0

.	sum
	LDA	x
	ADD	y
	STA	sum

.	diff
	LDA	x
	SUB	y
	STA	diff

.	prod
	LDA	x
	MUL	y
	STA	prod

.	quot
	LDA	x
	DIV	y
	STA	quot

.	mod
	LDA	quot
	MUL	y
	SUB	x
	MUL	NEG
	STA	mod

halt	J	halt

.	constants
NEG	WORD	-1

.	input
x	WORD	6
y	WORD	3

.	output
sum	RESW	1
diff	RESW	1	
prod	RESW	1
quot	RESW	1
mod	RESW	1