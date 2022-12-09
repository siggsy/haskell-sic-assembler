rec	START	0

	. initialize sp and sbp
	JSUB	sinit

	LDA	#0
	LDS	#0	. holds sum of number being read
	LDT	#0	. for multiplying S with 10
	. read number from device
readCharLoop
	RD	device
	COMP	#0x0A
	JEQ	fibp		. read newline -> fib & print
	COMP	#0
	JEQ	halt		. EOF

	. multiply previous sum by T = 10
	LDT	#10
	MULR	T, S

	. add A to S
	SUB	#0x30		. subtract ascii(0) from A
	ADDR	A, S

	J	readCharLoop

halt	J	halt
	END	rec

fibp	STA	olda
	RMO	S, A
	COMP	#0
	JEQ	halt

	JSUB	fib	. A <- fib(A)
	JSUB	num	. print A
	LDCH	#10	
	WD	#1	. print newline

	LDS	#0
	LDA	olda
	J	readCharLoop

. A <- fib(A)
fib
	. push L to stack
	STL	@sp
	JSUB	push

	COMP	#2	. fib(0) = 0, fib(1) = 1
	JLT	fib_end

	. push A to stack
	STA	@sp
	JSUB	push

	SUB	#2
	JSUB	fib	. A <- fib(A-2)

	. push fib(A-2) to stack
	STA	@sp
	JSUB	push

	. peek A from stack
	JSUB	pop
	JSUB	pop
	LDA	@sp
	JSUB	push
	JSUB	push

	SUB	#1
	JSUB	fib	. A <- fib(A-1)

	. add to top of stack (fib(A-2))
	JSUB	pop	. pop fib(A-2) from stack
	ADD	@sp	. A = fib(A-1) + fib(A-2)

	. pop A from stack
	JSUB	pop

fib_end
	. pop L from stack
	JSUB	pop
	LDL	@sp
	RSUB

. print value in A in decimal form
num	. store old register values
	STL	@sp	. push return address to stack
	JSUB	push
	STS	@sp	. push S to stack
	JSUB	push
	COMP	=-1
	JGT	term

. print minus sign
minus
	. store old register values
	STA	olda
	LDCH	=C'-'
	WD	#1
	. restore old register values
	LDA	olda
	. to make it easier to work with, multiply with -1
	MUL	=-1

term	. push termination word to stack
	STA	olda
	LDA	#0xff
	STA	@sp
	JSUB	push
	LDA	olda
. save digits of number (in "R to L" order) to stack
plus	JSUB	mod10		. modres <- A % 10
	LDS	modres		. push S (modres) to stack
	STS	@sp
	JSUB	push
	DIV	#10
	COMP	#0
	JEQ	nprint
	J	plus

. print digits in stack (0xff for termination)
nprint
	JSUB	pop
	LDS	@sp	. set S to popped byte

	RMO	S, A
	COMP	#0xff

	JEQ	endnum	. reached termination -> end routine
	
	. print digit
	ADD	#48	. ascii(48) = '0'
	WD	#1

	J	nprint	. else move to next (previous) byte

endnum	. reset stack pointer
	. LDA	#_stack
	. STA	sp
	. pop old register values from stack (reverse order)
	JSUB	pop	. pop S
	LDS	@sp
	JSUB	pop	. pop L
	LDL	@sp
	RSUB

. store (A % 10) in modres
mod10	. store old register values
	STA	olda

	DIV	#10	. A /= 10
	MUL	#10	. A *= 10
	SUB	olda	. A <- A - olda

	. make sure modres a is positive number
	COMP	=-1
	JGT	save
	MUL	=-1

save	STA	modres
	. restore old register values
	LDA	olda
	RSUB

. result of modulo function
modres	RESW	1


. set stack pointer to start of stack
sinit	STA	stack_a
	LDA	#_stack
	STA	sp
	STA	sbp
	LDA	stack_a
	RSUB

. increment sp by one word
push	STA	stack_a
	LDA	sp
	ADD	#3
	STA	sp
	LDA	stack_a
	RSUB

. decrement sp by one word
pop	STA	stack_a
	LDA	sp
	SUB	#3
	STA	sp
	LDA	stack_a
	RSUB

. stack memory reservation
_stack	RESW	300

. stack pointer
sp	RESW	1

. stack base pointer
sbp	RESW	1

. storing the value of A
. for stack-relatev operations
stack_a	RESW	1

. storage for old register values
olda	RESW	1

. device name/code
device	BYTE	X'FA'
