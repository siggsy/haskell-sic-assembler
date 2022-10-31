.=================================================.
. WARNING: This file requires linking
.=================================================.

.-------------------------------------------------.
. rec
.	assignment 1
...................................................
rec	START	0
...................................................
main	EXTREF	stack, sinit, push, pop, sptr	.Stack
	EXTREF	rchar, rnum			.Input
	EXTREF	nl, char, string, num		.Output
	EXTREF	fact
...................................................
.	init stack
	+LDA	#stack
	+JSUB	sinit

.	read input
loop
	+JSUB	rnum
	COMP	#0
	JEQ	halt

	+JSUB	fact
	+JSUB	num
	+JSUB	nl
	J	loop

halt	J	halt
._________________________________________________.

.-------------------------------------------------.
. fact
. 	calculating factorial recursive
...................................................
fact	CSECT
	EXTDEF	fact
	EXTREF	push, pop, sptr
...................................................
	+STL	@sptr
	+JSUB	push

.	f(1) = 1
	COMP	#1
	JEQ	halt

	+STA	@sptr
	+JSUB	push

.	f(n) = n * f(n-1)
	SUB	#1
	+JSUB	fact
	+JSUB	pop
	+MUL	@sptr

halt
	+JSUB	pop
	+LDL	@sptr
	RSUB
._________________________________________________.

.-------------------------------------------------.
. rchar
.	read number
...................................................
rchar	CSECT
	EXTDEF	rchar
...................................................
	CLEAR	A
	+RD	#0xFA
	RSUB
._________________________________________________.

.-------------------------------------------------.
. rnum
.	read number
...................................................
rnum	CSECT
	EXTDEF	rnum
	EXTREF	rchar
	EXTREF	push, pop, sptr
...................................................
	+STL	@sptr
	+JSUB	push
	+STS	@sptr
	+JSUB	push
	+STT	@sptr
	+JSUB	push

	LDS	#0
	LDT	#10

loop
	+JSUB	rchar

	COMP	#nl
	JEQ	halt

	SUB	#zero
	MULR	T, S
	ADDR	A, S
	J	loop

halt
	RMO	S, A
	+JSUB	pop
	+LDT	@sptr
	+JSUB	pop
	+LDS	@sptr
	+JSUB	pop
	+LDL	@sptr
	RSUB

nl	EQU	0x0A
zero	EQU	0x30
._________________________________________________.

.-------------------------------------------------.
. char
.	print character
...................................................
char	CSECT
	EXTDEF	char
...................................................
	+WD	stdout
	RSUB
stdout	BYTE	1
._________________________________________________.

.-------------------------------------------------.
. nl
.	print newline
...................................................
nl	CSECT
	EXTDEF	nl
	EXTREF	char
	EXTREF	push, pop, sptr
...................................................
	+STL	@sptr
	+JSUB	push
	+STA	@sptr
	+JSUB	push

	LDA	#10
	+JSUB	char

	+JSUB	pop
	+LDA	@sptr
	+JSUB	pop
	+LDL	@sptr
	RSUB
._________________________________________________.

.-------------------------------------------------.
. string
.	print string
.
. in	A: char*
...................................................
string	CSECT
	EXTDEF	string
	EXTREF	char
	EXTREF	push, pop, sptr
...................................................
	+STL	@sptr
	+JSUB	push
	+STX	@sptr
	+JSUB	push
	+STA	@sptr
	+JSUB	push

loop
	STA	addr
	CLEAR	A
	LDCH	@addr
	COMP	#0
	JEQ	halt
	+JSUB	char
	
	LDA	addr
	ADD	#1
	J	loop

halt
	+JSUB	pop
	+LDA	@sptr
	+JSUB	pop
	+LDX	@sptr
	+JSUB	pop
	+LDL	@sptr
	RSUB

addr	RESW	1
._________________________________________________.

.-------------------------------------------------.
. num
.	Print number. Since there are only 24 bits
.	to determine a number we can reserve 8
.	bytes to store digits and read them in
.	reverse 
.
. in	A: number
...................................................
num	CSECT
	EXTDEF	num
	EXTREF	char, dam
	EXTREF	push, pop, sptr
...................................................
	+STL	@sptr
	+JSUB	push
	+STX	@sptr
	+JSUB	push
	+STT	@sptr
	+JSUB	push
	+STS	@sptr
	+JSUB	push
	+STA	@sptr
	+JSUB	push

	STA	number
	LDX	#0

.	Get absolute value of the number
	COMP	#0
	JGT	calc_loop

.	Negate to get positive
	MUL	minus

calc_loop
	+JSUB	dam
	RMO	A, T
	RMO	S, A
	ADD	azero

	STCH	res, X

	RMO	T, A
	COMP	#0
	JEQ	print
	TIX	null
	J	calc_loop

print
	LDA	number	
	LDS	#1
	LDT	#res

.	Print sign if necessary
	COMP	#0
	JGT	print_loop
	JEQ	print_loop
	LDCH	aminus
	+JSUB	char
	LDA	number

print_loop
	LDCH	res, X
	+JSUB	char

	LDA	#res
	ADDR	X, A
	COMPR	A, T
	JEQ	halt
	
	SUBR	S, X
	J	print_loop

halt
	+JSUB	pop
	+LDA	@sptr
	+JSUB	pop
	+LDS	@sptr
	+JSUB	pop
	+LDT	@sptr
	+JSUB	pop
	+LDX	@sptr
	+JSUB	pop
	+LDL	@sptr
	RSUB
...................................................
number	RESW	1
neg	RESB	1
res	RESB	8
...................................................
azero	WORD	0x30
aminus	WORD	0x2D
minus	WORD	-1
null	BYTE	0x00
._________________________________________________.

.-------------------------------------------------.
. dam
.	division and modulo on register A
.
. in 	A: number
.
. out 	A: number (division)
.	S: number (modulo)
...................................................
dam	CSECT
	EXTDEF	dam
...................................................
	RMO	A, S

	DIV	#10
	MUL	#10
	SUBR	A, S
	DIV	#10

	RSUB
._________________________________________________.

.-------------------------------------------------.
. stack						  
._________________________________________________.

stacks	CSECT
	EXTDEF	stack, sinit, push, pop, sptr

.-stackinit---------------------------------------.
. initialize stack pointer
...................................................
sinit
	STA	sptr
	RSUB
._________________________________________________.

.-stackpush---------------------------------------.
. increase stack pointer
...................................................
push
	STA	_stack_tmp

	LDA	sptr
	ADD	#3
	STA	sptr

	LDA	_stack_tmp
	RSUB
._________________________________________________.

.-stackpop----------------------------------------.
. decrease stack pointer
...................................................
pop
	STA	_stack_tmp

	LDA	sptr
	SUB	#3
	STA	sptr

	LDA	_stack_tmp
	RSUB
._________________________________________________.

.-stack labels------------------------------------.
. For storing A register when calculating new
. stackptr value
_stack_tmp
	RESW	1

. Pointer to top of the stack
sptr
	RESW	1
._________________________________________________.

. Starting point of stack. This should be always
. at the end to allow as much space as available
stack
	RESW	1