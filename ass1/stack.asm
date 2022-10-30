stackt	START	0
.-main--------------------------------------------.
. program starting point
...................................................
main
	LDA	#stack
	JSUB	stackinit

	LDA	#10
	JSUB	fakulteta

	LDA	#10
	JSUB	fakulteta_it

halt	J	halt
._________________________________________________.

.-stackinit----------------------------------------.
. initialize stack pointer
...................................................
stackinit
	STA	stackptr
	RSUB
._________________________________________________.

.-stackpush---------------------------------------.
. increase stack pointer
...................................................
stackpush
	STA	_stack_tmp

	LDA	stackptr
	ADD	#3
	STA	stackptr

	LDA	_stack_tmp
	RSUB
._________________________________________________.

.-stackpop----------------------------------------.
. decrease stack pointer
...................................................
stackpop
	STA	_stack_tmp

	LDA	stackptr
	SUB	#3
	STA	stackptr

	LDA	_stack_tmp
	RSUB
._________________________________________________.

.-fakulteta---------------------------------------.
. calculating factorial recursive
...................................................
fakulteta
	STL	@stackptr
	JSUB	stackpush

.	f(1) = 1
	COMP	#1
	JEQ	fakulteta_end

	STA	@stackptr
	JSUB	stackpush

.	f(n) = n * f(n-1)
	SUB	#1
	JSUB	fakulteta
	JSUB	stackpop
	MUL	@stackptr

fakulteta_end
	JSUB	stackpop
	LDL	@stackptr
	RSUB
._________________________________________________.

.-fakulteta iterative-----------------------------.
. calculating factorial iterative
...................................................
fakulteta_it
	STL	@stackptr
	JSUB	stackpush
	STS	@stackptr
	JSUB	stackpush

	RMO	A, S

fakulteta_it_loop
	SUB	#1
	COMP	#1
	JEQ	fakulteta_it_end
	JLT	fakulteta_it_end
	MULR	A, S
	JGT	fakulteta_it_loop

fakulteta_it_end
	RMO	S, A

	JSUB	stackpop
	LDS	@stackptr
	JSUB	stackpop
	LDL	@stackptr
	RSUB
._________________________________________________.

.-hanoi-------------------------------------------.
. mah, zmanjkalo mi je energije za tole
._________________________________________________.

.-stack labels------------------------------------.

. For storing A register when calculating new
. stackptr value
_stack_tmp
	RESW	1

. Pointer to top of the stack
stackptr
	RESW	1

. Starting point of stack. This should be always
. at the end to allow as much space as available
stack
	RESW	1
._________________________________________________.

