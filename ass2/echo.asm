prog	START	0

.-------------------------------------------------.
. main
...................................................
main
	LDA #0xFF
	WD	#0xFA

.	Test number printing
	LDA	=-1241
	JSUB	num
	JSUB	nl

.	Test string printing
	LDA	#test_string
	JSUB	string
	JSUB	nl
	
halt	J	halt
.-------------------------------------------------.

.-------------------------------------------------.
. char
.	print character
...................................................
char	
	WD	stdout
	RSUB
.-------------------------------------------------.

.-------------------------------------------------.
. nl
.	print newline
...................................................
nl_a	RESW	1
nl_l	RESW	1
...................................................
nl	
	STA	nl_a
	STL	nl_l

	LDA	#10
	JSUB	char

	LDA	nl_a
	LDL	nl_l
	RSUB
.-------------------------------------------------.

.-------------------------------------------------.
. string
.	print string
.
. in	A: char*
...................................................
string_a
	RESW	1
string_x
	RESW	1
string_l
	RESW	1

string_addr
	RESW	1
...................................................
string
	STA	string_a
	STX	string_x
	STL	string_l

string_loop
	STA	string_addr
	CLEAR	A
	LDCH	@string_addr
	COMP	#0
	JEQ	string_halt
	JSUB	char
	
	LDA	string_addr
	ADD	#1
	J	string_loop

string_halt
	LDA	string_a
	LDX	string_x
	LDL	string_l
	RSUB
.-------------------------------------------------.

.-------------------------------------------------.
. num
.	Print number. Since there are only 24 bits
.	to determine a number we can reserve 8
.	bytes to store digits and read them in
.	reverse 
.
. in	A: number
...................................................
num_a	RESW	1
num_s	RESW	1
num_t	RESW	1
num_x 	RESW	1
num_l	RESW	1

num_neg	RESB	1
num_res	RESB	8
...................................................
num
	STA	num_a
	STS	num_s
	STT	num_t
	STX	num_x
	STL	num_l

	LDX	#0

.	Get absolute value of the number
	COMP	#0
	JGT	num_calc_loop

.	Negate to get positive
	MUL	=-1

num_calc_loop
	JSUB	dam
	RMO	A, T
	RMO	S, A
	ADD	asciizero

	STCH	num_res, X

	RMO	T, A
	COMP	#0
	JEQ	num_print
	TIX	null
	J	num_calc_loop

num_print
	LDA	num_a	
	LDS	#1
	LDT	#num_res

.	Print sign if necessary
	COMP	#0
	JGT	num_print_loop
	JEQ	num_print_loop
	LDCH	asciiminus
	JSUB	char
	LDA	num_a

num_print_loop
	LDCH	num_res, X
	JSUB	char

	LDA	#num_res
	ADDR	X, A
	COMPR	A, T
	JEQ	num_halt
	
	SUBR	S, X
	J	num_print_loop

num_halt
	LDA	num_a
	LDS	num_s
	LDT	num_t
	LDX	num_x
	LDL	num_l
	RSUB	
.-------------------------------------------------.

.-------------------------------------------------.
. dam
.	division and modulo on register A
.
. in 	A: number
.
. out 	A: number (division)
.	S: number (modulo)
...................................................
dam
	
	RMO	A, S

	DIV	#10
	MUL	#10
	SUBR	A, S
	DIV	#10

	RSUB
.-------------------------------------------------.

.	Constants
test_string
	BYTE	C'To je testni string, da preverim ce vse normalno deluje'
	BYTE	0x00

asciizero
	WORD	48
asciiminus
	WORD	C'-'
null	
	BYTE	0x00
stdout	
	BYTE	1