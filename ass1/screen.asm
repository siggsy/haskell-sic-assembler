prog	START	0

.-main--------------------------------------------.
	USE
main
	LDCH	char
	JSUB	scrfill
	JSUB	scrclear

	JSUB	printrst

.	putchar('T');
	LDCH	char
	JSUB	printch

.	putchar('\n');
	JSUB	printnl

.	printf("%s", &string);
	LDA	#string
	JSUB	printstr
	
halt	J	halt

	USE	data
char	BYTE	C'T'
string	BYTE	C'To je testni string'
	BYTE	0x00
._________________________________________________.

.-scrclear----------------------------------------.
. clears screen
...................................................
	USE	
scrclear
	STT	_scr_t
	LDT	#0
	J	scrfor
._________________________________________________.

.-scrfill-----------------------------------------.
. fills screen
...................................................
	USE
scrfill
	STL	_scr_t
	RMO	A, T
	J	scrfor
._________________________________________________.

.-scrfor------------------------------------------.
. common code for scrfill & scrclear
...................................................
	USE	variables
_scr_a	RESW	1
_scr_s	RESW	1
_scr_t	RESW	1
_scr_l	RESW	1
...................................................
	USE
scrfor
	STA	_scr_a
	STS	_scr_s
	STL	_scr_l

	JSUB	printrst

	LDS	#3
	RMO	T, A
	
scrfor_loop
	JSUB	printch
	LDA	scrptr
	+COMP	#scrend
	JEQ	scrfor_end
	JGT	scrfor_end
	RMO	T, A
	J	scrfor_loop

scrfor_end
	JSUB	printrst
	LDA	_scr_a
	LDS	_scr_s
	LDT	_scr_t
	LDL	_scr_l
	RSUB
._________________________________________________.

.-printch-----------------------------------------.
. print lower byte from register A
...................................................
	USE	variables
_ch_a	RESW	1
...................................................
	USE
printch
	STA	_ch_a

	STCH	@scrptr
	LDA	scrptr
	ADD	#1
	STA	scrptr

	LDA	_ch_a
	RSUB
._________________________________________________.

.-printstr----------------------------------------.
. print string from address stored in A 
. (null terminated)
...................................................
	USE	variables
_str_a	RESW	1
_str_l	RESW	1

_str_address
	RESW	1
...................................................
	USE
printstr
	STA	_str_a
	STA	_str_address
	STL	_str_l

printstr_loop
	LDCH	@_str_address
	COMP	#0
	JEQ	printstr_end

	JSUB	printch
	LDA	_str_address
	ADD	#1
	STA	_str_address
	J	printstr_loop

printstr_end
	LDA	_str_a
	LDL	_str_l
	RSUB
._________________________________________________.

.-printnl-----------------------------------------.
. jump to new column to simulate \n
...................................................
	USE	variables
_nl_a	RESW	1
...................................................
	USE
printnl
	STA	_nl_a

.	col = 0
.	row += 1
	LDA	scrptr
	+SUB	#screen
	DIV	#scrrows
	MUL	#scrrows
	ADD	#scrcols
	+ADD	#screen
	STA	scrptr

	LDA	_nl_a
	RSUB
._________________________________________________.

.-printrst----------------------------------------.
. reset screen pointer to start
...................................................
	USE	variables
_rst_a	RESW	1
...................................................
	USE
printrst
	STA	_rst_a

	+LDA	#screen
	STA	scrptr

	LDA	_rst_a
	RSUB
._________________________________________________.

.-constants---------------------------------------.
	USE	data
scraddr	EQU	0x0B800
scrcols	EQU	80
scrrows	EQU	25
scrlen	EQU	scrcols * scrrows

	USE	screen_section
scrptr	RESW	1
	ORG	scraddr
screen	RESB	scrlen
scrend	EQU	*
._________________________________________________.