prog	START	0

main
	JSUB	scrfill
	JSUB	scrclear
halt	J	halt

.-scrclear----------------------------------------.
. clears screen
...................................................
scrclear
	LDT	#0
	J	scrfor
._________________________________________________.

.-scrfill-----------------------------------------.
. fills screen
...................................................
scrfill
	LDT	max
	J	scrfor
._________________________________________________.

.-scrfor------------------------------------------.
. common code for scrfill & scrclear
...................................................
scrfor
	STL	_scr_l
	LDS	#3

	JSUB	printrst
	RMO	T, A
	
scrfor_loop
	JSUB	printstr
	LDA	scrptr
	+COMP	#scrend
	JEQ	scrfor_end
	JGT	scrfor_end
	RMO	T, A
	J	scrfor_loop

scrfor_end
	LDL	_scr_l
	RSUB
._________________________________________________.

printch
	STCH	@scrptr
	LDA	scrptr
	ADD	#1
	STA	scrptr
	RSUB

printstr
	STA	@scrptr
	LDA	scrptr
	ADD	#3
	STA	scrptr
	RSUB

printrst
	+LDA	#screen
	STA	scrptr
	RSUB

.-constants---------------------------------------.
_scr_l	RESW	1
scraddr	EQU	0x0A000
scrcols	EQU	64
scrrows	EQU	64
scrlen	EQU	scrcols * scrrows

max	WORD	0xFFFFFF

scrptr	RESW	1
	ORG	scraddr
screen	RESB	scrlen
scrend	EQU	*
._________________________________________________.