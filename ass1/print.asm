prog 	START 	0

loop	
.	while ((A) != NULL)
	LDCH	in, X
	COMP	null
	JEQ	halt

.	dev.write(in[x])
	WD	#0xAA

.	x++
	TIX	null
	J	loop

halt	J	halt

.	Null terminated string
in	WORD	C'SIC/XE'
	BYTE	10
null	BYTE	X'00'