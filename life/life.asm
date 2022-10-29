prog	START	0

.-main--------------------------------------------.
main
.	Init phase
	JSUB	init

.	Life phase
	+LDB	#life_screen
	BASE	life_screen
	JSUB	life
halt	J	halt
._________________________________________________.

.-moveup------------------------------------------.
. move to one row up (with modulo)
. 
. in
.	A: address
...................................................
moveup
	SUB	#x
	+COMP	#life_screen
	JEQ	moveup_end
	JGT	moveup_end

moveup_overflow
	+ADD 	#size

moveup_end
	RSUB
._________________________________________________.

.-movedown----------------------------------------.
. move to one row down (with modulo)
. 
. in
.	A: address
...................................................
movedown
	ADD	#x
	+COMP	#life_screen_end
	JLT	movedown_end

movedown_overflow
	+SUB 	#size

movedown_end
	RSUB
._________________________________________________.

.-moveleft----------------------------------------.
. move to one byte left (with modulo)
. 
. in
.	A: address
...................................................
moveleft
	SUB	#1
	+COMP	#life_screen
	JEQ	moveleft_end
	JGT	moveleft_end

moveleft_overflow
	+ADD 	#size

moveleft_end
	RSUB
._________________________________________________.

.-moveright----------------------------------------.
. move to one byte right (with modulo)
. 
. in
.	A: address
...................................................
moveright
	ADD	#1
	+COMP	#life_screen_end
	JLT	moveright_end

moveright_overflow
	+SUB 	#size

moveright_end
	RSUB
._________________________________________________.

.-init--------------------------------------------.
. interactive initialization routine to
. set the initial state of the game
...................................................
init
	STL	_init_l

	+LDA	#life_screen
	STA	_init_addr

	LDCH	@_init_addr
	STCH	_init_orig

.	Read keyboard
	+LDB	#keyboard
	BASE 	keyboard

init_keyboard
	LDCH	_init_orig
	AND	#0x08
	OR	#0xF0
	STCH	@_init_addr

	LDCH	keyboard
	COMP	#key_w
	JEQ	init_keyboard_w
	COMP	#key_a
	JEQ	init_keyboard_a
	COMP	#key_s
	JEQ	init_keyboard_s
	COMP	#key_d
	JEQ	init_keyboard_d
	COMP	#key_space
	JEQ	init_keyboard_space
	COMP	#key_enter
	JEQ	init_end
	J	init_keyboard_end

init_keyboard_w
	LDA	#moveup
	STA	_init_move
	J	init_keyboard_move

init_keyboard_a
	LDA	#moveleft
	STA	_init_move
	J	init_keyboard_move

init_keyboard_s
	LDA	#movedown
	STA	_init_move
	J	init_keyboard_move

init_keyboard_d
	LDA	#moveright
	STA	_init_move
	J	init_keyboard_move

init_keyboard_move
	LDCH	_init_orig
	STCH	@_init_addr
	LDA	_init_addr
	JSUB	@_init_move
	STA	_init_addr
	LDCH	@_init_addr
	STCH	_init_orig
	J	init_keyboard_clear

init_keyboard_space
	CLEAR	A
	LDCH	_init_orig
	COMP	#0xFF
	JEQ	init_keyboard_space_off
	J	init_keyboard_space_on

init_keyboard_space_off
	CLEAR	A
	LDCH	#0x00
	STCH	_init_orig
	J	init_keyboard_clear

init_keyboard_space_on
	LDA	#0xFF
	STCH	_init_orig
	J	init_keyboard_clear

init_keyboard_clear
	LDA	#0x00
	STCH	keyboard
	J	init_keyboard_end

init_keyboard_end
	J	init_keyboard
	
init_end
	LDCH	_init_orig
	STCH	@_init_addr
	LDL	_init_l
	RSUB
._________________________________________________.

.-check-------------------------------------------.
. apply neighbour rules for specified cell
. 	
. alive cell
.	<2 	-> dies
.	2..3 	-> lives
.	>3	-> dies
.
. dead cell
.	3	-> alive
...................................................
. in
.	check_addr: location of the cell to check
...................................................
check
	STL	_check_l

	LDA	#0
	STA	_check_alive
	STA	_check_count

	LDA	check_addr
	JSUB	moveleft
	STA	_check_addr

check_top
	JSUB	moveup
	STA	_check_addr

check_top_left
	LDA	#check_top_middle
	STA	_check_next
	J	check_byte

check_top_middle
	LDA	_check_addr
	JSUB	moveright
	STA	_check_addr

	LDA	#check_top_right
	STA	_check_next
	J	check_byte

check_top_right
	LDA	_check_addr
	JSUB	moveright
	STA	_check_addr

	LDA	#check_middle
	STA	_check_next
	J	check_byte

check_middle
	LDA	_check_addr
	JSUB	moveleft
	JSUB	moveleft
	JSUB	movedown
	STA	_check_addr

check_middle_left
	LDA	#check_middle_middle
	STA	_check_next
	J	check_byte

check_middle_middle
	LDA	_check_addr
	JSUB	moveright
	STA	_check_addr

	LDCH	@_check_addr
	AND	set
	COMP	set
	JLT	check_middle_right
	JGT	check_middle_right

	DIV	set
	STA	_check_alive

check_middle_right
	LDA	_check_addr
	JSUB	moveright
	STA	_check_addr

	LDA	#check_bottom
	STA	_check_next
	J	check_byte

check_bottom
	LDA	_check_addr
	JSUB	moveleft
	JSUB	moveleft
	JSUB	movedown
	STA	_check_addr

check_bottom_left
	LDA	#check_bottom_middle
	STA	_check_next
	J	check_byte

check_bottom_middle
	LDA	_check_addr
	JSUB	moveright
	STA	_check_addr

	LDA	#check_bottom_right
	STA	_check_next
	J	check_byte

check_bottom_right
	LDA	_check_addr
	JSUB	moveright
	STA	_check_addr

	LDA	#check_new
	STA	_check_next
	J	check_byte

check_byte
	LDCH	@_check_addr
	AND	set
	COMP	set
	JLT	@_check_next
	JGT	@_check_next

	DIV	set
	ADD	_check_count
	STA	_check_count
	J	@_check_next

check_new
	LDA	_check_alive
	COMP	#1
	JEQ	check_new_alive

check_new_dead
	LDA	_check_count
	COMP	#3
	JEQ	check_set_alive
	J	check_set_dead

check_new_alive
	LDA	_check_count
	COMP	#2
	JEQ	check_new_alive_2
	JGT	check_new_alive_2
	JLT	check_set_dead

check_new_alive_2
	COMP	#3
	JLT	check_set_alive
	JEQ	check_set_alive
	JGT	check_set_dead

check_set_alive
	LDA	#0xFF
	STCH	check_return

	LDL	_check_l
	RSUB

check_set_dead
	LDA	#0x00
	STCH	check_return
	
	LDL	_check_l
	RSUB
._________________________________________________.

.-life--------------------------------------------.
. main loop that iterates through entire
. screen and updates state
...................................................
life
	LDA	#life_screen
	LDX	#0

life_loop
	LDA	#life_screen
	ADDR	X, A

	STA	check_addr
	JSUB	check

	LDCH	check_return
	STCH	life_new, X

	LDA	#life_screen
	+TIX	#16384
	JEQ	life_copy
	JGT	life_copy
	J 	life_loop

life_copy
	LDX	#0

life_copy_loop
	LDA	life_new, X
	STA	life_screen, X
	RMO	X, A
	ADD	#3
	+COMP	#size
	JEQ	life
	JGT	life
	RMO	A, X
	J	life_copy_loop
._________________________________________________.

	END	prog

.-variables---------------------------------------.
.init..............................................
_init_addr
	RESW	1
_init_move
	RESW	1
_init_orig
	RESB	1

_init_l
	RESW	1

.check.............................................
check_addr
	RESW	1
check_return
	RESB	1

_check_addr
	RESW	1
_check_count
	RESW	1
_check_alive
	RESW	1
_check_next
	RESW	1

_check_l
	RESW	1

.life..............................................
_life_pos
	WORD	0
._________________________________________________.

.-parameters--------------------------------------.
x	EQU	64
y	EQU	64
screen_address
	EQU	0x0A000

.	Keep keyboard address near screen to
.	reuse the same base when using base
.	addressing
keyboard_address
	EQU	0x09FFF
._________________________________________________.

.-constants---------------------------------------.
size	EQU	x * y
key_w
	EQU	0x57
key_a
	EQU	0x41
key_s
	EQU	0x53
key_d
	EQU	0x44
key_space
	EQU	0x20
key_enter
	EQU	0x0A

set	WORD	0x0000FF
._________________________________________________.

.-screen------------------------------------------.
life_new
	RESB	size

	ORG	screen_address
life_screen
	RESB	size
life_screen_end
	EQU	*
._________________________________________________.

.-keyboard----------------------------------------.
	ORG	keyboard_address
keyboard
	RESB	1
._________________________________________________.