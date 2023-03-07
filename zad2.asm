;requires input in form of single digit, single character(B-white, C-red, N-blue, Z-green) and short text seperated with one space
err1	MACRO	txt1, txt2, txt3, txt4, txt5, txt6	;writes error message that end with '$' and ends program
	mov		dx, offset txt1
	call	string
	mov 	dx, offset txt2
	call	string
	mov		dx, offset txt3
	call	string
	mov 	dx, offset txt4
	call	string
	mov 	dx, offset txt5
	call	string
	mov 	dx, offset txt6
	call	string
	jmp 	fin

ENDM

code1 segment

start1:
		;ds -> wskazuje na segment programu
		;offset: 080h = liczba znakow buforu
		;		 081h = spacja
		;		 082h = poczatek bufforu

		;inicjalizacja stosu
		mov		sp, offset wstosu
		mov 	ax, seg wstosu
		mov 	ss, ax

		call	args       	;reading arguments

		mov 	ax, seg text_len
		mov 	es, ax
		mov 	di, offset text_len
		cmp 	byte ptr es:[di], 70 ;error
		je 		error_fin
		; call    test_input 	;testing input

		;change to VGA
		mov 	al, 13h 	;tryb graficzny 320x200 punktow 256 kolorow
		mov 	ah, 0
		int 	10h

		call 	draw_text

		;change back to text
		xor 	ax, ax
		int 	16h 	;czekaj na dowolny klawisz
		mov 	al, 3h	;tryb tekstowy
		mov 	ah, 0
		int 	10h
		jmp 	fin

;procedures

draw_text:
		mov 	ax, seg font1
		mov 	ds, ax
		xor 	cx, cx
		mov 	cl, byte ptr cs:[text_len]
		mov 	ax, 4
		mul 	byte ptr cs:[scale]		;ax=4*scale - we try to put characters in the middle y wise
		sub 	word ptr cs:[y], ax
		mov 	si, offset text 		;set current index in key
		cmp 	byte ptr cs:[text_len], 0
		jne 	test_scale
		ret 				;text_len = 0, we display nothing
	test_scale:
		cmp 	byte ptr cs:[scale], 0
		jne 	character
		ret 				;scale = 0, we display nothing
	character: ;iterate through each character
		push 	cx
		mov 	bx, 1 		;bit mask for the column in the bitmap
		mov 	al, byte ptr cs:[si] 	;get current character
        mov     di, 08h
		mul 	di 						;ax=8*ascii_of_character - bitmap of the current character(each bitmap is 8 bytes/rows)
		mov 	di, ax
		add     di, offset font1
		mov 	cx, 8
	ix: ;iterate through x axis in bitmap
		push 	cx
		mov 	cx, word ptr cs:[y] 	;to reset y in each column
		push 	cx
		mov 	cx, 8
	iy: ;iterate through y axis in bitmap
		push 	cx
             
        xor     ax, ax     
		mov 	al, byte ptr ds:[di] 	;access the bitmap of current row of current character
		and     ax, bx      			;test whether we should light the pixel(bit is 1 in current row and column)
		jz      skip
		call 	light_scaled_pixel

	skip:
		xor 	ax, ax 					; set ax=0
		mov 	al, byte ptr cs:[scale]
		add 	word ptr cs:[y], ax 	; y+=scale - moving to the next scaled pixel(down)

		inc 	di 						;move to the next row of bitmap of current character
		pop 	cx
		loop 	iy

		sub 	di, 8					;reset to the first row of bitmap of current character
		shl 	bx, 1					;shift left to move the column of the bitman - represents moving right in bitmap
		pop 	cx
		mov 	word ptr cs:[y], cx 	; reset y to middle-4*scale(top of each character)
		xor 	ax, ax 					; set ax=0
		mov 	al, byte ptr cs:[scale]
		add 	word ptr cs:[x], ax 	; x+=scale - moving to the next scaled pixel(to the right)
		cmp 	word ptr cs:[x], 320 	; is x out of bound? (>=320)
		jge 	clear_stack				; clear stack (2 cxs pushed) 
		pop 	cx
		loop 	ix

		inc 	si 						;move to the next character of key
		pop 	cx
		loop 	character

	end_printing:
		ret

	clear_stack:
		pop 	cx
		pop 	cx
		jmp 	end_printing

light_scaled_pixel: ;light scaled pixel - lights a square ((x,y),(x+scale, y+scale)) and returns to original (x,y)
		push 	cx 						;to return to original values
		mov 	cx, word ptr cs:[x]
		push 	cx 						;to return to original values
		mov 	cx, word ptr cs:[y]
		push 	cx 						;to return to original values
		cmp 	word ptr cs:[x], 320 	; is x out of bound? (>=320)
		jge 	end_lighting

		mov 	cl, byte ptr cs:[scale]
	ipx: ;iterate through x axis in pixel
		push 	cx
		mov 	cx, word ptr cs:[y] 	;to reset y of a scaled pixel(each time we start in new column)
		push 	cx
		mov 	cl, byte ptr cs:[scale]
	ipy: ;iterate through y axis in pixel
		push 	cx
		call 	zapal_punkt

		inc 	word ptr cs:[y]
		pop	 	cx
		loop 	ipy	

		pop 	cx
		mov 	word ptr cs:[y], cx  	; reset y to the top of the pixel
		pop 	cx
		inc 	word ptr cs:[x]
		cmp 	word ptr cs:[x], 320 	; is x out of bound? (>=320)
		jge 	end_lighting
		loop 	ipx

	end_lighting:
		pop 	cx
		mov 	word ptr cs:[y], cx		;reset y to the original value
		pop 	cx
		mov 	word ptr cs:[x], cx 	;reset x to the original value
		pop 	cx
		ret

;...........................
x		dw	0		;left
y		dw	100 	;middle of a screen
kol		db	13
 
zapal_punkt:
        push    bx
		mov	    ax, 0a000h  ;adres segm pam. obrazu
		mov	    es, ax
		mov	    ax, word ptr cs:[y]
		mov 	bx, 320  ;ilosc punktow wlinii obrazu
		mul	    bx	; dx:ax = ax * bx
		add	    ax, word ptr cs:[x]   ;ax = 320*y +x
		mov	    bx, ax
		mov	    al, byte ptr cs:[kol]
		mov	    byte ptr es:[bx], al
		pop     bx
		ret
;...........................


test_input: ;printing what was read in args -> scale, length of text and text
		mov 	dx, offset scale
		call 	string
		mov 	dx, offset text_len
		call 	string 
		mov 	dx, offset text
		call 	string 
		ret    

args: ;reads and splits arguments into scale and text to draw
		xor		cx, cx
		mov		cl, byte ptr ds:[080h]
		cmp		cl, 0 ;agruments length 0?
		jnz		read_scale
		; err1	err_no_args, failed
		mov 	dx, offset err_no_args
		call	string
		jmp 	error_reading

	read_scale:
		mov 	ax, seg scale
		mov 	es, ax
		mov 	di, offset scale
		mov 	si, 082h 				;start of arguments
		mov 	al, byte ptr ds:[si] 	;read digit
		cmp 	al, '9'
		jg 		not_digit
		cmp 	al, '0'
		jl 		not_digit
		sub     al, '0'                 ;convert to number
		mov 	byte ptr es:[di], al 	;save digit
		inc 	si 		;there are still characters remaining
		dec 	cl 		;we move to the character after the scale(digit) and assume it's space	
		cmp		cl, 0
		je		bad_format
		cmp 	byte ptr ds:[si], ' ' 	;exactly one space seperating!
		jne		bad_format
		inc 	si 		;there are still characters remaining
		dec 	cl 		;we move past the space seperating digit and colour
		cmp 	cl, 0
		je 		bad_format

	read_colour:
		mov 	al, byte ptr ds:[si] 	;read colour
		mov 	byte ptr cs:[kol], 15 	;white
		cmp 	al, 'B'
		je 		after_colour
		mov 	byte ptr cs:[kol], 4 	;red
		cmp 	al, 'C'
		je 		after_colour
		mov 	byte ptr cs:[kol], 2 	;green
		cmp 	al, 'Z'
		je 		after_colour
		mov 	byte ptr cs:[kol], 1  	;blue 
		cmp 	al, 'N'
		je 		after_colour
		cmp 	al, ' '
		je 		bad_format
		mov 	dx, offset err_no_col
		call 	string
		jmp 	error_reading

	after_colour:
		inc 	si 						;move to the next character, after colour
		dec 	cl
		cmp 	cl, 0 					;no more characters
		je 		bad_format
		cmp 	byte ptr ds:[si], ' ' 	;exactly one space seperating
		jne 	bad_format
		inc 	si 						;move to the next character, after space
		dec 	cl
		cmp 	cl, 0 					;no more characters
		je 		bad_format
		cmp 	cl, 41
		jle 	read_txt
		; err1 	err_length, failed	
		mov 	dx, offset err_length
		call 	string
		jmp 	error_reading

	read_txt:
		mov 	ax, seg text_len
		mov 	es, ax
		mov 	di, offset text_len
		mov 	byte ptr es:[di], cl ;set text_len - number of bytes left
		mov 	ax, seg text
		mov 	es, ax
		mov 	di, offset text
		call 	read_text 			;we are calling procedure below - NOT to confuse with read_txt 
	finish_parsing:
		ret
	bad_format:
		mov 	dx, offset err_format
		call 	string
		jmp 	error_reading
	not_digit:
		; err1 	err_not_dig, failed
		mov 	dx, offset err_not_dig
		call 	string
		jmp 	error_reading

	error_reading:
		mov 	ax, seg text_len
		mov 	es, ax
		mov 	di, offset text_len
		mov 	byte ptr es:[di], 70 ;set text_len >40 - error
		ret

read_text: ;reads chars ds:[si]->es:[di], total length in cl
	read_char:
		mov 	al, byte ptr ds:[si]	;read char
		mov 	byte ptr es:[di], al	;save char
		inc 	si 					;move to the next char (source)
		inc 	di 					;move to the next byte of destination
		loop 	read_char
		ret

string: ;print string with offset in dx, ended with '$'
		push	ds
		push	ax
		mov 	ax, code1
		mov 	ds, ax
		mov 	ah, 9
		int 	21h
		pop		ax
		pop 	ds
		ret

char: ;print char from dl
		push 	ax
		mov 	ah, 2h
		int 	21h
		pop 	ax
		ret

error_fin:
		err1 	format, explanation, colours_exp, text_exp, spaces, failed

fin: ;end program
		mov 	ah, 4ch ;zakoncz program i wroc do systemu
		int 	021h


;data - to keep dane1 segment only for bitmaps

scale		db 0, 13, 10, '$'
text		db 42 dup('$')
text_len 	db 0, 13, 10, '$'

;communicates

failed		 db "Failed", 13, 10, '$'
err_format	 db "Bad formatting", 13, 10, '$'
err_no_col   db "No such colour available", 13, 10, '$'
err_length	 db	"Too long argument", 13, 10, '$'
err_no_args	 db "Error no arguments", 13, 10, '$'
err_not_dig	 db "Error no digit as first argument or bad formatting", 13, 10, '$'
format 		 db "Correct format:name Z C text", 13, 10, '$'
explanation  db "Where: name-program name, Z-single digit zoom", 13, 10, '$'
colours_exp  db "C-single character colour from {B-white, C-Red, Z-green, N-blue}", 13, 10, '$'
text_exp 	 db "text-any text with length greater than 0 and less than 41", 13, 10, '$'
spaces 		 db "Each argument seperated with exactly one space", 13, 10, '$'

code1 ends


stos1 segment stack
		dw 200 dup(?)
wstosu 	dw ?
stos1 ends



dane1 segment ;bitamps of each character
font1   db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0000 (nul)
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0001
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0002
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0003
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0004
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0005
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0006
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0007
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0008
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0009
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+000A
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+000B
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+000C
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+000D
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+000E
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+000F
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0010
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0011
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0012
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0013
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0014
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0015
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0016
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0017
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0018
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0019
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+001A
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+001B
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+001C
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+001D
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+001E
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+001F
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0020 (space)
        db 018h, 03Ch, 03Ch, 018h, 018h, 000h, 018h, 000h    ; U+0021 (!)
        db 036h, 036h, 000h, 000h, 000h, 000h, 000h, 000h    ; U+0022 (")
        db 036h, 036h, 07Fh, 036h, 07Fh, 036h, 036h, 000h    ; U+0023 (#)
        db 00Ch, 03Eh, 003h, 01Eh, 030h, 01Fh, 00Ch, 000h    ; U+0024 ($)
        db 000h, 063h, 033h, 018h, 00Ch, 066h, 063h, 000h    ; U+0025 (%)
        db 01Ch, 036h, 01Ch, 06Eh, 03Bh, 033h, 06Eh, 000h    ; U+0026 (&)
        db 006h, 006h, 003h, 000h, 000h, 000h, 000h, 000h    ; U+0027 (')
        db 018h, 00Ch, 006h, 006h, 006h, 00Ch, 018h, 000h    ; U+0028 (()
        db 006h, 00Ch, 018h, 018h, 018h, 00Ch, 006h, 000h    ; U+0029 ())
        db 000h, 066h, 03Ch, 0FFh, 03Ch, 066h, 000h, 000h    ; U+002A (*)
        db 000h, 00Ch, 00Ch, 03Fh, 00Ch, 00Ch, 000h, 000h    ; U+002B (+)
        db 000h, 000h, 000h, 000h, 000h, 00Ch, 00Ch, 006h    ; U+002C (h,)
        db 000h, 000h, 000h, 03Fh, 000h, 000h, 000h, 000h    ; U+002D (-)
        db 000h, 000h, 000h, 000h, 000h, 00Ch, 00Ch, 000h    ; U+002E (.)
        db 060h, 030h, 018h, 00Ch, 006h, 003h, 001h, 000h    ; U+002F (/)
        db 03Eh, 063h, 073h, 07Bh, 06Fh, 067h, 03Eh, 000h    ; U+0030 (0)
        db 00Ch, 00Eh, 00Ch, 00Ch, 00Ch, 00Ch, 03Fh, 000h    ; U+0031 (1)
        db 01Eh, 033h, 030h, 01Ch, 006h, 033h, 03Fh, 000h    ; U+0032 (2)
        db 01Eh, 033h, 030h, 01Ch, 030h, 033h, 01Eh, 000h    ; U+0033 (3)
        db 038h, 03Ch, 036h, 033h, 07Fh, 030h, 078h, 000h    ; U+0034 (4)
        db 03Fh, 003h, 01Fh, 030h, 030h, 033h, 01Eh, 000h    ; U+0035 (5)
        db 01Ch, 006h, 003h, 01Fh, 033h, 033h, 01Eh, 000h    ; U+0036 (6)
        db 03Fh, 033h, 030h, 018h, 00Ch, 00Ch, 00Ch, 000h    ; U+0037 (7)
        db 01Eh, 033h, 033h, 01Eh, 033h, 033h, 01Eh, 000h    ; U+0038 (8)
        db 01Eh, 033h, 033h, 03Eh, 030h, 018h, 00Eh, 000h    ; U+0039 (9)
        db 000h, 00Ch, 00Ch, 000h, 000h, 00Ch, 00Ch, 000h    ; U+003A (:)
        db 000h, 00Ch, 00Ch, 000h, 000h, 00Ch, 00Ch, 006h    ; U+003B (;)
        db 018h, 00Ch, 006h, 003h, 006h, 00Ch, 018h, 000h    ; U+003C (<)
        db 000h, 000h, 03Fh, 000h, 000h, 03Fh, 000h, 000h    ; U+003D (=)
        db 006h, 00Ch, 018h, 030h, 018h, 00Ch, 006h, 000h    ; U+003E (>)
        db 01Eh, 033h, 030h, 018h, 00Ch, 000h, 00Ch, 000h    ; U+003F (?)
        db 03Eh, 063h, 07Bh, 07Bh, 07Bh, 003h, 01Eh, 000h    ; U+0040 (@)
        db 00Ch, 01Eh, 033h, 033h, 03Fh, 033h, 033h, 000h    ; U+0041 (A)
        db 03Fh, 066h, 066h, 03Eh, 066h, 066h, 03Fh, 000h    ; U+0042 (B)
        db 03Ch, 066h, 003h, 003h, 003h, 066h, 03Ch, 000h    ; U+0043 (C)
        db 01Fh, 036h, 066h, 066h, 066h, 036h, 01Fh, 000h    ; U+0044 (D)
        db 07Fh, 046h, 016h, 01Eh, 016h, 046h, 07Fh, 000h    ; U+0045 (E)
        db 07Fh, 046h, 016h, 01Eh, 016h, 006h, 00Fh, 000h    ; U+0046 (F)
        db 03Ch, 066h, 003h, 003h, 073h, 066h, 07Ch, 000h    ; U+0047 (G)
        db 033h, 033h, 033h, 03Fh, 033h, 033h, 033h, 000h    ; U+0048 (H)
        db 01Eh, 00Ch, 00Ch, 00Ch, 00Ch, 00Ch, 01Eh, 000h    ; U+0049 (I)
        db 078h, 030h, 030h, 030h, 033h, 033h, 01Eh, 000h    ; U+004A (J)
        db 067h, 066h, 036h, 01Eh, 036h, 066h, 067h, 000h    ; U+004B (K)
        db 00Fh, 006h, 006h, 006h, 046h, 066h, 07Fh, 000h    ; U+004C (L)
        db 063h, 077h, 07Fh, 07Fh, 06Bh, 063h, 063h, 000h    ; U+004D (M)
        db 063h, 067h, 06Fh, 07Bh, 073h, 063h, 063h, 000h    ; U+004E (N)
        db 01Ch, 036h, 063h, 063h, 063h, 036h, 01Ch, 000h    ; U+004F (O)
        db 03Fh, 066h, 066h, 03Eh, 006h, 006h, 00Fh, 000h    ; U+0050 (P)
        db 01Eh, 033h, 033h, 033h, 03Bh, 01Eh, 038h, 000h    ; U+0051 (Q)
        db 03Fh, 066h, 066h, 03Eh, 036h, 066h, 067h, 000h    ; U+0052 (R)
        db 01Eh, 033h, 007h, 00Eh, 038h, 033h, 01Eh, 000h    ; U+0053 (S)
        db 03Fh, 02Dh, 00Ch, 00Ch, 00Ch, 00Ch, 01Eh, 000h    ; U+0054 (T)
        db 033h, 033h, 033h, 033h, 033h, 033h, 03Fh, 000h    ; U+0055 (U)
        db 033h, 033h, 033h, 033h, 033h, 01Eh, 00Ch, 000h    ; U+0056 (V)
        db 063h, 063h, 063h, 06Bh, 07Fh, 077h, 063h, 000h    ; U+0057 (W)
        db 063h, 063h, 036h, 01Ch, 01Ch, 036h, 063h, 000h    ; U+0058 (X)
        db 033h, 033h, 033h, 01Eh, 00Ch, 00Ch, 01Eh, 000h    ; U+0059 (Y)
        db 07Fh, 063h, 031h, 018h, 04Ch, 066h, 07Fh, 000h    ; U+005A (Z)
        db 01Eh, 006h, 006h, 006h, 006h, 006h, 01Eh, 000h    ; U+005B ([)
        db 003h, 006h, 00Ch, 018h, 030h, 060h, 040h, 000h    ; U+005C (\)
        db 01Eh, 018h, 018h, 018h, 018h, 018h, 01Eh, 000h    ; U+005D (])
        db 008h, 01Ch, 036h, 063h, 000h, 000h, 000h, 000h    ; U+005E (^)
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 0FFh    ; U+005F (_)
        db 00Ch, 00Ch, 018h, 000h, 000h, 000h, 000h, 000h    ; U+0060 (`)
        db 000h, 000h, 01Eh, 030h, 03Eh, 033h, 06Eh, 000h    ; U+0061 (a)
        db 007h, 006h, 006h, 03Eh, 066h, 066h, 03Bh, 000h    ; U+0062 (b)
        db 000h, 000h, 01Eh, 033h, 003h, 033h, 01Eh, 000h    ; U+0063 (c)
        db 038h, 030h, 030h, 03eh, 033h, 033h, 06Eh, 000h    ; U+0064 (d)
        db 000h, 000h, 01Eh, 033h, 03fh, 003h, 01Eh, 000h    ; U+0065 (e)
        db 01Ch, 036h, 006h, 00fh, 006h, 006h, 00Fh, 000h    ; U+0066 (f)
        db 000h, 000h, 06Eh, 033h, 033h, 03Eh, 030h, 01Fh    ; U+0067 (g)
        db 007h, 006h, 036h, 06Eh, 066h, 066h, 067h, 000h    ; U+0068 (h)
        db 00Ch, 000h, 00Eh, 00Ch, 00Ch, 00Ch, 01Eh, 000h    ; U+0069 (i)
        db 030h, 000h, 030h, 030h, 030h, 033h, 033h, 01Eh    ; U+006A (j)
        db 007h, 006h, 066h, 036h, 01Eh, 036h, 067h, 000h    ; U+006B (k)
        db 00Eh, 00Ch, 00Ch, 00Ch, 00Ch, 00Ch, 01Eh, 000h    ; U+006C (l)
        db 000h, 000h, 033h, 07Fh, 07Fh, 06Bh, 063h, 000h    ; U+006D (m)
        db 000h, 000h, 01Fh, 033h, 033h, 033h, 033h, 000h    ; U+006E (n)
        db 000h, 000h, 01Eh, 033h, 033h, 033h, 01Eh, 000h    ; U+006F (o)
        db 000h, 000h, 03Bh, 066h, 066h, 03Eh, 006h, 00Fh    ; U+0070 (p)
        db 000h, 000h, 06Eh, 033h, 033h, 03Eh, 030h, 078h    ; U+0071 (q)
        db 000h, 000h, 03Bh, 06Eh, 066h, 006h, 00Fh, 000h    ; U+0072 (r)
        db 000h, 000h, 03Eh, 003h, 01Eh, 030h, 01Fh, 000h    ; U+0073 (s)
        db 008h, 00Ch, 03Eh, 00Ch, 00Ch, 02Ch, 018h, 000h    ; U+0074 (t)
        db 000h, 000h, 033h, 033h, 033h, 033h, 06Eh, 000h    ; U+0075 (u)
        db 000h, 000h, 033h, 033h, 033h, 01Eh, 00Ch, 000h    ; U+0076 (v)
        db 000h, 000h, 063h, 06Bh, 07Fh, 07Fh, 036h, 000h    ; U+0077 (w)
        db 000h, 000h, 063h, 036h, 01Ch, 036h, 063h, 000h    ; U+0078 (x)
        db 000h, 000h, 033h, 033h, 033h, 03Eh, 030h, 01Fh    ; U+0079 (y)
        db 000h, 000h, 03Fh, 019h, 00Ch, 026h, 03Fh, 000h    ; U+007A (z)
        db 038h, 00Ch, 00Ch, 007h, 00Ch, 00Ch, 038h, 000h    ; U+007B (db)
        db 018h, 018h, 018h, 000h, 018h, 018h, 018h, 000h    ; U+007C (|)
        db 007h, 00Ch, 00Ch, 038h, 00Ch, 00Ch, 007h, 000h    ; U+007D ()
        db 06Eh, 03Bh, 000h, 000h, 000h, 000h, 000h, 000h    ; U+007E (~)
        db 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000    ; U+007F
dane1 ends
 


end start1 