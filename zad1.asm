;requires input in form of inputfile outputfile "key", each seperated with one space
err1	MACRO	txt1, txt2	;writes error message that end with '$' and ends program
	mov		dx, offset txt1
	call	string
	mov 	dx, offset txt2
	call	string
	call 	fin

ENDM

dane1 segment

in_file		dw ?
out_file	dw ?
if_name		db 51 dup(0), 13, 10, '$'	;input file name limit is 50 so we ensure it ends with 0
of_name		db 51 dup(0), 13, 10, '$'	;output file name limit is 50 so we ensure it ends with 0
key			db 51 dup('$')
key_len 	db 0

buffor 		db 1024 dup(?)

; messages
success		 db "Success", 13, 10, '$'
failed 		 db "Failed", 13, 10, '$'
err_no_args	 db "No arguments", 13, 10, '$'
err_few_args db "Too few arguments", 13, 10, '$'
err_format	 db "Bad formatting", 13, 10, '$'
err_length	 db	"Too long argument", 13, 10, '$'
err_open_fil db "Error opening file", 13, 10, '$'
err_han_fil  db "Error handling file", 13, 10, '$'
err_rea_fil	 db	"Error reading file", 13, 10, '$'
err_wri_fil	 db "Error writting file", 13, 10, '$'
err_first	 db "First file", 13, 10, '$'
err_second 	 db	"Second file", 13, 10, '$'
err_closing  db	"Error closing file", 13, 10, '$'

dane1 ends


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
		call    test_input ;testing input
		call 	handle_files	;opening and creating files
		call 	cipher			;ciphering in_file using key into out_file
		call 	close_files		;closing opened files

		mov 	dx, offset success
		call 	string
		call 	fin

;procedures

cipher:	;ciphering in_file using key into out_file
		mov 	ax, seg buffor
		mov 	ds, ax
		mov 	bx, 0
		push 	bx 		; current index of character in key
	read: ;reading 1024 bytes from in_file
		mov 	dx, offset buffor 			; ds:[dx] -> buffor
		mov 	cx, 1024					; cx -> len of buffor
		mov 	bx, word ptr ds:[in_file]	; bx -> input file handler
		xor 	ax, ax						; ax=0
		mov 	ah, 03Fh					; read cx bytes from bx to ds:[dx]
		int 	21h

		jc 		reading_error		; error while reading file -> CY set
		cmp 	ax, 0				; end of file
		jne		xor_over_buf		; we read something
		pop 	bx 					;clear stack
		ret		

	xor_over_buf:
		mov 	cx, ax				; cx -> number of bytes read
		mov 	si, offset key 		; si -> byte in key
		pop 	bx 					;current index of character in key 
		mov 	ax, bx
		push 	cx					; on stack -> numberof bytes read to get number of bytes to write
		mov 	bx, offset buffor 	; bx -> byte in buffor
	loop1:
		cmp 	cx, 0				; no more bytes in buffor
		jz 		write
		cmp 	ah, byte ptr ds:[key_len]	; no more bytes in key
		jnz 	iter
		mov 	ah, 0						; reset our current byte of key to start
		mov 	si, offset key 				; reset si to start of the key 
	iter:
		mov 	dh, byte ptr ds:[bx] 	;current character in buffor
		mov 	al, byte ptr ds:[si]	;current character in key
		xor 	al, dh 					;xor character to get current output character - comment and should be key looped
		mov 	byte ptr ds:[bx], al 	;set current character in buffor to current output character
		inc 	si 					; move to next character in key
		inc 	bx 					; move to next characet in buffor
		inc 	ah 					; increase index of character in key
		loop 	loop1 				; go through each character in buffor(until cx=0)

	write: ;write buffor to out_file
		mov 	dx, offset buffor	; ds:[dx] -> buffor
		pop		cx					; cx -> number of bytes to write to file
		xor 	bx, bx
		mov 	bh, ah
		push 	bx 					; current index of character in key
		mov 	bx, word ptr ds:[out_file]	; bx -> output file handler
		mov 	ah, 40H				; write xs bytes from ds:[dx] to bx
		int 	21h

		jc 		writing_error		; error while writting file -> CY set

		jmp		read 				; read and process another batch of bytes

	reading_error:
		pop 	bx 					;clear stack
		err1 	err_rea_fil, failed

	writing_error:
		pop 	bx 					;clear stack
		err1 	err_wri_fil, failed

handle_files: ;handling files -> open if_name to in_file, create of_name to out_file (if file already exists, MS-DOS will open and truncate its length to 0)
    handle_first_file:
        mov     ax, seg if_name
        mov     ds, ax
        mov     dx, offset if_name ;ds:[dx] ->file name
        
        mov     al, 0h    			;readonly
        mov     ah, 3Dh   			;open file
        int     21h
        mov     word ptr ds:[in_file], ax 	;file handle if no error, no problem otherwise      
        jnc     handle_second_file			;no error while opening file -> CY clear
        mov 	dx, offset err_first
        call 	string
        err1    err_open_fil, failed
        
    handle_second_file: ;creating new file, even if the file already exists it is cleared
        mov     ax, seg of_name
        mov     ds, ax
        mov     dx, offset of_name 		;ds:[dx] ->file name
        
        mov     cx, 0h 					;normal attributes -> can be read from and written to
        mov 	ah, 3Ch 				;create file
        int 	21h
        mov 	word ptr ds:[out_file], ax 	;file handle if no error, no problem otherwise
        jc 		opening_error 				;error while opening file -> CY set
        ret 								;files handled with no errors, return

    opening_error:
    	mov 	dx, offset err_second
    	call 	string
    	err1 	err_open_fil, failed

close_files: ;closing both in_file and out_file
		mov 	bx, offset in_file
		call	close_file1
		mov 	bx, offset out_file
		call 	close_file1
		ret

close_file1: ;closing file with file handler in bx
		mov 	ah, 3Eh 	;closing file 
		int 	21h
		jc 		closing_error ;error while closing file -> CY set
		ret

	closing_error:
		err1	err_closing, failed

test_input: ;printing what was read in args -> first file, second file, key
		mov 	dx, offset if_name
		call 	string 
		mov 	dx, offset of_name
		call 	string 
		mov 	dx, offset key
		call 	string
		mov     dl, 13
		call    char
		mov     dl, 10
		call    char 
		ret    

args: ;reads and splits arguments into files and key
		xor		cx, cx
		mov		cl, byte ptr ds:[080h]
		cmp		cl, 0 ;agruments length 0?
		jnz		first_file
		err1	err_no_args, failed

	first_file:
		mov 	ax, seg if_name
		mov 	es, ax
		mov 	di, offset if_name
		mov 	si, 082h 			;start of arguments
		mov 	bl, ' '				;setting separator
		call 	seperate
		cmp		cl, 0
		jne		second_file
		err1	err_few_args, failed
	second_file:
		mov 	ax, seg of_name
		mov 	es, ax
		mov 	di, offset of_name
		mov 	bl, ' '				;setting separator
		call 	seperate
		cmp 	cl, 0
		jne 	test1
		err1 	err_few_args, failed
	test1:
	    mov 	al, byte ptr ds:[si] ;read char
		cmp 	al, '"' 
		je      read_key
		err1    err_format, failed
	read_key:
		inc 	si 				; there are still characters remaining
		dec		cl 				; we move past the first " of the key
		mov 	ax, seg key
		mov 	es, ax
		mov 	di, offset key
		mov		bl, '"'			;setting separator
		call 	seperate
	finish_parsing:
		ret


seperate: ;reads chars ds:[si]->es:[di], seperate by bl, total length in cl
		mov 	bh, 0 ;limit for names and key

	read_char:
		cmp 	cl, 0
		je 		formatting_error ;args ended before bl char appeared
		cmp		bh, 50
		je		finish_seperating
		mov 	al, byte ptr ds:[si] ;read char
		cmp 	al, bl
		je 		finish_seperating

		mov 	byte ptr es:[di], al
		inc 	si 					;move to the next char (source)
		dec 	cl 					;decrease number of bytes left
		inc 	di 					;move to the next byte of destination
		inc 	bh 					;length of current string
		jmp 	read_char

	formatting_error:
		err1 	err_format, failed

	length_error:
		err1 	err_length, failed

	finish_seperating:
		cmp		bh, 50
		je 		length_error ;too long argument
		cmp 	bh, 0
		je 		formatting_error ;no argument
		cmp		bl, '"' ;file name or key
		jne 	return1
		mov 	ax, seg key_len
		mov 	es, ax
		mov 	di, offset key_len
		mov 	byte ptr es:[di], bh ;set key_len
		return1:
		inc 	si ;move past the separator
		dec 	cl ;1 byte less left(after skipping separator)
		ret
		; call	fin

string: ;print string with offset in dx, ended with '$'
		push	ds
		push	ax
		mov 	ax, dane1
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

fin: ;end program
		mov 	ah, 4ch ;zakoncz program i wroc do systemu
		int 	021h

code1 ends


stos1 segment stack
		dw 200 dup(?)
wstosu 	dw ?
stos1 ends

end start1