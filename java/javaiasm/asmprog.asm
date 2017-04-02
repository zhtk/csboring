segment .text
        global  _start
 
_start:
	mov rax, 10_000_000_000 ;podkreslenie dla czytelnosci
loop:
	dec rax
	nop
	jnz loop
; wyjscie z programu
	mov     eax, 1
	xor     ebx, ebx
	int     0x80
; KONIEC PROGRAMU
