global convert
global rscale
global gscale
global bscale

section .data
	rscale: db 77
	gscale: db 151
	bscale: db 28

section .text

; RDI - obraz, RSI - cols, RDX - rows
; format piksela = { uint32_t r, g, b }
convert:
	; Stos nie będzie potrzebny, więc nie tworzę nowej ramki.

	; Rejestr RDI w trakcie przetwarzania będzie zniszczony.
	; W [RDI] będzie wynik. Zachowam wskaźnik na niego w R8.
	mov r8, rdi
	; RDX będzie zniszczony w trakcie mnożenia, trzeba go
	; przenieść (np. do R9).
	mov r9, rdx

	; Spacer po wierszach matrycy
row_loop:
	cmp r9, 0
	jz row_loop_end

	; Pętla przetwarzająca kolumny wiersza
	mov rcx, rsi   ; RCX = licznik pętli
	mov rbx, [rdi] ; RBX = wskaźnik na przetwarzaną kolumnę
	mov r10, rbx   ; R10 = wskaźnik na kolumnę wynikową
cols_loop:
	; Złożenie nowego koloru
	xor rax, rax

	mov eax, dword [rbx] ; Załadowanie czerwonej składowej
	mul byte [rscale]    ; Pomnożenie składowej przez wagę
	mov r11, rax         ; Dodanie wyniku do średniej ważonej
	add rbx, 4           ; Przeskoczenie do kolejnej składowej koloru

	mov eax, dword [rbx] ; Załadowanie zielonej składowej
	mul byte [gscale]
	add r11, rax
	add rbx, 4

	mov eax, dword [rbx] ; Załadowanie niebieskiej składowej
	mul byte [bscale]
	add r11, rax
	add rbx, 4

	shr r11, 8           ; Podzielenie wyniku przez 256 = 2^8

	mov [r10], r11 ; Umieszczenie szarego koloru w kolumnie wynikowej
	add r10, 4     ; Podbicie wskaźnika na kolumnę z wynikiem

	loop cols_loop

	; Epilog pętli przetwarzającej wiersz
	add rdi, 8 ; Przesunięcie wskaźnika wiersza o jedno oczko do przodu
	dec r9     ; Zmniejszenie liczby wierszy do przetworzenia

	jmp row_loop
row_loop_end:
	; Umieść wskaźnik do wyniku w RAX.
	mov rax, r8

	ret
