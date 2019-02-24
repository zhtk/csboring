; Piotr Zalas 361374

global start
global step

DEFAULT REL

section .bss

align   16

waga: resd 1
szer: resd 1
wys: resd 1
macierz: resq 1

float_size: equ 4

section .text

start:
	movss [waga], xmm0  ; Zapisanie wagi
	; GCC nie czyści górnej połówki rejestru
	mov [szer], edi     ; Zapamiętanie szerokości
	mov [wys], esi      ; Zapamiętanie wysokości
	mov [macierz], rdx  ; Zapamiętanie wskaźnika na matrycę
	ret

step:
	push rbp
	mov rbp, rsp

	xor rdx, rdx ; Wyczyszczenie rejestru na wysokość
	xor r8, r8   ; Wyczyszczenie rejestru na szerokość

	movss xmm6, [waga] ; Wczytanie wagi
	shufps xmm6, xmm6, 0x00 ; Rozrzucenie wagi na wszystkie elementy wektora

	; Pętla po wierszach
	mov rax, 1     ; Licznik wiersza
	mov edx, [wys] ; Liczba obrotów po wysokości

	mov r8d, [szer]     ; Zapisanie szerokości do R8
	mov rdi, [macierz]  ; Załadowanie wskaźnika bazowego do macierzy
	add rdi, float_size ; macierz[1]
	lea r9, [r8 + 2]    ; Prawdziwa szerokość jednego wiersza w matrycy
	lea rdi, [rdi + r9 * float_size] ; macierz[szer + 2 + 1]
	lea r10, [rdx + 2]  ; r10 = wys + 2 = prawdziwa wysokość matrycy
	imul r9, r10        ; r9 = (szer + 2)*(wys + 2)
	lea r9, [rdi + r9 * float_size] ; macierz[(szer + 2)*(wys + 2) + szer + 2 + 1]
                                        ; inaczej mówiąc, odpowiadająca rdi komórka w
                                        ; matrycy pomocniczej
step_wys_loop:
	; Sprawdzenie warunku pętli po wierszach
	; Wykorzystane rejestry: rax, rdx, rdi, r8, r9
	cmp rax, rdx
	jg step_wys_loop_end

	; Pętla po kolumnach
	mov rcx, 1          ; Licznik kolumny
	mov rsi, r8         ; Liczba obrotów po kolumnach
	and rsi, ~0x3       ; Liczba pełnych obrotów (obcięcie ostatnich kolumn)

	; Wsadzenie do xmm7 wartości w grzejniku
	movss xmm7, [r9 - float_size]
step_szer_full_loop:
	; Pętla po 4 elementowych blokach w wierszu
	; Wykorzystane rejestry: rax, rdx, rcx, rsi, rdi, r8, r9
	cmp rcx, rsi
	jg step_szer_full_loop_end

	; Niezmienniki pętli:
	; w xmm6 jest waga rozmieszczona na wszystkich elementach
	; w xmm7 jest pierwszy element przed aktualnie przetwarzanym bloczkiem

	movups xmm0, [r9]  ; Aktualnie przetwarzany bloczek z matrycy pomocniczej
	movaps xmm3, xmm0  ; Kopia, którą będziemy przesuwać bitowo
	movaps xmm4, xmm0  ; Kopia, którą będziemy przesuwać bitowo
	movups xmm1, [r9 + r8 * float_size + float_size * 2]   ; Wiersz pod bloczkiem
	lea r10, [r8 * float_size + float_size * 2] ; Offset do obszaru nad bloczkiem
	neg r10
	movups xmm2, [r9 + r10] ; Wiersz nad bloczkiem

	; Dodanie wszystkiego do siebie
	subps xmm0, xmm3 ; 0
	subps xmm0, xmm3 ; - a_ij
	addps xmm0, xmm0 ; -2 * a_ij
	addps xmm0, xmm0 ; -4 * a_ij
	addps xmm0, xmm1 ; -4 * a_ij + a_(i+1)j
	addps xmm0, xmm2 ; -4 * a_ij + a_(i+1)j + a(i-1)j

	; Dodanie elementu poprzedniego
	shufps xmm3, xmm3, 0x93 ; a,b,c,d -> d,a,b,c
	movss xmm5, xmm3        ; Skopiowanie ostatniego elementu bloczku do bufora
	movss xmm3, xmm7        ; d,a,b,c -> x,a,b,c
	addps xmm0, xmm3        ; -4 * a_ij + a_(i+1)j + a(i-1)j + a_i(j-1)

	; Ustawienie xmm7 dla kolejnego obrotu pętli
	movss xmm7, xmm5

	; Dodanie elementu następnego
	movss xmm5, [r9 + 4 * float_size] ; Załadowanie pierwszego elementu x za blokiem
	movaps xmm3, xmm4       ; Skopiowanie początkowej zawartości bloczku
	shufps xmm3, xmm3, 0x6C ; a,b,c,d -> ?,d,c,b
	movss xmm3, xmm5        ; Wsadzenie xmm5 na koniec (po odwróceniu)
	shufps xmm3, xmm3, 0x1B ; Odwrócenie elementów w bloczku -
	                        ; przywrócenie normalnego porządku
	addps xmm0, xmm3        ; W xmm0 mamy teraz całą różnicę temperatury

	; Końcowe operacje
	mulps xmm0, xmm6 ; Pomnożenie różnicy przez wagę
	addps xmm0, xmm4 ; Dodanie do różnicy początkowej wartości

	movups [rdi], xmm0 ; Odesłanie wyniku do matrycy

	; Przesunięcie wskaźników
	add r9,  4 * float_size
	add rdi, 4 * float_size

	; Podbicie licznika
	add rcx, 4

	jmp step_szer_full_loop
step_szer_full_loop_end:
	; Końcówka pętli po wierszach (max 3 ostatnie elementy)
	mov rcx, r8
	and rcx, 0x3 ; Ilość elementów w ostatnim bloczku
	inc rcx      ; Dodatkowo wczytamy wartość grzejnika
step_cut_loop:
	; Załadowanie elementu ostatniego wiersza do xmm0

	; Elementy przetwarzanego wiersza
	shufps xmm0, xmm0, 0x93 ; Przesunięcie elementów w wektorze o jedno oczko
	movss xmm1, [r9 + rcx * float_size - float_size]
	movss xmm0, xmm1

	; Wiersza pod
	lea r10, [r9 + rcx * float_size - float_size] ; Właściwa kolumna
	shufps xmm2, xmm2, 0x93
	movss xmm1, [r10 + r8 * float_size + float_size * 2]
	movss xmm2, xmm1

	; Wiersza nad
	lea r10, [r8 * float_size + float_size * 2] ; Offset do obszaru nad bloczkiem
	neg r10
	lea r10, [r10 + rcx * float_size - float_size]
	shufps xmm3, xmm3, 0x93
	movss xmm1, [r9 + r10]
	movss xmm3, xmm1

	loop step_cut_loop
step_cut_end:
	; Wykonanie operacji na ostatnim bloczku
	movaps xmm1, xmm0
	
	; -4 * a_ij
	subps xmm0, xmm1 ; 0
	subps xmm0, xmm1 ; - a_ij
	addps xmm0, xmm0 ; -2 * a_ij
	addps xmm0, xmm0 ; -4 * a_ij

	; Dodanie wierszy pod i nad
	addps xmm0, xmm2
	addps xmm0, xmm3

	; Dodanie poprzedników
	movaps xmm2, xmm1        ; Skopiowanie oryginalnego bloczku
	shufps xmm2, xmm2, 0x93  ; Przesunięcie o oczko
	movss xmm2, xmm7         ; Wpisanie brakującej wartości
	addps xmm0, xmm2         ; Dodanie

	; Dodanie następników
	movaps xmm2, xmm1        ; Skopiowanie oryginału
	shufps xmm2, xmm2, 0x39  ; Przesunięcie oczko w lewo
	addps xmm0, xmm2         ; Dodanie

	; Operacje końcowe
	mulps xmm0, xmm6 ; Pomnożenie zmiany przez wagę
	addps xmm0, xmm1 ; Dodanie oryginalnych wartości

	; Wpisanie wyniku do tablicy
	mov rcx, r8  ; Długość wiersza
	and rcx, 0x3 ; Ilość elementów w ostatnim bloczku
step_save_loop:
	jrcxz step_save_end

	movss [rdi], xmm0 ; Zapisanie pojedyńczego wyniku
	shufps xmm0, xmm0, 0x39  ; Przygotowanie kolejnej wartości

	add rdi, float_size  ; Przesunięcie wskaźników
	add r9, float_size

	loop step_save_loop
step_save_end:
	; Epilog pętli po wierszach
	add r9, float_size * 2
	add rdi, float_size * 2
	inc rax

	jmp step_wys_loop
step_wys_loop_end:
	; Poprawienie pomocniczej macierzy
	add rdx, 2   ; wys + 2
	add r8, 2    ; szer + 2
	imul rdx, r8 ; (wys + 2) * (szer + 2)

	mov rsi, [macierz]
	lea rdi, [rsi + rdx * float_size]
	mov rcx, rdx
	REP movsd ; Skopiowanie wyniku do macierzy pomocniczej

	leave
	ret

