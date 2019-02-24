@ Piotr Zalas 361374

.global start
.global run

.data

.balign 4
szer: .word 0

.balign 4
wys: .word 0

.balign 4
mat: .word 0

.text

start:
    ldr r3, =szer
    str r0, [r3]

    ldr r3, =wys
    str r1, [r3]

    ldr r3, =mat
    str r2, [r3]

    bx lr

run:
    @ r0 = szerokość
    @ r1 = wysokość
    @ r2 = tablica
    ldr r0, =szer
    ldr r0, [r0]

    ldr r1, =wys
    ldr r1, [r1]

    ldr r2, =mat
    LDR r2, [r2]

    @ r3 = szerokość * wysokość
    MUL r3, r0, r1

    @ Zachowanie zmiennych na stosie
    STMFD sp!, {r4-r9}

    @ Pętla po każdej komórce
    @ r4 = wiersz w tablicy
    @ r5 = kolumna w tablicy
    MOV r4, #0
    MOV r5, #0

loop:
    @ r6 = komórka w tablicy
    MUL r6, r4, r0
    ADD r6, r6, r5

    CMP r6, r3
    BGE copy

    @ Policzenie liczby sąsiadów
    @ r6 = adres aktualnej komórki w pamięci
    @ r7 = liczba sąsiadów
    @ r8 = rejestr pomocniczy do ładowania z pamięci
    ADD r6, r2, r6, LSL #2
    MOV r7, #0

    @ Sąsiad na lewo
    CMP r5, #0
    ADDNE r8, r6, #-4
    LDRNE r8, [r8]
    ADDNE r7, r7, r8

    @ Sąsiad na prawo
    @ r9 = rejestr pomocniczy
    ADD r9, r0, #-1
    CMP r5, r9
    ADDNE r8, r6, #4
    LDRNE r8, [r8]
    ADDNE r7, r7, r8

    @ Górni sąsiedzi
    @ r9 = rejestr pomocniczy
    CMP r4, #0  @ Jesteśmy w pierwszym wierszu
    BEQ down

    @ Górny środkowy
    SUB r8, r6, r0, LSL #2
    LDR r9, [r8]
    ADD r7, r7, r9

    @ Lewy górny
    CMP r5, #0
    SUBNE r8, r8, #4
    LDRNE r9, [r8]
    ADDNE r7, r7, r9
    ADDNE r8, r8, #4

    @ Prawy górny
    SUB r9, r0, #1
    CMP r5, r9
    ADDNE r8, r8, #4
    LDRNE r8, [r8]
    ADDNE r7, r7, r8

down:
    @ Dolni sąsiedzi
    @ r9 = rejestr pomocniczy
    SUB r9, r1, #1
    CMP r4, r9  @ Jesteśmy w ostatnim wierszu
    BEQ save

    @ Dolny środkowy
    ADD r8, r6, r0, LSL #2
    LDR r9, [r8]
    ADD r7, r7, r9

    @ Lewy dolny
    CMP r5, #0
    SUBNE r8, r8, #4
    LDRNE r9, [r8]
    ADDNE r7, r7, r9
    ADDNE r8, r8, #4

    @ Prawy dolny
    SUB r9, r0, #1
    CMP r5, r9
    ADDNE r8, r8, #4
    LDRNE r8, [r8]
    ADDNE r7, r7, r8

save:
    @ Zapisanie nowej wartości
    @ W r6 dalej jest wskaźnik na obecną komórkę
    @ r7 = ilość żywych sąsiadów
    @ r8 = wartość do zapisania
    @ r9 = stan obecnej komórki
    LDR r9, [r6]

    @ Domyślnie komórka jest martwa
    MOV r8, #0 

    @ Chyba, że ma 3 sąsiadów
    CMP r7, #3
    MOVEQ r8, #1 

    @ Albo jest żywa i ma 2 sąsiadów
    CMP r9, #1
    CMPEQ r7, #2
    MOVEQ r8, #1

    @ Zapis do tablicy pomocniczej
    MUL r9, r0, r1
    STR r8, [r6, r9, LSL #2]

    @ Przewinięcie kolumny i jeśli potrzeba wiersza
    ADD r5, #1
    CMP r5, r0
    MOVEQ r5, #0
    ADDEQ r4, #1

    B loop

copy:
    @ Skopiowanie pomocniczej tablicy
    @ r0 = zmienna pomocnicza na kopiowaną wartość
    @ r1 = licznik pętli
    @ r2 = adres bazowy tablicy
    @ r3 = adres kopii tablicy
    @ r4 = ilość komórek do skopiowania

    MUL r4, r0, r1
    MOV r1, #0
    ADD r3, r2, r4, LSL #2

copyloop:
    CMP r1, r4
    BGE end

    LDR r0, [r3, r1, LSL #2]
    STR r0, [r2, r1, LSL #2]

    ADD r1, #1
    B copyloop

end:
    @ Odtworzenie zmiennych
    LDMFD sp!, {r4-r9} 

    bx lr

