# Piotr Zalas 361374

program: main.c symulacja.o
	gcc -g main.c symulacja.o -o program

symulacja.o: symulacja.asm
	nasm -f elf64 -F dwarf -g symulacja.asm

clean:
	rm -f program *.o
