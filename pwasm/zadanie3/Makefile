# Piotr Zalas 361374

program: main.c symulacja.o
	gcc -g main.c symulacja.o -o program

symulacja.o: symulacja.s
	as -g symulacja.s -o symulacja.o

clean:
	rm -f program *.o
