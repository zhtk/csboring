CFLAGS = -c -Wall -Inetpbm-10.47.70/lib/importinc/
LDFLAGS= -L netpbm-10.47.70/lib/ -lnetpbm -lm

zad1: netpbm-10.47.70/lib/libnetpbm.a main.o converter.o
	gcc -static main.o converter.o $(LDFLAGS) -o program

converter.o: converter.asm
	nasm -f elf64 -F dwarf -g converter.asm

main.o: main.c
	$(CC) $(CFLAGS) main.c

netpbm-10.47.70/lib/libnetpbm.a: netpbm-10.47.70.tgz
	make -C netpbm-10.47.70/lib/

netpbm-10.47.70.tgz:
	wget -O netpbm-10.47.70.tgz https://sourceforge.net/projects/netpbm/files/super_stable/10.47.70/netpbm-10.47.70.tgz/download
	tar xzf netpbm-10.47.70.tgz
