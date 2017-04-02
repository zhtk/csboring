test: asmprog Javaprog.class

asmprog: asmprog.asm
	nasm -f elf64 asmprog.asm
	ld asmprog.o -o asmprog

Javaprog.class: javaprog.jasm
	java -jar jasmin.jar javaprog.jasm 

clean:
	rm *.o *.class asmprog
