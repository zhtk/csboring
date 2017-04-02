/**
 * Dane autora:
 * Piotr Zalas
 * Nr ind. 361374
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <ctype.h>

void last_step()
{
	int out_counter;
	scanf("%d", &out_counter);

	// Wypisanie "kolejki"
	char *sym;
	bool fst = true;

	while(out_counter--) {
		scanf("%ms", &sym);

		if(!fst)
			printf(" ");

		printf("%s", sym);
		fst = false;
		free(sym);
	}

	// Wypisanie zawartości stosu
	scanf("%d", &out_counter);
	while(out_counter--) {
		scanf("%ms", &sym);

		if(!fst)
			printf(" ");

		printf("%s", sym);
		fst = false;
		free(sym);
	}
}

int op_order(char *op)
{
	if(*op == '^')
		return 3;
	if(*op == '*' || *op == '/')
		return 2;
	if(*op == '+' || *op == '-')
		return 1;
	return 0;
}

void execute_order(FILE *child_in, int scount)
{
	// Pobranie symbolu do sparsowania
	// scount > 0
	char *symbol;
	scanf("%ms", &symbol);

	// Przerzucenie reszty do kolejnego procesu
	--scount;
	fprintf(child_in, "%d ", scount);

	while(scount--) {
		char *tmp;
		scanf("%ms", &tmp);
		fprintf(child_in, "%s ", tmp);
		free(tmp);
	}
	fflush(child_in); // Na potzreby dbg

	// Wczytanie kolejki z wynikiem częściowym
	int qcount;
	scanf("%d", &qcount);
	char **queue;
	queue = calloc(qcount, sizeof(char *));
	
	for(int i = 0; i < qcount; ++i) 
		scanf("%ms", queue + i);
	
	// Wczytanie stosu
	scanf("%d", &scount);
	char **stack;

	stack = calloc(scount, sizeof(char *));
	for(int i = 0; i < scount; ++i) 
		scanf("%ms", stack + i);

	// Przetworzenie
	int stackptr = 0;
	int isnumber = (isalnum(*symbol) ? 1 : 0);
	int isbrack = 0;

	if(op_order(symbol)) { // symbol jest operatorem
		while(stackptr < scount && op_order(symbol) <= op_order(stack[stackptr]))
			++stackptr;
	}
	
	if(*symbol == ')') { // Symbol jest prawym nawiasem
		isbrack = 1;

		// Jeśli wyjdziemy za tablicę to wyr. jest nieprawidłowe
		while(*stack[stackptr] != '(')
			++stackptr;

		free(stack[stackptr]);
	}

	// Przerzucenie reszty do nowego procesu
	// wynik częściowy 
	fprintf(child_in, "%d ", qcount + stackptr + isnumber);

	for(int i = 0; i < qcount; ++i) {
		fprintf(child_in, "%s ", queue[i]);
		free(queue[i]);
	}
	free(queue);

	for(int i = 0; i < stackptr; ++i) { /// ????
		fprintf(child_in, "%s ", stack[i]);
		free(stack[i]);
	}

	if(isnumber)
		fprintf(child_in, "%s ", symbol);

	// Stos
	stackptr += isbrack;

	fprintf(child_in, "%d ", scount - stackptr + 1 - isnumber - isbrack);

	if(!isnumber && !isbrack)
		fprintf(child_in, "%s ", symbol);

	for(int i = stackptr; i < scount; ++i) {
		fprintf(child_in, "%s ", stack[i]);
		free(stack[i]);
	}

	free(stack);
	fflush(child_in); 

	// Symbol nie jest już potrzebny
	free(symbol);
}

void copy_to_output(FILE *child_out)
{
	char tmp;
	while(fscanf(child_out, "%c", &tmp) != EOF)
		printf("%c", tmp);
}

int main (int argc, char *argv[])
{
	int sym_count;
	scanf("%d", &sym_count);

	// Koniec wykonywania
	if(sym_count == 0) {
		last_step();

		fflush(stdout);
		if(close(0) == -1 || close(1) == -1)
			return 1;

		return 0;
	}

	// Utworzenie kolejnego procesu W(i+1)
	int pipe_worker_in[2];
	int pipe_worker_out[2];
	
	if(pipe(pipe_worker_in) == -1)
		return 1;

	if(pipe(pipe_worker_out) == -1)
		return 1;

	switch(fork()) {
	case -1:
		printf("Error in fork\n");
		return 1;

	case 0: 
		if(close(pipe_worker_in[1]) == -1) {
			printf("Error in close pipe_worker_in[1]\n");
			return 1;
		}

		if(close(pipe_worker_out[0]) == -1) {
			printf("Error in close pipe_worker_out[0]\n");
			return 1;
		}
		
		if(dup2(pipe_worker_in[0], 0) == -1 || dup2(pipe_worker_out[1], 1) == -1) {
			printf("Error in dup2\n");
			return 1;
		}

		execl("./W", "W", (char *) 0);
		printf("Error in execl\n");
		return 1;
	}
	
	if(close(pipe_worker_in[0]) == -1 || close(pipe_worker_out[1]) == -1) {
		printf("Error in close (pipe_worker[i])\n");
		return 1;
	}

	// Otwarcie strumieni do dzieci
	FILE *child_in = fdopen(pipe_worker_in[1], "w");
	FILE *child_out = fdopen(pipe_worker_out[0], "r");

	if(child_in == NULL || child_out == NULL)  {
		printf("Error in child streams\n");
		return 1;
	}

	// Sparsowanie danych i przekazanie ich do 
	// kolejnego procesu
	execute_order(child_in, sym_count);

	if(wait(0) == -1) {
		printf("Error in wait\n");
		return 1;
	}

	copy_to_output(child_out);

	// Zamknięcie strumieni
	fclose(child_out);
	fclose(child_in);
	
	fflush(stdout);
	if(close(0) == -1 || close(1) == -1)
		return 1;

	return 0;
}
