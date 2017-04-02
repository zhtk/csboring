/**
 * Dane autora:
 * Piotr Zalas
 * Nr ind. 361374
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <stdbool.h>

int count_words(const char *str)
{
	int wynik = 0;
	bool ls = true;

	while(*str) {
		if(*str != ' ' && ls) {
			++wynik;
			ls = false;
		}

		if(*str == ' ')
			ls = true;

		++str;
	}

	return wynik;
}

int main(int argc, char *argv[])
{
	if (argc < 2) {
		printf("Za malo argumentow!\n");
		return 1;
	}

	if (strlen(argv[1]) == 0) {
		// Nic nie ma
		return 0;
	}

	// Tworzymy workera
	int pipe_worker_in[2];
	int pipe_worker_out[2];
	
	if (pipe(pipe_worker_in) == -1)
		return 1;

	if (pipe(pipe_worker_out) == -1)
		return 1;

	switch (fork()) {
	case -1:
		printf("Error in fork\n");
		return 1;

	case 0: 
		if (close(pipe_worker_in[1]) == -1) {
			printf("Error in close pipe_worker_in[1]\n");
			return 1;
		}

		if (close(pipe_worker_out[0]) == -1) {
			printf("Error in close pipe_worker_out[0]\n");
			return 1;
		}
		
		// Przekierowanie stdio do workera
		// stdout = 1
		// stdin = 0
		if (dup2(pipe_worker_in[0], 0) == -1 || dup2(pipe_worker_out[1], 1) == -1) {
			printf("Error in dup2\n");
			return 1;
		}

		execl("./W", "W", (char *) 0);
		printf("Error in execl\n");
		return 1;
		
	default:
		if (close(pipe_worker_in[0]) == -1 || close(pipe_worker_out[1]) == -1) {
			printf("Error in close (pipe_worker[i])\n");
			return 1;
		}

		// Otwarcie plików do strumieni
		FILE *child_in = fdopen(pipe_worker_in[1], "w");
		FILE *child_out = fdopen(pipe_worker_out[0], "r");

		if(child_in == NULL || child_out == NULL)  {
			printf("Error in child streams\n");
			return 1;
		}

		// Przekazanie workerowi początkowych danych
		// Długość napisu wejściowego i sam napis

		if (fprintf(child_in, "%d %s ", count_words(argv[1]),  argv[1]) == -1) {
			printf("Error in write: %s\n", argv[1]);
			return 1;
		}

		// Wielkość stosu i długość uzyskanego wyniku
		if (fprintf(child_in, "0 0") == -1) {
			printf("Error in write: stack & result\n");
			return 1;
		}
		// Koniec wpisywania, czas przepisać wynik
		fclose(child_in);

		// Niby nieefektywne ale chciałem zobaczyć jak to działa
		char tmp;
		while(fscanf(child_out, "%c", &tmp) != EOF)
			printf("%c", tmp);

		fclose(child_out);

		if (wait(0) == -1) {
			printf("Error in wait\n");
			return 1;
		}
	}

	return 0;
}
