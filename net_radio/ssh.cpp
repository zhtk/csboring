#include "ssh.h"

#include <string>
#include <unistd.h>
#include <sys/wait.h>

using namespace std;

/*
 * Zakończenie procesu można poznać po zamknięciu łącza
 */
pid_t ssh_execute_command(string host, string command, int& outpipe, 
                          int& errpipe)
{
	int pipe1[2], pipe2[2];
	
	
	if (pipe(pipe1) < 0 || pipe(pipe2) < 0)
		return -1;
	
	outpipe = pipe1[0];
	errpipe = pipe2[0];
	
	pid_t pid = fork();
	
	switch (pid) {
	case -1:
		close(pipe1[0]);
		close(pipe1[1]);
		close(pipe2[0]);
		close(pipe2[1]);
		
		return -1;

	case 0:
		close(0);
		dup2(pipe1[1], 1);
		dup2(pipe2[1], 2);
		close(pipe1[0]);
		close(pipe2[0]);
		
		execlp("ssh", "ssh", host.c_str(), command.c_str(), NULL);
		
		exit(1); // Error with exec

	default:
		close(pipe1[1]);
		close(pipe2[1]);
	}
	
	return pid;
}

/* Sprawdza, czy podany proces się zakończył
 * 
 * Przyjmowane wartości:
 * -2 - jakiś bardzo dziwny błąd
 * -1 - proces się nie zakończył
 * 0 - wszystko OK
 * > 0 - miał miejsce jakiś błąd
 */
int ssh_check_status(pid_t process)
{
	int status;
	int res = waitpid(process, &status, WNOHANG);
	
	if (res == 0)
		return -1;
	
	if (WIFEXITED(status))
		return WEXITSTATUS(status);
	
	return -2;
}
