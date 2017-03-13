#ifndef _SSH_
#define _SSH_

#include <string>
#include <unistd.h>

/* Uruchamia nowy proces SSH
 *
 * Zakończenie procesu można poznać po zamknięciu łącza
 */
pid_t ssh_execute_command(std::string host, std::string command,
                          int& outpipe, int& errpipe);

/* Sprawdza, czy podany proces się zakończył
 * 
 * Przyjmowane wartości:
 * -2 - jakiś bardzo dziwny błąd
 * -1 - proces się nie zakończył
 * 0 - wszystko OK
 * > 0 - miał miejsce jakiś błąd
 */
int ssh_check_status(pid_t process);

/*
// Przykład użycia biblioteczki

int err, out;
pid_t pr = ssh_execute_command("mimuw", "ls", out, err);

pollfd fd;
fd.fd = out;
fd.events = POLLIN;

while(true) {
	fd.revents = 0;
	poll(&fd, 1, 0);
	
	char c;
	int r = read(fd.fd, &c, 1);
	
	if (r == 0) {
		assert(ssh_check_status(pr) == 0);
		break;
	}
	
	write(1, &c, 1);
}
*/

#endif
