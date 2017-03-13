#include <stdio.h>
#include <poll.h>

#include "err.h"
#include "net.h"

const int MAX_CLIENTS = 20;
const uint16_t DEFAULT_PORT = 20160;

pollfd client[MAX_CLIENTS + 1];
TransmissionProgress progress[MAX_CLIENTS + 1];
int activeClients = 0;

int bind_socket(uint16_t port = DEFAULT_PORT)
{
	int sock;
	
	/* Tworzymy gniazdko centrali */
	sock = socket(PF_INET, SOCK_STREAM, 0);

	if (sock < 0)
		fatal("nie można otworzyć gniazdka");

	/* Wybór adresu */
	struct sockaddr_in server;
	
	server.sin_family = AF_INET;
	server.sin_addr.s_addr = htonl(INADDR_ANY);
	server.sin_port = htons(port);
	
	if (bind(sock, (struct sockaddr*) &server, (socklen_t) sizeof(server)) < 0)
		fatal("nie można zbindować gniazdka");

	/* Zapraszamy klientów */
	if (listen(sock, MAX_CLIENTS) == -1)
		fatal("nie działa nasłuchiwanie");
	
	return sock;
}

void prepare_for_listening()
{
	for (int i = 0; i < MAX_CLIENTS + 1; ++i) {
		client[i].fd = -1;
		client[i].events = POLLIN;
		client[i].revents = 0;
	}

	activeClients = 0;
}

void accept_connection()
{
	int clientsock = accept(client[0].fd, (struct sockaddr*)0, (socklen_t*)0);

	if (clientsock == -1) {
		perror("problem z akceptowaniem połączenia");
		return;
	}

	for (int i = 1; i < MAX_CLIENTS + 1; ++i) 
		if (client[i].fd == -1) {
			client[i].fd = clientsock;
			break;
		}
    
	++activeClients;
}

void pass_message(int num)
{	
	for (int i = 1; i < MAX_CLIENTS + 1; ++i) {
		if (num == i || client[i].fd == -1)
			continue;
		
		netsend_message(client[i].fd, progress[num].msg);
	}
}

void check_client(int num)
{
	if (client[num].fd == -1)
		return;
	
	if (client[num].revents & (POLLIN | POLLERR)) {
		int ret = receive_message(client[num].fd, progress[num]);
		
		switch (ret) {
			case -1: // błąd połączenia
				perror("błąd połączenia, odłączono klienta");
			case 2: // zamknięto połączenie
			case -2: // otrzymano złośliwy nagłówek
				close(client[num].fd);
				client[num].fd = -1;
				progress[num].transfered_bytes = 0;
				--activeClients;
				break;
			case 0: // wszystko odczytano (wiadomość)
				progress[num].msg.msg_size = ntohs(progress[num].msg.msg_size);
				pass_message(num);
				progress[num].transfered_bytes = 0;
				break;
			case 1: // brakuje danych w strumieniu, ponów potem
				break;
		}
	}
	
	client[num].revents = 0;
}

void serve_clients(int sock)
{
	client[0].fd = sock;
	
	while (true) {
		int ret = poll(client, MAX_CLIENTS + 1, -1);
		
		if (ret < 0)
			fatal("problem z poll");
		
		if ((client[0].revents & POLLIN) && activeClients < MAX_CLIENTS)
			accept_connection();
		
		for (int i = 1; i < MAX_CLIENTS + 1; ++i)
			check_client(i);
	}
	
	if (client[0].fd >= 0)
		if (close(client[0].fd) < 0)
			fatal("błąd przy zamykaniu głównego socketa");
}

int main(int argc, char *argv[])
{
	if (argc > 2)
		fatal("złe argumenty. Użycie: ./server [port]");

	uint16_t port = DEFAULT_PORT;
	if (argc == 2 && sscanf(argv[1], "%hu", &port) < 1)
		fatal("nie można sparsować numeru portu");
	
	int sock = bind_socket(port);
	prepare_for_listening();
	serve_clients(sock);
	
	return 0;
}
