#include <stdio.h>
#include <poll.h>

#include "err.h"
#include "net.h"

const char *default_port = "20160";

void send_message(int sock)
{
	static MessageStructure msg;
	
	read(0, msg.msg + msg.msg_size, 1);
	++msg.msg_size;
	
	if (msg.msg[msg.msg_size - 1] != '\n' && msg.msg_size < 1000) 
		return;
	else if (msg.msg[msg.msg_size - 1] == '\n')
		--msg.msg_size;
	
	if (netsend_message(sock, msg) == -1)
		fatal("nie można wysłać wiadomości");
	
	msg.msg_size = 0;
}

int get_message(int socket, TransmissionProgress& progress)
{
	int ret = receive_message(socket, progress);
	
	if (ret == -2)
		extended_fatal(100, "zły nagłówek wiadomości");
	else if (ret == -1)
		fatal("błąd sieci przy odbieraniu wiadomości");
	else if (ret == 1)
		return 0; // Czekamy na resztę
	else if (ret == 2)
		return 1;
	
	// Wszystko ok, można wypisać wiadomość
	MessageStructure& msg = progress.msg;
	msg.msg_size = ntohs(msg.msg_size);
	printf("%.*s\n", msg.msg_size, msg.msg);
	fflush(stdout);
	
	// Wyczyszczenie postępu
	progress.transfered_bytes = 0;
	msg.msg_size = 0;
	
	return 0;
}

void serve(int socket)
{
	const int fdcount = 2;
	struct pollfd inputs[fdcount];
	
	for (int i = 0; i < fdcount; ++i) {
		inputs[i].fd = -1;
		inputs[i].events = POLLIN;
		inputs[i].revents = 0;
	}
	
	inputs[0].fd = STDIN_FILENO;
	inputs[1].fd = socket;
	
	// Strukturka na odbierane wiadomości
	TransmissionProgress progress;
	progress.transfered_bytes = 0;
	progress.msg.msg_size = 0;
	
	while (true) {
		for (int i = 0; i < fdcount; ++i)
			inputs[i].revents = 0;
      
		int ret = poll(inputs, fdcount, -1);
		
		if (ret > 0 && (inputs[0].revents & POLLIN) != 0) {
			send_message(socket);
		}
		
		if (ret > 0 && (inputs[1].revents & POLLIN) != 0) {
			if (get_message(socket, progress))
				break;
		}
		
		if (ret > 0 && (inputs[1].revents & POLLHUP) != 0) {
			if (progress.transfered_bytes == 0)
				break; // No niby nic złego się nie stało
			else
				extended_fatal(100, "host się rozłączył w połowie transmisji");
		}
	}
}

int main(int argc, char *argv[])
{
	// Ustawienie danych hosta
	const char *host, *port;
	
	if (argc != 2 && argc != 3)
		fatal("podano złą liczbę argumentów. Użycie: ./client host [port]");
	
	host = argv[1];
	port = default_port;
	
	if (argc == 3)
		port = argv[2];
	
	// Podłączenie do serwera
	struct addrinfo *addr_result;
	if (get_host_adress(host, port, addr_result) != 0)
		fatal("nie można odnaleźć hosta");
	
	int sock = connect_to_server(addr_result);
	freeaddrinfo(addr_result);
	
	if (sock == -1)
		fatal("nie można połączyć się z hostem");
	
	// Obsługa połączenia
	serve(sock);

	if (close(sock) < 0)
		fatal("błąd przy zamykaniu socketa");
	
	return 0;
}
