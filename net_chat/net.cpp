#include "net.h"

/* Returns 0 when ok 
 * 
 * Remember to freeaddrinfo(addr_result);
 */

int get_host_adress(const char *host, const char *port, addrinfo *&addr_result) 
{
	int err;
	struct addrinfo addr_hints;
	
	memset(&addr_hints, 0, sizeof(addrinfo));
	
	addr_hints.ai_family = AF_INET;
	addr_hints.ai_socktype = SOCK_STREAM;
	addr_hints.ai_protocol = IPPROTO_TCP;
	
	err = getaddrinfo(host, port, &addr_hints, &addr_result);
	
	return err;
}

/* Returns socket */
int connect_to_server(addrinfo *addr_result)
{
	int sock = socket(addr_result->ai_family, addr_result->ai_socktype, addr_result->ai_protocol);
	
	if (sock < 0)
		return -1;
	
	// connect socket to the server
	if (connect(sock, addr_result->ai_addr, addr_result->ai_addrlen) < 0) {
		close(sock);
		
		return -1;
	}
	
	//fcntl(sock, F_SETFL, O_NONBLOCK);
	return sock;
}

/* Zwracane wartości:
 * -2 - otrzymano złośliwy nagłówek
 * -1 - błąd połączenia
 *  0 - wszystko odczytano
 *  1 - brakuje danych w strumieniu, ponów potem
 *  2 - zamknięto połączenie
 * 
 *  NIE konwertuje porządku sieciowego na hosta
 *  NIE zamyka problematycznych połączeń
 */
int receive_message(int socket, TransmissionProgress& progress)
{
	unsigned short int expected = 0;
	char *buff = (char *) &progress.msg;
	buff += progress.transfered_bytes;
	
	if (progress.transfered_bytes < sizeof(uint16_t)) {
		expected = sizeof(uint16_t) - progress.transfered_bytes;
	} else {
		expected = ntohs(progress.msg.msg_size);
		expected -= progress.transfered_bytes - sizeof(uint16_t);
	}
	
	if (expected == 0)
		return 0;
	
	int rval = read(socket, (void *) buff, expected);
	
	if (rval < 0) {
		return -1;
	} else if (rval == 0 && progress.transfered_bytes > 0) {
		return -2;
	} else if (rval == 0) {
		return 2;
	}
	
	progress.transfered_bytes += rval;
	
	// interpretacja danych i update struktury
	if (progress.transfered_bytes == sizeof(uint16_t) && 
	    ntohs(progress.msg.msg_size) > 1000) {
		return -2;
	} else if (progress.transfered_bytes > sizeof(uint16_t) && 
	           progress.transfered_bytes == sizeof(uint16_t) 
	           + ntohs(progress.msg.msg_size)) {
		return 0;
	}
	
	return 1;
}

/* -1 oznacza błąd */
int netsend_message(int sock, MessageStructure msg)
{	
	int size = sizeof(uint16_t) + msg.msg_size;
	msg.msg_size = htons(msg.msg_size);
	
	if (write(sock, (const void *) &msg, size) < 0) {
		perror("writing on stream socket");
		return -1;
	}
	
	return 0;
}
