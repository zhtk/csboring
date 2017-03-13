#include "common.h"

/* Zwraca 0 jeśli wszystko OK */
int get_host_adress(const char *host, const char *port, addrinfo *&addr_result)
{
	struct addrinfo addr_hints;
	
	memset(&addr_hints, 0, sizeof(addrinfo));
	
	addr_hints.ai_family = AF_INET;
	addr_hints.ai_socktype = SOCK_STREAM;
	addr_hints.ai_protocol = IPPROTO_TCP;
	
	int err = getaddrinfo(host, port, &addr_hints, &addr_result);
	
	return err;
}

int get_udp_host_adress(const char *host, const char *port, addrinfo *&addr_result)
{
	struct addrinfo addr_hints;
	memset(&addr_hints, 0, sizeof(addrinfo));
	
	addr_hints.ai_family = AF_INET; // IPv4
	addr_hints.ai_socktype = SOCK_DGRAM;
	addr_hints.ai_protocol = IPPROTO_UDP;
	addr_hints.ai_flags = 0;
	addr_hints.ai_addrlen = 0;
	addr_hints.ai_addr = NULL;
	addr_hints.ai_canonname = NULL;
	addr_hints.ai_next = NULL;
	
	int err = getaddrinfo(host, port, &addr_hints, &addr_result);
	
	return err;
}

/* Podłącza do serwera i zwraca socket */
int connect_to_server(addrinfo *addr_result)
{
	int sock = socket(addr_result->ai_family, addr_result->ai_socktype,
	                  addr_result->ai_protocol);
	
	if (sock < 0)
		return -1;
	
	if (connect(sock, addr_result->ai_addr, addr_result->ai_addrlen) < 0) {
		close(sock);
		
		return -1;
	}
	
	return sock;
}
