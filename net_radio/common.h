#ifndef _COMMON_
#define _COMMON_

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

extern int get_host_adress(const char *host, const char *port, addrinfo *&addr_result);
extern int get_udp_host_adress(const char *host, const char *port, addrinfo *&addr_result);

extern int connect_to_server(addrinfo* addr_result);

#endif
