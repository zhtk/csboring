#ifndef _NET_
#define _NET_

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

struct __attribute__((__packed__)) MessageStructure 
{
  uint16_t msg_size;
  char msg[1001];
};

struct TransmissionProgress 
{
	unsigned int transfered_bytes;
	struct MessageStructure msg;
};

extern int get_host_adress(const char *host, const char *port, addrinfo *&addr_result);

extern int connect_to_server(addrinfo* addr_result);

extern int netsend_message(int sock, MessageStructure msg);

extern int receive_message(int socket, TransmissionProgress& progress);
#endif
