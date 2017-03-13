TARGET: server client

CC	= gcc
CFLAGS	= -Wall -O2
CXXFLAGS	= -Wall -O2
LFLAGS	= -Wall

server: server.o net.o err.o
	$(CC) $(LFLAGS) $^ -o $@

client: client.o net.o err.o
	$(CC) $(LFLAGS) $^ -o $@

.PHONY: clean TARGET
clean:
	rm -f server client *.o *~ *.bak
