#include "err.h"
#include "common.h"

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <poll.h>
#include <sys/stat.h> 
#include <fcntl.h>
#include <algorithm>
#include <string>
#include <boost/regex.hpp>

using namespace boost;
using namespace std;

// Ustawienia aplikacji

char *musicPath = NULL;
bool getMetadata = false;
struct addrinfo *hostAddress = NULL;
int musicFile = 1; // stdout
int controlSocket = -1;
string title("");

// Funkcje pomocnicze

int open_control_socket(uint16_t port)
{
	int sock = socket(AF_INET, SOCK_DGRAM, 0);
	
	if (sock < 0)
		return sock;

	sockaddr_in server_address;
	server_address.sin_family = AF_INET;
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);
	server_address.sin_port = htons(port);

	if (bind(sock, (struct sockaddr *) &server_address,
			(socklen_t) sizeof(server_address)) < 0) {
		close(sock);
		return -1;
	}
	
	return sock;
}

void parse_args(int argc, char *argv[])
{
	if (argc != 7)
		fatal("Usage: ./player host path r-port file m-port md");
	
	// Czy mamy pobierać metadane?
	if (!strcmp(argv[6], "no"))
		getMetadata = false;
	else if (!strcmp(argv[6], "yes"))
		getMetadata = true;
	else
		fatal("Niepoprawne ustawienie pobierania metadanych");
	
	// Otwarcie gniazda nasłuchującego na rozkazy
	unsigned short int port = 0;
	
	if (sscanf(argv[5], "%hu", &port) == 0)
		fatal("Zła wartość m-port");
	
	controlSocket = open_control_socket(port);
	
	if (controlSocket < 0)
		syserr("Nie udało się otworzyć gniazda do nasłuchiwania");
	
	// Otwarcie pliku do wypisywania strumienia
	if (strcmp(argv[4], "-"))
		musicFile = open(argv[4], O_CREAT | O_TRUNC | O_RDWR, 0666);
	
	if (musicFile == -1)
		syserr("Nie udało się otworzyć podanego pliku");
	
	// Znalezienie hosta z muzyką
	if (get_host_adress(argv[1], argv[3], hostAddress) != 0)
		syserr("Nie można odnaleźć hosta");
	
	musicPath = argv[2];
}

void clean_up()
{
	close(controlSocket);
	close(musicFile);
	freeaddrinfo(hostAddress);
}

int request_music_stream()
{
	int sock = connect_to_server(hostAddress);
	
	if (sock < 0)
		return sock;
	
	int error = 0;
	error |= write(sock, "GET ", strlen("GET "));
	error |= write(sock, musicPath, strlen(musicPath));
	error |= write(sock, " HTTP/1.0\r\n", strlen(" HTTP/1.0\r\n"));
	
	if (getMetadata)
		error |= write(sock, "Icy-MetaData:1\r\n", strlen("Icy-MetaData:1\r\n"));
	else 
		error |= write(sock, "Icy-MetaData:0\r\n", strlen("Icy-MetaData:0\r\n"));
	
	error |= write(sock, "\r\n", strlen("\r\n"));
	
	if (error < 0) {
		close(sock);
		return -1;
	}
	
	return sock;
}

/* Parsuje odpowiedź serwera z muzyką na zapytanie
 * 
 * Wynik to:
 * -1 gdy nastąpił błąd
 * 0 gdy trzeba poczekać na dane
 * 1 gdy wszystko odebrano
 * 
 * Jeśli metaint == 0 to znaczy to, że nie zostało ono odebrane lub 
 * nagłowek zawierający metainta jest błędny
 */
int music_request_response(int sock, int& metaint, int& buffcount)
{
	static int linecount = 0;
	static char buff[255];
	
	if (buffcount == 0) {
		linecount++;
		
		if (linecount > 200) {
			linecount = 0;
			return -1;
		}
		
		memset(buff, 0, 255);
	}
	
	int res = read(sock, buff + buffcount, 1);
	
	buffcount += res;
	
	if (buffcount >= 250) {
		linecount = 0;
		return -1;
	}
	
	if (buffcount >= 2 && buff[buffcount - 2] == '\r' 
	                   && buff[buffcount - 1] == '\n') {
		buff[buffcount] = '\0';
		buffcount = 0;
		cmatch what;
		
		// Pierwsza linia - wszystko OK
		static regex OK200("^ICY 200 OK\r\n$");
		
		if (linecount == 1 && !regex_match(buff, what, OK200)) {
			linecount = 0;
			return -1;
		}
		
		// Linia z metaint
		static regex metaintline("^icy-metaint:([0-9]{1,8})\r\n$");
		
		if (regex_match(buff, what, metaintline)) 
			metaint = std::atoi(what[1].first);
		
		// Koniec nagłówka
		static regex headerend("^\r\n$");
		
		if (regex_match(buff, what, headerend)) {
			linecount = 0;
			return 1;
		}
	}
	
	if (res <= 0) {
		linecount = 0;
		return -1;
	}
	
	return 0;
}

int connect_to_stream(int& metaint)
{
	int sock = request_music_stream();
	
	if (sock < 0)
		return -1;
	
	time_t start = time(NULL);
	int buffcount = 0;
	
	pollfd stream;
	stream.revents = 0;
	stream.events = POLLIN;
	stream.fd = sock;
	
	// Odlicz 5 sekund od pierwszego uruchomienia poll
	while (difftime(time(NULL), start) < 5.0) {
		stream.revents = 0;
		int events = poll(&stream, 1, 1000);
		
		if (events == 0)
			continue;
		
		if (events < 0 || (stream.revents & (POLLERR | POLLHUP | POLLNVAL))) {
			close(sock);
			return -1;
		}
		
		if (stream.revents & POLLIN) {
			int res = music_request_response(sock, metaint, buffcount);
			
			if (res == -1) {
				close(sock);
				return -1;
			}
			
			if (res == 1)
				return sock;
		}
	}
	
	close(sock);
	return -1;
}

// Zwracane wartości takie jak w read
int read_music_stream(int sock, int metaint, int& metacounter)
{
	static char metabuff[4080 + 10];
	static int metalen = 0;
	char buff[255];
	
	if (getMetadata && metacounter == metaint + metalen) { // Zmiana stanu
		if (metalen > 0) {
			metabuff[metalen] = '\0';
			
			static regex songtitle("StreamTitle=\'([^\']+)\';");
			smatch what;
			if(regex_search(string(metabuff), what, songtitle))
				title = string(what[1].first, what[1].second);
			else
				return -1;
			
			metacounter = 0;
			metalen = 0;
			return 1;
		} else {
			unsigned char lenbyte;
			int res = read(sock, &lenbyte, 1);
			
			if (res == 1)
				metalen = int(lenbyte) * 16;
			
			if (metalen == 0)
				metacounter = 0;
			
			return res;
		}
	} else if (getMetadata && metacounter >= metaint) { // Metadane
		int received = metacounter - metaint;
		int res = read(sock, metabuff + received, metalen - received);
		
		if (res > 0)
			metacounter += res;
		
		return res;
	} else { // Normalne dane - muzyka
		int toread = 255;
		if (getMetadata)
			toread = min(toread, metaint - metacounter);
		
		int res = read(sock, buff, toread);
		
		if (res < 0)
			return -1;
		
		write(musicFile, buff, res);
		metacounter += res;
		
		return res;
	}
}

int main(int argc, char *argv[])
{
	parse_args(argc, argv);
	
	// Granie muzyki
	int metaint = 0, metacounter = 0;
	
	pollfd connections[2];
	connections[0].fd = controlSocket;
	connections[1].fd = connect_to_stream(metaint);
	
	if ((getMetadata && metaint == 0) || (!getMetadata && metaint != 0))
		fatal("Niepoprawna wartość metaint");
	
	if (connections[1].fd < 0)
		syserr("Nie można podłączyć do serwera radia lub radio wysłało "
		       "nieprawidłowy nagłówek lub nastąpił timeout");
	
	connections[0].events = POLLIN;
	connections[1].events = POLLIN;
	
	time_t radioresponse = time(NULL);
	
	while (true) { 
		if (difftime(time(NULL), radioresponse) > 5.0 && connections[1].fd >= 0) // Limit 5 sekund
			fatal("Radio ucichło i nie zamknęło połączenia!");
		
		connections[0].revents = 0;
		connections[1].revents = 0;
		
		int count = poll(connections, 2, 500);
		
		if (count < 0)
			syserr("poll nie działa");
		
		// Czytanie odpowiedzi radyjka
		
		if (connections[1].revents & (POLLERR | POLLNVAL | POLLHUP))
			syserr("błąd poll przy czytaniu strumienia");
		
		if (connections[1].revents & POLLIN) {
			radioresponse = time(NULL); // Update czasu odpowiedzi
			
			int res = read_music_stream(connections[1].fd, metaint, metacounter);
		 
			if (res == 0) 
				break;
			
			if (res < 0)
				syserr("Błąd przy czytaniu strumienia");
		}	
		
		// Polecenia sterujące
		
		if (connections[0].revents & POLLIN) {
			char buff[6];
			struct sockaddr_in client;
			socklen_t rcvalen = (socklen_t) sizeof(client);
			
			int len = recvfrom(connections[0].fd, buff, sizeof(buff), 0,
			                   (struct sockaddr *) &client, &rcvalen);
			
			if (len == 4 && !strncmp("QUIT", buff, 4))
				break;
			
			if (len == 4 && !strncmp("PLAY", buff, 4) && connections[1].fd == -1) {
				connections[1].fd = connect_to_stream(metaint);
				radioresponse = time(NULL);
				metacounter = 0;
				
				if ((getMetadata && metaint == 0) || (!getMetadata && metaint != 0))
					fatal("Niepoprawna wartość metaint");
	
				if (connections[1].fd < 0)
					syserr("Nie można podłączyć do serwera radia lub radio wysłało "
					       "nieprawidłowy nagłówek lub nastąpił timeout");
			}
			
			if (len == 5 && !strncmp("PAUSE", buff, 5) && connections[1].fd != -1) {
				close(connections[1].fd);
				connections[1].fd = -1;
			}
			
			if (len == 5 && !strncmp("TITLE", buff, 5)) {
				sendto(connections[0].fd, title.c_str(), title.size(), 0,
				       (struct sockaddr *) &client, rcvalen);
			}
		}
	}
	
	// Koniec programu
	if (connections[1].fd != -1) {
		close(connections[1].fd);
		connections[1].fd = -1;
	}
	
	clean_up();
	
	return 0;
}
