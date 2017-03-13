#include "err.h"
#include "ssh.h"
#include "telnet.h"
#include "common.h"

#include <poll.h>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <set>

using namespace std;

// Bardzo ważne strukturki
struct Pipetype
{
	enum class Type {SPECIAL, SOCKET, ERR, OUT} type;
	string playerid; // ID playera
};

struct PlayerInfo
{
	int owner; // Socket sesji, która uruchomiła
	string bufferr; // Bufor na errory
	pid_t process; // Proces playera
	addrinfo *addr; // Adres nasłuchiwania playera
};

class StringCounter
{
	string next;
	
	public:
	StringCounter() : next("0") {};

	string nextValue() {
		int p = 1;

		for(int i = next.size() - 1; p && i >= 0; --i) {
			next[i] -= '0';

			next[i] += p;
			p = next[i] / 10;
			next[i] %= 10;
			next[i] += '0';
		}
	
		if(p)
			next = "1" + next;
	
		return next;
	}
 
	string value() {
		return next;
	}
} id_getter;

struct TaskEvent 
{
	enum class Type {TITLE, START, STOP} type;
	time_t time;
	TelnetCommand cmd;
	string id;
	
	bool operator< (const TaskEvent& rhs) const
	{
		return time < rhs.time;
	}
};

// Kontenery na dane
map<string, PlayerInfo> players;
vector<pollfd> clients;
vector<Pipetype> clients_types;
multiset<TaskEvent> events;

void start_player(TelnetCommand& cmd, int owner)
{
	PlayerInfo pi;
	pi.owner = owner;
	
	if (get_udp_host_adress(cmd.machine.c_str(), cmd.params2.c_str(), pi.addr) < 0) {
		static string msg("ERROR Niepoprawny argument - nie mozna znalezc hosta\n");
		write(owner, msg.c_str(), msg.size());
		return;
	}
	
	int outpipe, errpipe;
	pi.process = ssh_execute_command(cmd.machine, "player " + cmd.params1, outpipe, errpipe);
	
	if (pi.process < 0) {
		freeaddrinfo(pi.addr);
		static string msg("ERROR SSH nie dziala\n");
		write(owner, msg.c_str(), msg.size());
		return;
	}
	
	string id = id_getter.nextValue();
	players[id] = pi;
	
	string msg = "OK ID";
	msg += id;
	msg += "\n";
	write(owner, msg.c_str(), msg.size());
	
	Pipetype pt;
	pt.type = Pipetype::Type::ERR;
	pt.playerid = id;
	clients_types.push_back(pt);
	
	pollfd fd;
	fd.fd = errpipe;
	fd.events = POLLIN | POLLHUP;
	clients.push_back(fd);
	
	close(outpipe); // Obchodzą nas tylko errory
}

// Sprząta po playerze - deskryptory, mapę, eventy
void delete_player(string id)
{
	// Sprzątanie mapy
	auto iter = players.find(id);
	if (iter != players.end()) {
		PlayerInfo& pi = iter->second;
		freeaddrinfo(pi.addr);
		players.erase(iter);
	}
	
	// Sprzątanie deskryptorów
	for (unsigned int i = 0; i < clients.size(); ++i)
		if (clients_types[i].type == Pipetype::Type::ERR &&
		    clients_types[i].playerid == id) {
			close(clients[i].fd);
			clients[i].fd = -1;
		}
	
	// Usuń eventy
	auto it = events.begin();
	while (it != events.end())
		if (it->id == id) {
			events.erase(it);
			it = events.begin();
		}
}

void check_player(int num)
{
	if (clients_types[num].type != Pipetype::Type::ERR ||
	    !(clients[num].revents & (POLLIN | POLLHUP | POLLERR | POLLNVAL)))
		return;

	auto iter = players.find(clients_types[num].playerid);
	if (iter == players.end()) {
		delete_player(clients_types[num].playerid); // Coś tu nie gra...
		return;
	}

	PlayerInfo& info = iter->second;
	const int BUFF_SIZE = 255;
	char buff[BUFF_SIZE + 1];
	
	int res = read(clients[num].fd, buff, BUFF_SIZE);
	
	if (res > 0) {
		buff[res] = '\0';
		info.bufferr += buff;
		return;
	}
	
	// Koniec połączenia z playerem
	res = ssh_check_status(info.process);
	
	if (res == -1)
		return;
	
	if (res > 0) {
		string msg = "ERROR ID";
		msg += clients_types[num].playerid;
		msg += " Kod: ";
		msg += to_string(res);
		msg += "; Output: ";
		msg += info.bufferr;
		write(info.owner, msg.c_str(), msg.size());
	}

	delete_player(clients_types[num].playerid);
}

void ask_player(TelnetCommand cmd, int telnet, bool notify = true)
{
	auto iter = players.find(cmd.machine);
	if (iter == players.end()) {
		string msg = "ERROR ID" + cmd.machine + " taki player nie istnieje\n";
		write(telnet, msg.c_str(), msg.size());
		return;
	}
	
	PlayerInfo& info = iter->second;
	
	// Jeśli QUIT i player nie ma pid to usuń go z kolejki
	if (cmd.params1 == "QUIT" && info.process == -1) {
		delete_player(cmd.machine);
		string msg = "OK ID" + cmd.machine + "\n";
		write(telnet, msg.c_str(), msg.size());
		return;
	}
	
	if (info.process == -1) {
		string msg = "ERROR ID" + cmd.machine + " player nie jest uruchomiony\n";
		write(telnet, msg.c_str(), msg.size());
		return;
	}
	
	// Wysyłamy polecenie
	int res = sendto(clients[1].fd, cmd.params1.c_str(), cmd.params1.size(), 0, 
	                 info.addr->ai_addr, info.addr->ai_addrlen);
	
	if (res == -1) {
		string msg = "ERROR ID" + cmd.machine + " nieudana wysylka polecenia " + cmd.params1 + "\n";
		write(telnet, msg.c_str(), msg.size());
		return;
	}
	
	// Jeśli title to dodajemy event za 3 sekundy, nic nie wypisujemy
	if (cmd.params1 == "TITLE") {
		TaskEvent event;
		event.type = TaskEvent::Type::TITLE;
		event.id = cmd.machine;
		
		event.time = time(NULL);
		tm *local = localtime(&event.time);
		local->tm_sec += 3;
		event.time = mktime(local);
		
		events.insert(event);
		
		return;
	}
	
	string msg = "OK ID" + cmd.machine + "\n";
	if (notify)
		write(telnet, msg.c_str(), msg.size());
}

void at_command(TelnetCommand cmd, int telnet)
{
	// Dodanie eventa
	TaskEvent event;
	event.type = TaskEvent::Type::START;
	event.id = id_getter.nextValue();
	event.cmd = cmd;
	
	int hour = atoi(cmd.params1.c_str());
	int minute = atoi(cmd.params2.c_str());
	
	if (hour >= 24 || minute >= 60) {
		static string msg("ERROR niepoprawny format czasu\n");
		write(telnet, msg.c_str(), msg.size());
		return;
	}
	
	event.time = time(NULL);
	tm *local = localtime(&event.time);
	
	if (local->tm_hour > hour || (local->tm_hour == hour && local->tm_min > minute))
		local->tm_mday += 1;
	
	local->tm_hour = hour;
	local->tm_min = minute;
	local->tm_sec = 0;
	
	event.time = mktime(local);
	
	// Dodanie do listy playerów
	PlayerInfo pi;
	pi.owner = telnet;
	pi.process = -1;
	
	if (get_udp_host_adress(cmd.machine.c_str(), cmd.params5.c_str(), pi.addr) < 0) {
		static string msg("ERROR Niepoprawny argument - nie mozna znalezc hosta\n");
		write(telnet, msg.c_str(), msg.size());
		return;
	}
	
	// Akceptacja i wysłanie odpowiedzi
	players[event.id] = pi;
	events.insert(event);
	string msg = "OK ID" + event.id + "\n";
	write(telnet, msg.c_str(), msg.size());
}

void check_client(int num)
{
	if (clients[num].fd == -1 || clients_types[num].type != Pipetype::Type::SOCKET)
		return;
	
	if (!(clients[num].revents & POLLIN))
		return;

	int res = telnet_fill_buffer(clients[num].fd);
		
	if (res <= 0) {
		for (auto pi : players)
			if (pi.second.owner == clients[num].fd)
				pi.second.owner = -1;
		
		telnet_close(clients[num].fd);
		clients[num].fd = -1;
		return;
	}
	
	while (telnet_is_message(clients[num].fd)) {
		TelnetCommand cmd = telnet_read_message(clients[num].fd);
		static string invalidcmd("ERROR Niepoprawne polecenie lub jego argument\n");
		static string toolong("ERROR Zbyt dlugie polecenie, ucinam bufor\n");
		
		if (cmd.type == TelnetCommand::Type::INVALID)
			write(clients[num].fd, invalidcmd.c_str(), invalidcmd.size());
		else if (cmd.type == TelnetCommand::Type::TOOLONG)
			write(clients[num].fd, toolong.c_str(), toolong.size());
		else if (cmd.type == TelnetCommand::Type::START)
			start_player(cmd, clients[num].fd);
		else if (cmd.type == TelnetCommand::Type::ASK)
			ask_player(cmd, clients[num].fd);
		else if (cmd.type == TelnetCommand::Type::AT)
			at_command(cmd, clients[num].fd);
	}
}

void accept_remote_client()
{
	pollfd fd;
	fd.fd = telnet_accept(clients[0].fd);
	fd.events = POLLIN;

	if (fd.fd != -1)		
		clients.push_back(fd);
	
	Pipetype pt;
	pt.type = Pipetype::Type::SOCKET;
	clients_types.push_back(pt);
}

bool is_same_sender(struct sockaddr_in& got, struct sockaddr& exp)
{
	if (got.sin_family != exp.sa_family)
		return false;
	
	sockaddr_in& ex = (sockaddr_in&) exp;
	
	if (got.sin_port != ex.sin_port || got.sin_addr.s_addr != ex.sin_addr.s_addr)
		return false;
	
	return true;
}

void receive_title(int sock)
{
	struct sockaddr_in addr;
	const int BUFFER_MAX = 4080; // Maksymalna długość nagłowka icecast
	char buff[BUFFER_MAX + 1];
	
	socklen_t addr_len = (socklen_t) sizeof(addr);
    int len = recvfrom(sock, buff, BUFFER_MAX, 0, (struct sockaddr *) &addr, &addr_len);

    if (len < 0)
		return;
	buff[len] = '\0';
	
	// Znaleźć sesję telneta oraz id
	auto iter = players.begin();
	
	while (iter != players.end()) {
		if (is_same_sender(addr, *(iter->second.addr->ai_addr)))
			break;
		
		++iter;
	}
	
	if (iter == players.end())
		return;
	
	// Usunąć z kolejki
	for (auto elem = events.begin(); elem != events.end(); ++elem)
		if (elem->type == TaskEvent::Type::TITLE && elem->id == iter->first) {
			events.erase(elem);
			break;
		}
	
	// Wysłać odpowiedź
	string msg = "OK ID" + iter->first + " " + buff + "\n";
	write(iter->second.owner, msg.c_str(), msg.size());
}

void at_event(TaskEvent& event)
{
	// Wyszukanie playera na liście
	auto iter = players.find(event.id);
	if (iter == players.end())
		return;
	
	// Uruchomienie playera
	int outpipe, errpipe;
	iter->second.process = ssh_execute_command(event.cmd.machine, 
	                                           "player " + event.cmd.params4, 
	                                           outpipe, errpipe);
	
	if (iter->second.process < 0) {
		static string msg("ERROR SSH nie dziala\n");
		write(iter->second.owner, msg.c_str(), msg.size());
		delete_player(event.id);
		return;
	}
	
	Pipetype pt;
	pt.type = Pipetype::Type::ERR;
	pt.playerid = event.id;
	clients_types.push_back(pt);
	
	pollfd fd;
	fd.fd = errpipe;
	fd.events = POLLIN | POLLHUP;
	clients.push_back(fd);
	
	close(outpipe);
	
	// Dodanie QUIT eventa
	event.type = TaskEvent::Type::STOP;
	
	event.time = time(NULL);
	tm *local = localtime(&event.time);
	local->tm_min += atoi(event.cmd.params3.c_str());
	event.time = mktime(local);
	
	events.insert(event);
}

void check_event()
{
	if (!events.size() || difftime(events.begin()->time, time(NULL)) > 0)
		return;
	
	TaskEvent ev = *(events.begin());
	events.erase(events.begin());
	
	if (ev.type == TaskEvent::Type::TITLE) {
		auto iter = players.find(ev.id);
		if (iter == players.end())
			return;
		
		string msg = "ERROR ID" + ev.id + " nie otrzymano tytulu w 3 sekundy\n";
		write(iter->second.owner, msg.c_str(), msg.size());
	} else if (ev.type == TaskEvent::Type::STOP) {
		int sock = -1;
		auto iter = players.find(ev.id);
		if (iter != players.end())
			sock = iter->second.owner;
		
		TelnetCommand cmd;
		cmd.type = TelnetCommand::Type::ASK;
		cmd.machine = ev.id;
		cmd.params1 = "QUIT";
		ask_player(cmd, sock, false);
	} else if (ev.type == TaskEvent::Type::START) {
		at_event(ev);
	}	
}

void serve_clients(int socket)
{
	clients.clear();
	clients_types.clear();
	
	// Socket akceptujący
	clients.push_back(pollfd());
	clients_types.push_back(Pipetype());
	clients[0].fd = socket;
	clients[0].events = POLLIN;
	clients_types[0].type = Pipetype::Type::SPECIAL;
	
	// Socket na UDP
	clients.push_back(pollfd());
	clients_types.push_back(Pipetype());
	clients[1].fd = ::socket(AF_INET, SOCK_DGRAM, 0);
	clients[1].events = POLLIN;
	clients_types[1].type = Pipetype::Type::SPECIAL;
	
	if (clients[1].fd < 0)
		syserr("Nie utworzono socketa UDP");
	
	// Pętla komunikatów
	while (true) {
		for (unsigned int i = 0; i < clients.size(); ++i)
			clients[i].revents = 0;
		
		// Przetwarzanie eventów
		int waittime = -1;
		
		while (events.size() && waittime <= 0) {
			waittime = difftime(events.begin()->time, time(NULL)) * 1000;
			
			// Sprawdź jeden event
			check_event();
		}
		
		if (waittime < 0)
			waittime = -1;
		
		// Poll
		poll(&(clients[0]), clients.size(), waittime);
		
		// Akceptacja połączenia klienta
		if (clients[0].revents & POLLIN)
			accept_remote_client();
		
		// Obsługa przychodzącego tytułu
		if (clients[1].revents & POLLIN)
			receive_title(clients[1].fd);
		
		// Obsługa klientów
		for (unsigned int i = 0; i < clients.size(); ++i)
			check_client(i);
		
		// Obsługa programów - playerów
		// Np. zamknięcie połączenia lub error
		for (unsigned int i = 0; i < clients.size(); ++i)
			check_player(i);
		
		// Czyszczenie tabeli z klientami - usuwanie fd == -1
		for (unsigned int i = 0; i < clients.size(); ++i) {
			if (clients[i].fd != -1)
				continue;
			
			swap(clients[i], clients[clients.size() - 1]);
			swap(clients_types[i], clients_types[clients_types.size() - 1]);
			clients.pop_back();
			clients_types.pop_back();
			
			--i;
		}
	}
	
	close(clients[0].fd);
	close(clients[1].fd);
}

int main(int argc, char *argv[])
{
	if (argc > 2)
		fatal("Wywołuj tak: ./master [port]");
	
	uint16_t port = 0;
	addrinfo *addr;
	if (argc == 2 && (get_host_adress("127.0.0.1", argv[1], addr) != 0
	                  || sscanf(argv[1], "%hu", &port) < 1 || port == 0))
		fatal("Z numerem portu jest coś nie tak");
	freeaddrinfo(addr);
	
	int sock = telnet_create_listening_socket(port);
	
	if (sock < 0)
		syserr("Nie można otworzyć socketa");
	
	if (argc < 2)
		printf("%d\n", port);
	
	serve_clients(sock);
	
	return 0;
}
