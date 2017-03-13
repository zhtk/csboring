#include "telnet.h"

#include <string>
#include <map>
#include <tuple>
#include <boost/regex.hpp>

using namespace std;
using namespace boost;

enum TELNET_STATE {T_NORMAL, T_CMD, T_ARG, T_SUB};
map<int, tuple<string, TELNET_STATE>> message_cache;

int telnet_create_listening_socket(uint16_t& port)
{
	int sock = socket(PF_INET, SOCK_STREAM, 0);

	if (sock < 0)
		return -1;

	struct sockaddr_in server;
	server.sin_family = AF_INET;
	server.sin_addr.s_addr = htonl(INADDR_ANY);
	server.sin_port = htons(port);
	socklen_t len = (socklen_t) sizeof(server);
	
	if (bind(sock, (struct sockaddr*) &server, len) < 0 ||
	    listen(sock, MAX_CONNECTIONS) == -1 ||
	    getsockname(sock, (struct sockaddr *) &server, &len) == -1) {
		close(sock);
		return -1;
	}
	
	port = ntohs(server.sin_port);
	return sock;
}

int telnet_accept(int sock)
{
	int connection = accept(sock, (struct sockaddr*) 0, (socklen_t*) 0);
	
	if (connection < 0)
		return -1;
	
	message_cache[connection] = make_tuple(string(), T_NORMAL);
	
	return connection;
}

void telnet_close(int sock)
{
	auto iter = message_cache.find(sock);
	
	if (iter != message_cache.end())
		message_cache.erase(iter);
	
	close(sock);
}

int telnet_fill_buffer(int socket)
{
	const int buffsize = 255;
	char buff[buffsize + 1];
	
	auto iter = message_cache.find(socket);
	
	if (iter == message_cache.end())
		return -1;
	
	int res = read(socket, buff, buffsize);
	
	if (res <= 0)
		return res;
	
	// Usuwanie znaków sterujących
	TELNET_STATE& state = get<1>(iter->second);
	
	int offset = 0;
	for (int i = 0; i < res; ++i) {
		unsigned char c = (unsigned char) buff[i];
		
		if (state == T_NORMAL && c != 255) {
			buff[offset] = buff[i];
			++offset;
		} else if (state == T_NORMAL && c == 255) {
			state = T_CMD;
		} else if (state == T_CMD && c == 250) {
			state = T_SUB;
		} else if (state == T_CMD && 251 <= c && c <= 254) {
			state = T_ARG;
		} else if (state == T_ARG) {
			state = T_NORMAL;
		} else if (state == T_SUB && c == 240) {
			state = T_NORMAL;
		}
	}
	
	// Dopisanie do bufora wiadomości
	buff[offset] = '\0';
	get<0>(iter->second) += buff;
	
	return res;
}

bool telnet_is_message(int sock)
{
	auto iter = message_cache.find(sock);
	
	if (iter == message_cache.end())
		return false;
	
	if (get<0>(iter->second).size() > MAX_MESSAGE_SIZE)
		return true;
	
	static regex crlf("(\r\n|\r|\n)");
	smatch what;
	return regex_search(get<0>(iter->second), what, crlf);
}

TelnetCommand telnet_read_message(int sock)
{
	TelnetCommand res;
	res.type = TelnetCommand::Type::INVALID;
	
	auto iter = message_cache.find(sock);
	
	if (iter == message_cache.end())
		return res;
	
	static regex command1("^(.*)(\r\n)"), command2(("^(.*)(\r|\n)"));
	smatch what;
	
	if (!regex_search(get<0>(iter->second), what, command1) &&
	    !regex_search(get<0>(iter->second), what, command2)) {
		if (get<0>(iter->second).size() > MAX_MESSAGE_SIZE) {
			get<0>(iter->second) = "";
			res.type = TelnetCommand::Type::TOOLONG;
		}
		
		return res;
	}
	
	string text = string(what[1].first, what[1].second);
	get<0>(iter->second) = get<0>(iter->second).substr(what[0].length());
	
	static regex start("START ([^\\s;]+) ([^\\s;]+ [^\\s;]+ [^\\s;]+ ([^\\s;]+) ([^\\s;]+) [^\\s;]+)");
	static regex player("(PLAY|PAUSE|QUIT|TITLE) ID(\\d+)");
	static regex at("AT (\\d\\d)\\.(\\d\\d) (\\d{1,8}) ([^\\s;]+) ([^\\s;]+ [^\\s;]+ [^\\s;]+ ([^\\s;]+) ([^\\s;]+) [^\\s;]+)");
	
	if (regex_match(text, what, start)) {
		if (string(what[3].first, what[3].second) == "-")
			return res;
		
		res.type = TelnetCommand::Type::START;
		res.machine = string(what[1].first, what[1].second);
		res.params1 = string(what[2].first, what[2].second);
		res.params2 = string(what[4].first, what[4].second);
	} else if (regex_match(text, what, player)) {
		res.type = TelnetCommand::Type::ASK;
		res.params1 = string(what[1].first, what[1].second);
		res.machine = string(what[2].first, what[2].second);
	} else if (regex_match(text, what, at)) {
		if (string(what[6].first, what[6].second) == "-")
			return res;
		
		res.type = TelnetCommand::Type::AT;
		res.machine = string(what[4].first, what[4].second);
		res.params1 = string(what[1].first, what[1].second);
		res.params2 = string(what[2].first, what[2].second);
		res.params3 = string(what[3].first, what[3].second);
		res.params4 = string(what[5].first, what[5].second);
		res.params5 = string(what[7].first, what[7].second);
	}
	
	return res;
}
