TARGET: player master

player: player.cpp common.cpp err.cpp
	g++ -Wall -O2 $^ -o $@ -lboost_regex

master: master.cpp err.cpp ssh.cpp telnet.cpp common.cpp
	g++ -Wall -std=c++11 -O2 $^ -o $@ -lboost_regex

.PHONY: clean TARGET
clean:
	rm -f master player *.o *~ *.bak
