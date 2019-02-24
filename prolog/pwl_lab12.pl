% 1. Informacje o pewnej funkcji, o skończonej dziedzinie i
%    przeciwdziedzinie D, są dane w postaci definicji predykatu:
%         wf(x,y) <==> f(x)=y.
%    Zdefiniuj predykat fna zachodzący wtw, gdy funkcja zdefiniowana
%    predykatem wf/2 jest "na".
%    Dziedzina funkcji może być reprezentowana jako lista elementów.
%    Napisz program bez negacji oraz program z negacją.

wf(1, 2).
wf(2, 1).

% funkcjaNa(Lista elementów uniwersum).
funkcjaNa([]).
funkcjaNa([E|U]) :-
	wf(_, E),
	funkcjaNa(U).

% Wersja z negacją
funkcjaNa2 :- \+ funkcjaNieNa.

funkcjaNieNa :-
	wf(X, _),
	\+ wPrzeciwDziedzinie(X).

wPrzeciwDziedzinie(E) :- wf(_, E).

% 2. Zdefiniuj predykat different(X,Y) zachodzący wtw, gdy zbiory X i Y są różne,
%    nie używając negacji (tj. nie używając predykatów typu nonmember/2, \=/2 itp.).

different(X, Y) :-
	member(E, X),
	niemember(E, Y).

niemember(_, []).
niemember(E, [X|R]) :- ne(E, X), niemember(E, R).

% Teraz jedyną możliwością jest wzięcie alfabetu całego programu
% i ręczne wypisanie, które elementy są nierówne.
ne(X, Y) :- X \= Y.
