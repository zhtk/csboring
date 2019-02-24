% listaProl(L) :- L jest listą prologową
listaProl([]).
listaProl([_|Y]) :- listaProl(Y).

% mojaLista(L) :- L jest listą w mojej notacji
% Notacja: 1; 2; 3; nil = [1, 2, 3]
:- op(500, xfy, ;).
mojaLista(nil).
mojaLista(_ ; X) :- mojaLista(X).

% pierwszy(E, L) wtw, gdy E jest pierwszym elementem L
pierwszy(E, [E | _]).

% ostatni(E, L) wtw, gdy E jest ostatnim elementem listy L
ostatni(E, [E]).
ostatni(E, [_, L]) :- ostatni(E, L).

% element(E, L) wtw, gdy  E jest elementem L (member/2)
element(E, [E | _]).
element(E, [_ | L]) :- element(E, L).

% intersection(L1, L2) wtw, gdy zbiory (listy) L1 i L2 nie sa rozłączne
intersection(L1, L2) :- element(E, L1), element(E, L2).

% scal(L1, L2, L3) wtw, gdy L1 o L2 = L3 (scalanie list; append/3)
scal([], L, L).
scal([H | T], L2, [H | L3]) :- scal(T, L2, L3).

% Zastosowania scal:
% pierwszy(E, L) :- scal([E], _, L).
% ostatni(E, L) :- scal(_, [E], L).
% element(E, L) :- scal(_, [E | _], L).
% prefix(P, L) :- scal(P, _, L).
% sufix(S, L) :- scal(_, S, L).
% bezostatniego(L, W) :- scal(W, [_], L).

% podziel(Lista, NieParz, Parz) wtw, gdy NieParz (odp. Parz) jest
% listą zawierającą wszystkie elementy listy Lista znajdujące się na
% miejscach o nieparzystych (odp. parzystych) indeksach (zał. indeksujemy od 1)
podziel([], [], []).
podziel([X | L], [X | P], Np) :- podziel(L, Np, P).

% wypisz(L) == czytelne wypisanie wszystkich elementów listy L
% (elementy oddzielone przecinkami, na końcu kropka, lub CR,
% info gdy lista pusta)
wypisz([]) :- write('Pusta lista'), nl.
wypisz([A|L]) :-
	append(IL, [_], [A|L]),
	member(X, IL),
	write(X),
	write(', '),
	fail.
wypisz([A|L]) :-
	append(_, [X], [A|L]),
	write(X),
	write('.').

% podlista(P, L) wtw, gdy P jest spójną podlistą L 
% podlista(B, ABC) :-
%	append(B, BC, ABC),
%	append(B, C, BC).
podlista([], _).
podlista([E|P], L) :-
	append(_, K, L),
	append([E|P], _, K).

% podciag(P, L) wtw, gdy P jest (nie)spójną podlistą L (podciągiem)
podciag([], _).
podciag([X|Y], L) :-
	append(_, [X|L2], L),
	podciag(Y, L2).

% srodek(E, L) wtw, gdy E jest środkowym elementem listy L
% (L jest nieparzystej długości)
% czyli (k+1)-ym elementem, gdzie 2k+1 = długość listy L
% Koszt liniowy, 3 klauzule

