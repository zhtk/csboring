% uzg(L, W) - Uzgadnianie termów na liście L i W
uzg(L, W) :- uzg([W|L]).

uzg([]).
uzg([_]).
uzg([X, X|L]) :- uzg([X|L]).

% 1. Zdefiniuj predykat max2(L) odnoszący sukces wtw, gdy ustalona lista L
%    reprezentuje zbiór o mocy co najwyżej 2.
%    Predykat powinien odnosić jednokrotny sukces lub skończoną porażkę.
%
%    Np. max2([a,b,b,a]) - sukces,   max2([a,b,b,c,a]) - porażka.
%        max2([a,a,a,a]) - sukces

max2([]).
max2([X|L]) :- max2(L, X, _).

max2([], _, _).
max2([A|L], A, B) :- max2(L, A, B).
max2([B|L], A, B) :- max2(L, A, B).

% 2. Zdefiniuj predykat
%   rozneZmienne(L) wtw, gdy L jest listą różnych zmiennnych
%                        (dozwolone efekty uboczne)
%   Np. ?- rozneZmienne([X,Y,X]).  - porażka
%       ?- rozneZmienne([X,Y,Z]).  - sukces

rozneZmienne([]).
rozneZmienne([X|L]) :-
	unify_with_occurs_check(X, L),
	rozneZmienne(L).

% 3. Dana definicja predykatu nawiasy/2:
%    nawiasy(NawiasOtwierający, NawiasZamykający)
%    Zdefiniuj predykat wyrNawiasowe(Wyrażenie) odnoszący sukces wtw. gdy
%    podane Wyrażenie (lista) jest poprawnym wyrażeniem nawiasowym.
%
%    Na przykład:
%      nawiasy(0'(, 0')).      % definicja nawiasów
%      nawiasy(0'[, 0']).
%
%    ?- wyrNawiasowe("([()])").    sukces
%    ?- wyrNawiasowe("([[)[])").   porażka
%    ?- wyrNawiasowe("{}").        porażka

nawiasy(0'(, 0')).
nawiasy(0'[, 0']).

wyrNawiasowe(W) :- wyrNawiasowe(W, []).
wyrNawiasowe([], []).
wyrNawiasowe([O | L], A) :-
	nawiasy(O, Z),
	wyrNawiasowe(L, [Z|A]).
wyrNawiasowe([Z | L], [Z | A]) :-
	wyrNawiasowe(L, A).

% Zadanie domowe
%   Zdefinuj predykat dobre(n, p), który dla podanej liczby naturalnej n
%   (reprezentowanej za pomocą 0 i s/1) odniesie sukces uzgadniając zmienną p
%   z reprezentacją dowolnego podstawienia takiego, że:
%   *)  p^n = epsilon
%   *)  p^k <> epsilon, dla 1 <= k < n
%   Reprezentacja podstawień: lista elementów postaci para(zmienna, term).
%   Np. podstawienie {X/f(Y), Z/a, W/Y} jest reprezentowane jako lista:
%   [ para(X, f(Y)), para(Z, a), para(W, Y) ].
%
% Rozwiązanie:
%   Poszukiwanym podstawieniem jest przemianowanie zmiennych.
%
%   Zatem P = [para(X1, X2), para(X2, X3), ..., para(Xn, X1)]

use_module(library(lists)).

dobre(0, []).
dobre(s(0), []).
dobre(s(s(0)), [para(X1, X2), para(X2, X1)]).
dobre(s(s(s(N))), P) :-
	dobre(s(s(N)), Mp),
	% wyciągnięcie dwóch sąsiednich podstawień
	select(para(X1, X2), Mp, Mp2),
	select(para(X2, X3), Mp2, Mp3),
	% wpisanie pomiędzy nie trzeciego
	select(para(X1, X2), P1, Mp3),
	select(para(X2, X), P2, P1),
	select(para(X, X3), P, P2).
