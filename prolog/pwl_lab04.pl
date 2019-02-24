% 1. Zdefiniuj predykaty
%    a) podlista(P, L) wtw. gdy P jest (spójną) podlistą L
%        podciąg(P, L) wtw, gdy P jest podciągiem L (niespójną podlistą)
%       Uwaga: oba predykaty nie powinny udzielać redundantnych odpowiedzi (i być sprawne).
%    b) tuzPrzed(X, Y, L) == X występuje bezpośrednio przed Y na liście L

tuzPrzed(X, Y, L) :-
	append(_, [X,Y|_], L).

%    c) przed(X, Y, L) == X występuje przed Y na liście L

przed(X, Y, L) :-
	append(_, [X|L1], L),
	member(Y, L1).

%2. Zdefiniuj predykaty
%    a) odwrotna(L, R) wtw, gdy R jest odwróceniem listy L
% TODO lepsza definicja do wymyślenia

odwrotna(L, R) :- odwrotna(L, [], R).
odwrotna([], R, R).
odwrotna([E|L], A, R) :- odwrotna(L, [E|A], R).

%    b) palindrom( L ) wtw, gdy lista L jest palindromem

palindrom(L) :- palindrom(L, [], L).
palindrom([], L, L).
palindrom([E|L], A, O) :- palindrom(L, [E|A], O).

%    c) sufiksy(L, S) wtw, gdy S = lista wszystkich sufiksów L
%                                  (od najkrótszych do najdłuższych
%                               lub od najdłuższych do najkrótszych)

sufiksy([], [[]]).
sufiksy([E|L], [[E|L]|S]) :- sufiksy(L, S).

%    d) flaga polska (+ ew. flaga holenderska)
%       fp(L, F) wtw, gdy L = lista stałych b oraz c,
%                         F = lista posortowana (flaga polska)
%
%       Np. ?- fp([b,c,b,c], [b,b,c,c]).   - sukces
%           ?- fp([c,b,c,b,c], L).
%            L = [b,b,c,c,c]
%
%    Uwaga: warto napisać (i porównać) następujące trzy wersje rozwiązania:
%       fp(L, F) :- fp(L, [], F).
%       fp2(L, F) :- podziel(L, B, C), append(B, C, F).
%       fp3(L, F) :- fp3(L, [], [], F).

fp(L, F) :- fp(L, [], F).
fp([], L, L).
fp([c|L], C, F) :- fp(L, [c|C], F).
fp([b|L], C, [b|F]) :- fp(L, C, F).

fp3(L, F) :- fp3(L, [], [], F).
fp3([b|L], B, C, F) :- fp3(L, [b|B], C, F).
fp3([c|L], B, C, F) :- fp3(L, B, [c|C], F).
fp3([], B, C, F) :- append(B, C, F).

%    d) slowo( S ) wtw, gdy S = a^n b^n
%       (S = ustalona lista reprezentująca słowo)
%           i)  n >= 0
%           ii) n > 0
%       Np. slowo([a,a,b,b]) - sukces, zaś slowo([a,b,a,b]) - porażka.

slowo(S) :- slowo(S, []).
slowo([a|S], B) :- slowo(S, [b|B]).
slowo(B, B). % wersja n >= 0
% slowo([b|B], [b|B]). % wersja n > 0

%    e) slowoR(S, Reszta) wtw, gdy S = a^n b^n  * Reszta, n>= 1
%
%       Np. ?- slowo([a,a,b,b,b,c,d], R).
%       R = [b,c,d]
slowoR([a|S], R) :- slowoR(S, [b|R]).
slowoR([b|S], [b|S]).

