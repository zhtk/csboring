% Korzystamy z techniki z akumulatorem

% suma(L, S) wtw, gdy S = suma elementów listy L
suma([], A, S) :- S is A.
suma([E | L], A, S) :- S1 is A + E, suma(L, S1, S).
suma(L, S) :- suma(L, 0, S).

% dlugosc(L, K) wtw, gdy K = liczba elementów listy L (length/2)
dlugosc([], A, K) :- K is A.
dlugosc([_ | L], A, K) :- A1 is A + 1, dlugosc(L, A1, K).
dlugosc(L, K) :- dlugosc(L, 0, K).

% min(L, M) wtw, gdy M jest minimalnym elementem L (L = lista np. liczb całkowitych)
min([], A, M) :- M is A.
min([E | L], A, M) :- E >= A, min(L, A, M).
min([E | L], A, M) :- E < A, min(L, E, M).

min([E | L], M) :- min(L, E, M).

% odwroc(L, R) wtw, gdy R jest odwróconą listą L (np. odwroc([1,2,3,4], [4,3,2,1]) - sukces)
odwroc([], R, R).
odwroc([E | L], A, R) :- odwroc(L, [E | A], R).
odwroc(L, R) :- odwroc(L, [], R).

% palindrom(Slowo) wtw, gdy (lista) Slowo jest palindromem 
% (np. palindrom([k,a,j,a,k]), palindrom([1,b,2,b,1]) - sukcesy)
palindrom(S) :- odwroc(S, S).

% slowo(Slowo) == Slowo= a^nb^n (Uwaga: bez arytmetyki!)
% dwa warianty: (*) n > 0 (**) n >= 0 (np. slowo([a,a,b,b]) - sukces)

% wariant (*):
slowo([], [], pp).
slowo([b | L], [a | S], pp) :- slowo(L, S, pp).
slowo([b | L], S, ps) :- slowo([b | L], S, pp).
slowo([a | L], S, ps) :- slowo(L, [a | S], ps).
slowo(S) :- slowo(S, [], ps).
% wariant (**): dodatkowa linia
slowo([]).

% slowo(Zdanie, Reszta) == Zdanie = Slowo * Reszta, 
% Slowo - jw. (np. slowo([a,a,b,b,c,d], [c,d]) - sukces)

