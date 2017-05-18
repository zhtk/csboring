% dziecko(Dziecko, Matka, Ojciec, płeć)
dziecko(jasio, ewa, jan, ch).
dziecko(stasio, ewa, jan, ch).
dziecko(basia, anna, piotr, dz).
dziecko(jan, ela, jakub, ch).

% dziecko(Dziecko, Matka, Ojciec)
%dziecko(jasio, ewa, jan).
%dziecko(stasio, ewa, jan).
%dziecko(basia, anna, piotr).
%dziecko(jan, ela, jakub).
dziecko(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, _).

ojciec(Dziecko, Ojciec) :- dziecko(Dziecko, _, Ojciec).
matka(Dziecko, Matka) :- dziecko(Dziecko, Matka, _).
rodzic(Dziecko, Rodzic) :- matka(Dziecko, Rodzic).
rodzic(Dziecko, Rodzic) :- ojciec(Dziecko, Rodzic).
babcia(Dziecko, Babcia) :- rodzic(Dziecko, X), matka(X, Babcia).
wnuk(Wnuk, BabciaDziadek) :- rodzic(Wnuk, X), rodzic(X, BabciaDziadek).
przodek(Przodek, Potomek) :- rodzic(Potomek, Przodek).
przodek(Przodek, Potomek) :- rodzic(Potomek, X), przodek(Przodek, X).

syn(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, ch).
corka(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, dz).
wnuczka(Dziecko, BabciaDziadek) :- dziecko(Dziecko, M, _, dz), rodzic(M, BabciaDziadek).
wnuczka(Dziecko, BabciaDziadek) :- dziecko(Dziecko, _, O, dz), rodzic(O, BabciaDziadek).

nat(z).
nat(s(X)) :- nat(X).
plus(z, X, X).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).
minus(X, z, X).
minus(s(X), s(Y), Z) :- minus(X, Y, Z).
fib(z, z).
fib(s(z), s(z)).
% fib(k, n) wtw, gdy n = k-ta liczba Fibonacciego
fib(s(s(K)), N) :- fib(K, N1), fib(s(K), N2), plus(N1, N2, N).
fib(z, z, s(z)).
fib(s(K), N, N1) :- fib(K, N1, A1), plus(N1, A1, N).

% lista(L) wtw, gdy L jest (prologową) listą
lista([]).
lista([_ | X]) :- lista(X).
% pierwszy(E, L) wtw, gdy E jest pierwszym elementem L
pierwszy(E, [E | _]).
% ostatni(E, L) wtw, gdy E jest ostatnim elementem L
ostatni(E, [E]).
ostatni(E, [_ | X]) :- ostatni(E, X).
% element(E, L) wtw, gdy E jest (dowolnym) elementem L
element(E, [E | _]).
element(E, [_ | L]) :- element(E, L).
% scal(L1, L2, L3) wtw, gdy L3 = konkatenacja listy L1 z L2
scal([], L, L) :- lista(L).
scal([X | L], Y, [X | Z]) :- scal(L, Y, Z).
% intersect(Z1,Z2) wtw, gdy zbiory (listy) Z1 i Z2 mają niepuste przecięcie
intersect([X | _], [X | _]).
intersect([X | _], [_ | L]) :- intersect([X], L).
intersect([_ | Y], L) :- intersect(Y, L).
% podziel(Lista, NieParz, Parz) == podział danej listy na dwie
% podlisty zawierające kolejne elementy (odpowiednio) z parzystych
% (nieparzystych) pozycji
% (np. podziel([1,3,5,7,9], [1,5,9], [3,7]) - sukces)
podziel([], [], []).
podziel([A], [A], []).
podziel([A, C | X], [A | B], [C | D]) :- podziel(X, B, D).
% podlista(P, L) wtw, gdy P jest spójną podlistą L
podlista([], L) :- lista(L).
podlista([X|Z], Y) :- scal([X|Z], _, Y).
podlista([X|Z], [_ | Y]) :- podlista([X|Z], Y).
% podciag(P, L)  wtw, gdy P jest podciągiem L
podciag([], L) :- lista(L).
podciag([X | Y], [X | Z]) :- podciag(Y, Z).
podciag(X, [_ | Y]) :- podciag(X, Y).
% wypisz(L) == czytelne wypisanie elementów listy L, z zaznaczeniem
% jeśli lista pusta (np. elementy oddzielane przecinkami, po
% ostatnim elemencie kropka)
wypisz([]) :- print(.).
wypisz([X]) :- !, print(X), print('.').
wypisz([X | Y]) :- print(X), print(','), wypisz(Y).
% sortowanie przez wstawianie:
% insertionSort(Lista, Posortowana),
% insert(Lista, Elem, NowaLista)
insert([], E, [E]).
insert([X | Y], E, [E | [X | Y]]) :- E =< X.
insert([X | Y], E, [X | Z]) :- E > X, insert(Y, E, Z).

insertionSort([], []).
insertionSort([E | L], P) :- insertionSort(L, P1), insert(P1, E, P).

% srodek(E, L) wtw, gdy E jest środkowym elementem L
% (lista nieparzystej długości; np. srodek(3,[1,2,3,4,5]))
srodek(E, [E | _], [_]).
srodek(E, [_ | X], [_, _ | Y]) :- srodek(E, X, Y).
srodek(E, X) :- srodek(E, X, X).
