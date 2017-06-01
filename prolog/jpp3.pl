% Drzewa binarne, BST
% TODO

% wszerz(D, L) wtw, gdy L = lista wszystkich wierzchołków wszerz
wszerz(D, L) :- wszerzK([D | R] - R, L).
wszerzK(K - _, L) :- var(K), !, L = [].
wszerzK([nil | K] - R, L) :- wszerzK(K - R, L).
wszerzK([node(L, W, P) | K] - [L, P | R], [W | X]) :- wszerzK(K - R, X)

% Grafy
% (skierowane, nieskierowane, cykliczne, acykliczne, etykietowane itd.)

% Reprezentacja grafów: zbiory krawędzi, czyli
%   listy termów postaci np. kr(A, B)
graf1([kr(a, b), kr(a,c), kr(a,d), kr(b,e), kr(c,e)]).

%   klauzule unarne typu: edge(A, B).
edge(a,b).
edge(a,c).
edge(a,d).
edge(b,e).
edge(c,e).
%   Do grafu cyklicznego:
edge(e,a).

% Grafy DAG (skierowane, acykliczne)
%  a) connect(A,B), connect(Graf,A,B) wtw, gdy istnieje ścieżka z A do B.
%  Uwaga: ścieżka = niepusty (!) ciąg krawędzi
connect(A, B) :- edge(A, B).
connect(A, B) :- edge(A, X), connect(X, B).

connect(Graf,A,B) :- member(kr(A, B), Graf).
connect(Graf,A,B) :- member(kr(A, X), Graf), connect(Graf, X, B).

%  b) path(A,B,P) wtw, gdy P = opis ścieżki z A do B, tzn. P = [A, ..., B]
path(A, B, [A, B]) :- edge(A, B).
path(A, C, [A, B | R]) :- edge(A, B), path(B, C, [B | R]).

% Grafy (nie)skierowane, (a)cykliczne
%  c) pathC(A,B,P) w dowolnym grafie skierowanym (cyklicznym)
pathC(A, B, P) :- pathC(A, B, [A], P).
pathC(A, B, _, [A, B]) :- edge(A, B).
pathC(A, C, V, [A | R]) :- edge(A, B), \+member(B, V), pathC(B, C, [B | V], R).

%  d) euler/? - czy dany graf jest grafem Eulera, czyli znalezienie
%  (sprawdzenie) ścieżki Eulera (wprost z definicji):
%  ścieżka, która przechodzi przez każdą krawędź grafu dokładnie raz
euler([B | P], Es) :- euler([B | P], B, Es).
euler([B], B, []).
%euler(P, B, V) :- TODO

% Struktury otwarte i różnicowe
% Implementacja kolejki FIFO, czyli:
% a) init(Kolejka) - inicjalizacja kolejki (na pustą)
init(A-A).

% b) get(Elem, Kolejka, NowaKolejka) - pobranie
get(E, [E | A] - B, A - B).

% c) put(Elem, Kolejka, NowaKolejka) - wstawienie
put(E, A - [E | B], A - B).

% d) empty(Kolejka) - czy kolejka pusta
empty(A - _) :- var(A).

% append na listach różnicowych
appendr(A-B, B-C, A-C).
