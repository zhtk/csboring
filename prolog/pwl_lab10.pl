% Reprezentacja grafów (skierowanych, nieskierowanych, etykietowanych itp.)
%  a) termowa: listy krawędzi, listy/macierze sąsiedztwa
%  b) klauzulowa
%      zbiór klauzul unarnych = zbiór krawędzi, zbiór list sąsiedztwa itd.

edge(1, 2).
edge(1, 3).
edge(2, 3).
edge(3, 4).

% 1. Rozważamy grafy typu DAG (skierowane acykliczne).
%    Zdefiniuj predykaty:
%     a) connect(A, B), connect(Graf, A, B) <==> istnieje ścieżka z A do B,
%         tzn. istnieje niepusty ciąg krawędzi prowadzących z A do B
%        (dwie wersje: dwie reprezentacje grafu - termowa i klauzulowa, zbiory krawędzi)
%     b) path(A,B,P) <==> P = ścieżka z A do B (jw.),
%        tzn. P = [A, ..., B], czyli lista wierzchołków kolejnych krawędzi.

connect(A, B) :-
	edge(A, B).
connect(A, C) :-
	edge(A, B),
	connect(B, C).

connect(G, A, B) :-
	member(edge(A, B), G).
connect(G, A, C) :-
	member(edge(A, B), G),
	connect(G, B, C).

path(A, B, [A, B]) :-
	edge(A, B).
path(A, C, [A, B | R]) :-
	edge(A, B),
	path(B, C, [B | R]).

% 2. Grafy skierowane (cykliczne).
%    Zdefiniuj predykat:   pathC(A,B,P) <==> P ścieżka z A do B (jw.),

pathC(A, B, P) :- pathC(A, B, [], P).

pathC(A, B, _, [A, B]) :-
	edge(A, B).
pathC(A, C, V, [A | P]) :-
	edge(A, B),
	nonmember(B, V),
	pathC(B, C, [A | V], P).

% 3. Grafy nieskierowane.
%    Zdefiniuj predykat euler/k (k >= 1) odnoszący sukces wtw, gdy podany
%    graf jest grafem Eulera i parametr P jest ścieżką Eulera w tym grafie
%    (tj. ścieżką przechodzącą przez każdą krawędź tego drzewa dokładnie raz).



