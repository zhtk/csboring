% Podstawowe operacje na drzewach binarnych
% -----------------------------------------

% Reprezentacja (termowa) drzew binarnych:
%   nil                  - drzewo puste
%   tree(Lewe, W, Prawe) - drzewo niepuste

% Zdefiniuj predykaty:
% a) drzewo(D) == D jest drzewem binarnym

drzewo(nil).
drzewo(tree(L, _, P)) :-
	drzewo(L),
	drzewo(P).

% b) ileW(D, K) == drzewo binarne D składa się z K wierzchołków
ileW(D, K) :- ileW(D, 0, K).
ileW(nil, A, A).
ileW(tree(L, _, P), A, K) :-
	A1 is A + 1,
	ileW(L, A1, K1),
	ileW(P, K1, K).

% c) insertBST(D, E, N) == N jest drzewem BST powstałym z (drzewa BST) D przez dodanie E
insertBST(nil, E, tree(nil, E, nil)).
insertBST(tree(L, K, P), E, tree(W, K, P)) :-
	K > E,
	!,
	insertBST(L, E, W).
insertBST(tree(L, K, P), E, tree(L, K, W)) :-
	K =< E,
	insertBST(P, E, W).

% d) createBST(L, D) == D jest drzewem BST zawierającym wszystkie wartości z listy L
createBST(L, D) :- createBST(L, nil, D).
createBST([], A, A).
createBST([E|L], A, D) :-
	insertBST(A, E, D1),
	createBST(L, D1, D).

% e) wypiszBST(D) == wypisanie wszystkich elementów drzewa BST
%                    (porządek infiksowy; prosto, ale czytelnie)
wypiszBST(nil).
wypiszBST(tree(L, E, P)) :-
	wypiszBST(L),
	write(E),
	write('; '),
	wypiszBST(P).

% f) wypiszBST(D, L) == L = lista wszystkich elementów drzewa BST
%                       (porządek infiksowy)
wypiszBST(D, L) :- wypiszBST(D, [], L).
wypiszBST(nil, A, A).
wypiszBST(tree(P1, E, P2), A, L) :-
	wypiszBST(P2, A, L1),
	wypiszBST(P1, [E|L1], L).

% g) sortBST(L, S) == S = wynik sortowania L przy użyciu drzew BST
sortBST(L, S) :-
	createBST(L, D),
	wypiszBST(D, S).

% h) liscie(D, L) == L = lista wszystkich lisci drzewa D
%                         (od lewej do prawej)
liscie(D, L) :- liscie(D, [], L).
liscie(nil, A, A).
liscie(tree(nil, E, nil), A, [E|A]).
liscie(tree(P1, _, P2), A, L) :-
	% (P1 \= nil; P2 \= nil), % ten warunek odnosi sukces 2 razy
	(P1, P2) \= (nil, nil),   % teraz dobrze
	liscie(P2, A, L1),
	liscie(P1, L1, L).

% i) wszerz(Drzewo, ListaWszerz) == lista wierzchołków drzewa
%          (przejście po drzewie binarnym wszerz; klasyczny algorytm)
wszerz(D, LW) :- wszerz_p([D], LW).
wszerz_p([], []).
wszerz_p(Q, LW) :-
	append(Q1, [nil], Q),
	!,
	wszerz_p(Q1, LW).
wszerz_p(Q, [E|LW]) :-
	append(Q1, [tree(L, E, P)], Q),
	wszerz_p([P, L | Q1], LW).

% j) depth(Drzewo, GłębokośćDrzewa)
% Zadanie domowe:  deleteBST(D, E, NoweD).
