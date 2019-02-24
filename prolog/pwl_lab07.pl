% 1. Zdefiniuj predykat qsort(L, S) implementujący algorytm QuickSort.
%    a) wersja bez akumulatora (definicja algorytmu)
%    b) wersja z akumulatorem.
quicksort([], []).
quicksort([E|L], W) :-
	partition(L, E, W1, W2),
	quicksort(W1, W1S),
	quicksort(W2, W2S),
	append(W1S, [E|W2S], W).

qsort(L, W) :- qsort(L, [], W).
qsort([], A, A).
qsort([E|L], A, W) :-
	partition(L, E, W1, W2),
	qsort(W2, A, A1),
	qsort(W1, [E|A1], W).

% partition(Lista, Element, Mniejsze, RowneWieksze)
partition([], _, [], []).
partition([X|L], E, [X|R1], R2) :-
	X < E,
	partition(L, E, R1, R2).
partition([X|L], E, R1, [X|R2]) :-
	X >= E,
	partition(L, E, R1, R2).

% 2. Zdefiniuj predykat flatten(LL, L) odnoszący sukces wtw, gdy lista LL
%    jest zagnieżdżoną listą list, której elementami są liczby całkowite, a
%    lista L jest płaską listą wszystkich liczb całkowitych z listy LL (w tej samej kolejności).
%
%    Np. ?- flatten([[[[1,[2,[3],4],5],6],[]],7], L).
%    L = [1,2,3,4,5,6,7];
%    no

flatten(LL, W) :- flatten(LL, [], W).
flatten([], W, W).
flatten(L, A, [L|A]) :-
	integer(L),
	!.
flatten([X|L], A, W) :-
	flatten(X, FLL, W),
	flatten(L, A, FLL).

% 3. Zdefiniuj predykat qsort/2 tak, aby głębokość stosu rekurencyjnych
%    wywołań qsort była logarytmiczna (oczywiście bez append/3).

% 4. Liczby całkowite (ze znakiem) reprezentujemy za pomocą termów postaci
%
%     liczba(s^i(x), s^j(x)),
%     gdzie i-j jest wartością liczby, a x jest unikatową zmienną.
%
%    Zdefiniuj predykat suma(l, s), który dla podanej listy liczb całkowitych
%    odnosi sukces wtw, gdy s jest sumą wszystkich liczb będących elementami
%    listy l.
%
%    Przykładowe (poprawne) zapytanie:
%      suma([liczba(s(X), X), liczba(s(s(Y)), s(Y)), liczba(Z, s(Z))], W).
%    Wynikiem powinien być (dowolny) term reprezentujący liczbę 1.

