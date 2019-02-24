% Operacje na drzewach i grafach, cd.

% 1. Zdefiniuj predykat   prawiBracia(Drzewo, W, Bracia)
% odnoszący sukces wtw, gdy w drzewie binarnym Drzewo występuje wierzchołek zawierający podaną wartość W
% i lista Bracia jest listą prawych braci tego wierzchołka, czyli listą wartości znajdujących się w wierzchołkach drzewa
% położonych na tym samym poziomie i na prawo od wierzchołka W.

:- use_module(library(lists)).

prawiBracia(D, W, B) :- prawiBracia([D], [], W, B).

% Kolejka, lista elementów w kolejnym poziomie, szukany wierzchołek, lista braci
prawiBracia([], [E|K], W, B) :-
	reverse([E|K], K2),
	prawiBracia(K2, [], W, B).
prawiBracia([tree(_, W, _)|K], _, W, K2) :-
	dajWierzcholki(K, K2).
prawiBracia([tree(L, _, R)|K], NP, W, B) :-
	dodajNieNil(L, R, NP, NP2),
	prawiBracia(K, NP2, W, B).

dodajNieNil(nil, nil, N, N).
dodajNieNil(A, nil, N, [A|N]) :-
	A \= nil.
dodajNieNil(nil, A, N, [A|N]) :-
	A \= nil.
dodajNieNil(A, B, N, [B,A|N]) :-
	(A, B) \= (nil, nil).

dajWierzcholki([], []).
dajWierzcholki([tree(_, W, _)|R], [W|WN]) :-
	dajWierzcholki(R, WN).

% 2. Graf skierowany acykliczny reprezentujemy za pomocą klauzul postaci:
%         graf(ListaWierzchołków) oraz
%         sasiedzi(W, ListaSąsiadówW),
% gdzie ListaSąsiadówW jest zbiorem (listą bez powtórzeń) wszystkich
% wierzchołków, do których prowadzi krawędź z wierzchołka W.
% Zakładamy, że dla każdego wierzchołka grafu jest podana jedna klauzula
% predykatu sasiedzi/2.
% 
% Odległość dwóch wierzchołków definiujemy jako długość ścieżki
% (tj. liczba krawędzi) prowadzącej z jednego wierzchołka do drugiego.
% Przyjmujemy, że odległość wierzchołka od samego siebie wynosi zero.
% 
% Zdefiniuj predykat
%           odległe(W1, W2, Odległości),
% który odnosi sukces wtw, gdy Odległości jest uporządkowaną niemalejąco
% listą wszystkich odległości z wierzchołka W1 od wierzchołka W2.
% 
% Dla przykładowego grafu:
 
graf([a,b,c,d]).
sasiedzi(a, [b,c,d]).
sasiedzi(b, [d]).
sasiedzi(c, [d]).
sasiedzi(d, []).
 
% na zapytanie odległe(a, d, K) powinna zostać udzielona (dokładnie jedna)
% odpowiedź:  K = [1,2,2],
% a na zapytanie odległe(b, C, L) powinny zostać udzielone 2 odpowiedzi
% (po jednej odpowiedzi dla każdego wierzchołka grafu osiągalnego z b),
% czyli C = b, L = [0] oraz C = d, L = [1].

odlegle(A, D, K) :- odlegle2([para(A, 0)], D, K).

odlegle2([], _, []).
odlegle2([para(D, O)|Q], D, [O|K]) :- odlegle2(Q, D, K).
odlegle2([para(V, O)|Q], D, K) :-
	sasiedzi(V, S),
	O2 is O + 1,
	kolejkuj(S, O2, KK),
	append(Q, KK, Q2),
	odlegle2(Q2, D, K).

kolejkuj([], _, []).
kolejkuj([V|RV], O, [para(V, O)|K]) :- kolejkuj(RV, O, K).

% 3. Zdefiniuj predykat  zerowe( Drzewo, Lista ), który dla drzewa binarnego Drzewo, w którego
% wierzchołkach znajdują się liczby całkowite oraz zmiennej  Lista odnosi sukces uzgadniając Listę
% z listą złożoną z wartości pewnego podciągu ciągu wszystkich wartości z wierzchołków danego drzewa,
% którego suma elementów wynosi zero.
% Wartość każdego wierzchołka drzewa może wystąpić na liście co najwyżej jeden raz.
% Przyjmujemy, że suma elementów listy pustej wynosi zero.
% Predykat powinien generować wszystkie (z dokładnością do permutacji)
% listy spełniające podane warunki.. Grafy skierowane reprezentujemy jako listy krawędzi, czyli listy termów
%    postaci kr(A,B) oznaczających krawędź z A do B.
%    Reprezentacja drzew binarnych: nil - drzewo puste,
%      tree(Lewe,Wierzch,Prawe) - drzewo niepuste.

% 4.  Napisać predykat drzewo(Graf, Drzewo), który dla w pełni ustalonego
%    termu Graf, reprezentującego graf skierowany, odniesie sukces wtw, gdy
%    graf ten jest drzewem binarnym i uzgodni zmienną Drzewo z reprezentacją
%    (dowolnego) drzewa binarnego odpowiadającego grafowi Graf.

drzewo(G, D) :-
	korzen(G, K),
	buduj(G, K, D).

korzen(G, K) :-
	member(edge(K, _), G),
	\+ niekorzen(K, G).

niekorzen(K, G) :- member(edge(_, K), G).
