dlugosc([], 0).
dlugosc([_|L], s(K)) :- dlugosc(L, K).

dlugosc2(L, K) :- dlugosc2(L, 0, K).
dlugosc2([], K, K).
dlugosc2([_|L], A1, K) :- A2 is A1 + 1, dlugosc2(L, A2, K).

% 2. Zdefiniuj predykat suma(L, S) odnoszący sukces wtw, gdy S jest sumą
%    wszystkich elementów (liczb) ustalonej listy liczb L.

suma(L, S) :- suma(L, 0, S).
suma([], S, S).
suma([E|L], A1, S) :- A2 is A1 + E, suma(L, A2, S).

% 3. Zdefiniuj predykat fib(K, N) odnoszący sukces wtw, gdy N jest K-tą
%    liczbą Fibonacciego.
%
%    Predykat ma być: sprawny i maksymalnie wielofunkcyjny
%    (w szczególności powinien ponosić skończoną porażkę dla zapytań:
%    którą liczbą Fibonacciego jest liczba niebędąca liczbą Fibonacciego).
%
%    Pytanie.
%    Czy program ma własność stopu?
%    (Czy/dla jakich zapytań program pętli się?)

fib(K, N) :- fib(0, 1, 0, K, N).
fib(_, N, K, K, N).
fib(F1, F2, AK, K, N) :-
	(integer(K) -> K > AK; (integer(N) -> N =< F2; true)),
	AK2 is AK + 1,
	F3 is F1 + F2,
	fib(F2, F3, AK2, K, N).

% 4. Zdefiniuj predykat poziom(ListaList, Poziom, ListaPoziomu), który
%    dla podanej listy list, których elementami są liczby całkowite oraz
%    dla podanej liczby Poziom (>=1) odnosi sukces wtw, gdy ListaPoziomu
%    jest listą wszystkich liczb znajdujących sie na podanym poziomie
%    listy list.
%
%    Przyjmujemy, że poziom o numerze jeden tworzą elementy listy.
%
%    Na przykład na zapytanie
%    poziom([1,[2,[3,[4]],5],[6,[7],8]], 2, L)
%    powinna zostać udzielona dokładnie jedna odpowiedź L = [2,5,6,8].
%
%    ?- poziom([1,[2,[3,[4]],5],[6,[7],8]], 1, L).
%    L = [1]
%
%    ?- poziom([1,[2,[3,[4]],5],[6,[7],8]], 3, L).
%    L = [3,7]
%
%    ?- poziom([1,[2,[3,[4]],5],[6,[7],8]], 4, L).
%    L = [4]
%
%    ?- poziom([1,[2,[3,[4]],5],[6,[7],8]], 5, L).
%    L = []

poziom(ListaList, Poziom, ListaPoziomu) :- poziom(ListaList, Poziom, [], ListaPoziomu).
poziom([], _, A, A).
poziom([_|_], 0, A, A).
poziom(E, 0, A, [E|A]) :-
	integer(E).
poziom(E, P, A, A) :-
	integer(E),
	P >= 1.
poziom([E|LL], P, A, LP) :-
	P >= 1,
	PM is P - 1,
	poziom(LL, P, A, LP1),
	poziom(E, PM, LP1, LP).


