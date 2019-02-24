% nat(X) :- X jest liczbą naturalną
% s(X) - następnik liczby X
nat(0).
nat(s(X)) :- nat(X).

% plus(X, Y, X + Y).
plus(0, X, X). % :- nat(X). - nie sprawdzamy tego warunku, to nieefektywne
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).

% minus(X, Y, X - Y).
minus(X, 0, X).
minus(s(X), s(Y), Z) :- minus(X, Y, Z).

% fib(N, Liczba).
fib(0, 0).
fib(s(0), s(0)).
fib(s(s(N)), Liczba) :-
	fib(s(N), L1),
	fib(N, L2),
	plus(L1, L2, Liczba).

% le(X, Y) wtw. X <= Y
le(0, _).
le(s(X), s(Y)) :- le(X, Y).

% lt(X, Y) wtw. X < Y
lt(0, s(_)).
lt(s(X), s(Y)) :- lt(X, Y).

% razy(X, Y, X * Y)
razy(0, _, 0).
razy(s(0), X, X).
razy(s(s(X)), Y, Wynik) :-
	razy(s(X), Y, Tmp),
	plus(Tmp, Y, Wynik).

% exp(N, X, Wynik) - Wynik = X^N
exp(0, _, s(0)).
exp(s(N), X, Wynik) :-
	exp(N, X, W1),
	razy(W1, X, Wynik).

% silnia(N, S) == S = N!
silnia(0, s(0)).
silnia(s(N), S) :-
	silnia(N, S1),
	razy(s(N), S1, S).

% odd(N)  == N jest liczbą nieparzystą
odd(s(0)).
odd(s(s(N))) :- odd(N).

% even(N) == N jest liczbą parzystą
even(0).
even(s(N)) :- odd(N).

% mod(x, y, z) == x modulo y = z
mod(X, Y, X) :-
	lt(X, Y).
mod(X, Y, Z) :-
	le(Y, X),
	minus(X, Y, T),
	mod(T, Y, Z).

% nwd(x, y, z) == z = największy wspólny dzielnik x i y
nwd(0, X, X).
nwd(s(X), Y, Z) :-
	mod(s(X), Y, T),
	nwd(Y, T, Z).
