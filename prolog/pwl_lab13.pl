% Drzewa otwarte
% -------------
% 
% Zdefiniuj predykaty:
% 
%   a) insertBST(Elem, DrzewoOtwarte)
%       (wstawienie Elem do drzewa BST - drzewa otwartego)
%       (3 wersje procedury: Elem jest(?) w drzewie)

% Wersja z wstawianiem jednokrotnym
% insertBST(E, tree(_, E, _)).
% insertBST(E, T) :-
%	nonvar(T),
%	T = tree(L, W, P),
%	(E < W -> insertBST(E, L); insertBST(E, P)).

% Wstawianie wielokrotne
insertBST(E, T) :-
	var(T),
	!,
	T = tree(_, E, _).
insertBST(E, T) :-
	nonvar(T),
	T = tree(L, W, P),
	(E < W -> insertBST(E, L); insertBST(E, P)).

%   b) closeD(DrzewoOtwarte)
%        (zamknięcie drzewa otwartego)

closeD(nil) :- !.
closeD(tree(L, _, P)) :-
	closeD(L),
	closeD(P).

%   c) createBST(Lista, DrzewoBST-Zamknięte)

createBST([], T) :- closeD(T).
createBST([E|L], T) :-
	insertBST(E, T),
	createBST(L, T).

%   d) sortBST(Lista, Posortowana)

%sortBST(L, P) :-
%	createBST(L, T),
%	listaDfs(T, P).


% Listy różnicowe
% --------------
% 
% Zdefiniuj predykaty:
% 
%   a) flagaPolska(L, F)   z użyciem list różnicowych:
%          (a) akumulator: lista różnicowa,
%          (b) wynikowa lista to lista różnicowa.

:- op(500, xfx, --).

% Wersja z akumulatorem
flagaPolskaA(L, F) :- flagaPolskaA(L, A -- A, F).

flagaPolskaA([], P -- [], P).
flagaPolskaA([b | R], P -- K, [b | F]) :- flagaPolskaA(R, P -- K, F).
flagaPolskaA([c | R], P -- [c | K], F) :- flagaPolskaA(R, P -- K, F).

% Wynik to lista różnicowa
flagaPolska([], L -- L).
flagaPolska([b | R], [b | P] -- K) :- flagaPolska(R, P -- K).
flagaPolska([c | R], P -- K) :- flagaPolska(R, P -- [c | K]).

flagaPolskaClosed(L, W) :- flagaPolska(L, W -- []).

%   b) wszerz(DrzewoBinarne, ListaWierzchWszerz)
%      (kolejka reprezentowana jako lista różnicowa)

wszerz(D, LW) :- wszerz_p([D | Q] -- Q, LW).

wszerz_p(Q -- _, L) :- var(Q), !, L = [].
wszerz_p([nil | P] -- K, LW) :- wszerz_p(P -- K, LW).
wszerz_p([tree(L, E, P) | QP] -- [L, P | QK], [E|LW]) :- wszerz_p(QP -- QK, LW).

%   c) flagaHolenderska(ListaRWB, FlagaRWB) (RedWhiteBlue)
%      (bez append; wersje: 1, 2 lub 3 pomocnicze listy różnicowe)

flagaHolenderska1(ListaRWB, FlagaRWB) :- fh(ListaRWB, FlagaRWB -- WB, WB -- B, B -- []).
flagaHolenderska2(L, F) :- fh2(L, X -- X, F).

fh([], R -- R, W -- W, B -- B).
fh([r | L], [r | R] -- KR, W, B) :- fh(L, R -- KR, W, B).
fh([w | L], R, [w | W] -- KW, B) :- fh(L, R, W -- KW, B).
fh([b | L], R, W, [b | B] -- KB) :- fh(L, R, W, B -- KB).

fh2([], F -- [], F).
fh2([r | L], R -- RK, [r | F]) :- fh2(L, R -- RK, F).
fh2([w | L], R -- RK, F) :- fh2(L, [w | R] -- RK, F).
fh2([b | L], R -- [b | RK], F) :- fh2(L, R -- RK , F).

