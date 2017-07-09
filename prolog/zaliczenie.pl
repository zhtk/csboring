% Piotr Zalas 361374
%
% Program kompilowany na SICStus 4.3.5
% Oceniony na 3 punkty z 8.
% 
% Automat jest reprezentowany przy pomocy predykatu
% automat(Elements, A, Start)
% gdzie:
% Elements to zbiór terminali i nieterminali
% Start jest numerem stanu początkowego
% A jest połączeniem tablicy goto i action w taki sposób, że:
%   kolumny tablicy odpowiadają symbolom z Elements
%   wiersze odpowiadają stanom automatu, są numerowane od 1
%   w komórkach znajdują się predykaty: null, accept, shift(N),
%     reduce(Nieterminal, elementy stosu), goto(N)

:- use_module(library(lists)).

% Dopisuje do gramatyki stan początkowy
fixGrammar(gramatyka(Sp, Zp),
  gramatyka('Z', [prod('Z', [[nt(Sp), '#']]) | Zp])).

% Wyszukuje wszystkie produkcje danego nieterminala
findProductions([], _, []) :-
  !.
findProductions([prod(Nieterminal, Wynik) | _], nt(Nieterminal), Wynik) :-
  !.
findProductions([_ | Zp], nt(Nieterminal), Wynik) :-
  !,
  findProductions(Zp, nt(Nieterminal), Wynik).
findProductions(_, _, []).

% Dodaje element do zbioru na liście
addToSet(Wynik, Element, Wynik) :-
  memberchk(Element, Wynik),
  !.
addToSet(Lista, Element, [Element | Lista]).

% Dodaje elementy do zbioru Result z wyjątkiem tych,
% które są na liście Except
addExclusive([], _, []).
addExclusive([E | Elems], Except, Result) :-
  member(E, Except),
  !,
  addExclusive(Elems, Except, Result).
addExclusive([E | Elems], Except, Result) :-
  addExclusive(Elems, Except, Result1),
  addToSet(Result1, E, Result).

% Zamienia produkcje na listę początkowych sytuacji
productionsToSituations([], _, []).
productionsToSituations([E | L], Nt, [syt(Nt, [], E) | A]) :-
  productionsToSituations(L, Nt, A).

% Tworzy domknięcie danej sytuacji w danej gramatyce
% Format sytuacji:
%  * Nieterminal
%  * Przeczytane symbole
%  * Pozostałe symbole
% np. syt('Z', [], ['E', '#'])
closure(Sytuacje, gramatyka(_, Zp), Wynik) :-
  closure(Sytuacje, Zp, [], Wynik).

closure([], _, A, A).
closure([syt(Nt, Prz, [nt(NowyZnak) | Poz]) | St], Zp, A, Wynik) :-
  !,
  findProductions(Zp, nt(NowyZnak), Produkcje),
  productionsToSituations(Produkcje, NowyZnak, NoweSytuacje),
  addExclusive(NoweSytuacje, [syt(Nt, Prz, [nt(NowyZnak) | Poz]) | A], St1),
  append(St, St1, St2),
  remove_dups(St2, St3),
  closure(St3, Zp, [syt(Nt, Prz, [nt(NowyZnak) | Poz]) | A], Wynik).
closure([S | St], Zp, A, Wynik) :-
  closure(St, Zp, [S | A], Wynik).

% Daje symbol początkowy gramatyki
startingSymbol(gramatyka(Sym, _), Sym).

% Zbiera listę wszystkich terminale z Gramatyki
gatherSymbols([], A, Elements) :-
  remove_dups(A, Elements).
gatherSymbols([prod(_, E) | L], A, Elements) :-
  append(E, Syms),
  append(Syms, A, A1),
  gatherSymbols(L, A1, Elements).

gatherSymbols(gramatyka(_, L), Elements) :-
  gatherSymbols(L, [], Elements).

% Predykat sprawdzający, czy w podanym stanie możliwe jest przejście po T
isTransition(syt(_, _, [T | _]), T).

% Wykonaj przejście
doTransition(syt(Z, A, [T | B]), syt(Z, A1, B)) :-
  append(A, [T], A1).

% Filtruje możliwe do wykonania przejścia z sytuacji i je wykonuje
filterTransition([], _, []).
filterTransition([S | St], E, [S1 | R]) :-
  isTransition(S, E),
  !,
  doTransition(S, S1),
  filterTransition(St, E, R).
filterTransition([_ | St], E, R) :-
  filterTransition(St, E, R).

% Generuje wszystkie możliwe stany po przejściu automatu ze stanu S w
% gramatyce Gr po kolei po symbolach Sym
generateTransitions(Gr, S, Sym, Tr) :-
  generateTransitions(Gr, S, Sym, [], Tr).

generateTransitions(_, _, [], A, A).
generateTransitions(Gr, S, [E | Sym], A, Tr) :-
  filterTransition(S, E, Res),
  closure(Res, Gr, Res1),
  generateTransitions(Gr, S, Sym, [Res1 | A], Tr).

% Zbiera listę wszystkich osiągalnych sytuacji w Gramatyce
% Argumenty: +Gramatyka, +Sytuacja startowa, +Symbole, -Lista sytuacji
gatherSituations(Gr, Start, Syms, Wynik) :-
  gatherSituations(Gr, [Start], Syms, [], Wynik).

gatherSituations(_, [], _, Wynik, Wynik).
gatherSituations(Gr, [S | R], Syms, A, Wynik) :-
  generateTransitions(Gr, S, Syms, Tr),
  addExclusive(Tr, [S | A], R1),
  append(R, R1, R2),
  remove_dups(R2, R3),
  gatherSituations(Gr, R3, Syms, [S | A], Wynik).

% Kontruuje wiersz tymczasowej tabeli z sytuacjami
createTableRow(_, [], _, _, []).
createTableRow(Gr, [E | Elem], Syt, S, [R | Row]) :-
  filterTransition(S, E, S1),
  closure(S1, Gr, S2),
  nth1(R, Syt, S2),
  createTableRow(Gr, Elem, Syt, S, Row).

% Tworzy wstępną tabelę z budową automatu.
% Argumenty:
% +Gramatyka
% +Lista symboli
% +Lista sytuacji
% +Kolejka sytuacji
% -Tabela w formie listy list:
%   wiersz - sytuacja
%   kolumna - symbol na wejściu
createTable(_, _, _, [], []).
createTable(Gr, Elements, Sytuacje, [S | Sit], [Row | Table]) :-
  createTableRow(Gr, Elements, Sytuacje, S, Row),
  createTable(Gr, Elements, Sytuacje, Sit, Table).

% Opakowuje elementy wiersza w operację shift lub goto
rowShift([], [], []).
rowShift([null | T], [_ | Sym], [null | W]) :-
  !,
  rowShift(T, Sym, W).
rowShift([E | T], [nt(_) | Sym], [goto(E) | W]) :-
  !,
  rowShift(T, Sym, W).
rowShift([E | T], [_ | Sym], [shift(E) | W]) :-
  rowShift(T, Sym, W).

% Usuwa z wiersza odwołania do śmietnika
rowDeleteTrash([], _, []).
rowDeleteTrash([R | Rs], Syt, [W | Ws]) :-
  nth1(N, Syt, []),
  replace(N, R, null, W),
  rowDeleteTrash(Rs, Syt, Ws) .

replace(_, [], _, []).
replace(X, [X | Xs], Y, [Y | Ys]) :-
  !,
  replace(X, Xs, Y, Ys).
replace(X, [Z | Xs], Y, [Z | Ys]) :-
  replace(X, Xs, Y, Ys).

% Dodaje akcję reduce do wiersza o ile jest to potrzebne
rowReduce2([], [], _, yes, []) :-
  !.
rowReduce2([shift(_) | _], _, _, konflikt('Konflikt shift-reduce'), null) :-
  !.
rowReduce2([E | R], [nt(_) | S], P, Info, [E | W]) :-
  !,
  rowReduce2(R, S, P, Info, W).
rowReduce2([null | R], [ _ | Sym], P, Info, [P | W]) :- 
  !,
  rowReduce2(R, Sym, P, Info, W).
rowReduce2([E | R], Sym, P, Info, [E | W]) :-
  !,
  rowReduce2(R, Sym, P, Info, W).

rowReduce(R, Sym, Syt, Info, W) :-
  notReduceConflict(Syt),
  getReduceProd(Syt, P),
  !,
  rowReduce2(R, Sym, P, Info, W).
rowReduce(R, _, Syt, yes, R) :-
  notReduceConflict(Syt),
  !. 
rowReduce(_, _, _, konflikt('Konflikt reduce-reduce'), null).

getReduceProd([syt(Nt, P, []) | _], reduce(Nt, P)).
getReduceProd([_ | S], W) :-
  getReduceProd(S, W).

notReduceConflict([syt(Nt, P, []) | S], null) :-
  notReduceConflict(S, r(Nt, P)).
notReduceConflict([syt(Nt, P, []) | S], r(Nt, P)) :-
  notReduceConflict(S, r(Nt, P)).
notReduceConflict([syt(_, _, [_ | _]) | S], P) :-
  notReduceConflict(S, P).
notReduceConflict([], _).
notReduceConflict(L) :-
  notReduceConflict(L, null).

% Uzupełnia wiersz o akcję accept
rowSetAccept(R, Sym, Syt, W) :-
  some(isAccepting, Syt),
  !,
  rowSetAccept2(R, Sym, W).
rowSetAccept(W, _, _, W).

isAccepting(syt(_, _, ['#'])).

rowSetAccept2([], _, []).
rowSetAccept2([_ | Rs], ['#' | S], [accept | Ws]) :-
  !,
  rowSetAccept2(Rs, S, Ws).
rowSetAccept2([R | Rs], [_ | S], [R | Ws]) :-
  rowSetAccept2(Rs, S, Ws).

% Uzupełnia tabelę o operacje shift-reduce
% Argumenty:
% +Tabela
% +Lista symboli
% +Lista sytuacji
% -Wynik zmiennej Info
% -Wynikowa tabela
fillTable([Table | _], Sym, [S | _], konflikt(T), null) :-
  rowShift(Table, Sym, W1),
  rowSetAccept(W1, Sym, S, W2),
  rowReduce(W2, Sym, S, Info, _),
  Info = konflikt(T).
fillTable([Table | Ts], Sym, [S | Syt], konflikt(T), null) :-
  rowShift(Table, Sym, S, W1),
  rowSetAccept(W1, Sym, W2),
  rowReduce(W2, Sym, S, I1, _),
  I1 == yes,
  fillTable(Ts, Sym, Syt, konflikt(T), null).
fillTable([], _, _, yes, []).
fillTable([T | Tabela], Sym, [S | Syt], yes, [W | Wynik]) :-
  rowShift(T, Sym, W1),
  rowSetAccept(W1, Sym, S, W2),
  rowReduce(W2, Sym, S, I1, W),
  I1 == yes,
  fillTable(Tabela, Sym, Syt, yes, Wynik).

% Tworzy automat LR(0)
createLR(Gramatyka, Automat, Info) :-
  fixGrammar(Gramatyka, G1),
  startingSymbol(Gramatyka, Start),
  closure([syt('Z', [], [nt(Start), '#'])], G1, Dop),
  gatherSymbols(G1, Elements),
  gatherSituations(G1, Dop, Elements, Situations),
  createTable(G1, Elements, Situations, Situations, Table),
  rowDeleteTrash(Table, Situations, T1),
  fillTable(T1, Elements, Situations, Info, A1),
  length(A1, Len),
  Automat = automat(Elements, A1, Len).

% Sprawdza, czy słowo można zaakceptować
accept(automat(Es, A, St), Slowo) :-
  append(Slowo, ['#'], S),
  accept(Es, A, S, [St, 'Z']).

accept(Es, A, [Z | _], [S | _]) :- % Akceptacja
  nth1(N, Es, Z),       % Zlokalizowanie kolumny z symbolem
  nth1(S, A, R),        % Zlokalizowanie wiersza
  nth1(N, R, accept).   % Sprawdzenie akceptacji
accept(Es, A, [Z | Slowo], [S | Stos]) :- % Shift
  nth1(N, Es, Z),
  nth1(S, A, R),
  nth1(N, R, shift(Nowy)),
  accept(Es, A, Slowo, [Nowy, Z, S | Stos]).
accept(Es, A, [Z | Slowo], [S | Stos]) :- % Reduce
  nth1(N, Es, Z),
  nth1(S, A, R),
  nth1(N, R, reduce(Nt, L)),
  performReduce([S | Stos], L, [Q | NowyStos]),
  nth1(Kolumna, Es, nt(Nt)),
  nth1(Q, A, R2),
  nth1(Kolumna, R2, goto(G)),
  accept(Es, A, [Z | Slowo], [G, Nt, Q | NowyStos]).

performReduce([_, _ | Stos], [_ | L], Wynik) :-
  performReduce(Stos, L, Wynik).
performReduce(Stos, [], Stos).

% test(+NazwaGramatyki, +ListaSlowDoZbadania)
test(NG, ListaSlow) :-
  grammar(NG, G),
  createLR(G, Automat, yes),
  checkWords(ListaSlow, Automat).
  checkWords([], _) :-  write('Koniec testu.\n').

checkWords([S|RS], Automat) :-
  format("  Slowo: ~p ", [S]),
  (accept(Automat, S) -> true; write('NIE ')),
  write('nalezy.\n'),
  checkWords(RS, Automat).

grammar(wiki, gramatyka('E',
  [prod('E', [[nt('E'), '*', nt('B')], [nt('E'), '+', nt('B')],[nt('B')]]),
  prod('B', [['1'], ['0']]) ])).
grammar(ex1, gramatyka('E',
  [prod('E', [[nt('E'), '+', nt('T')], [nt('T')]]),
  prod('T', [[id], ['(', nt('E'), ')']]) ])).
grammar(ex2, gramatyka('A', [prod('A', [[nt('A'), x], [x]])])).
grammar(ex3, gramatyka('A', [prod('A', [[x, nt('A')], [x]])])).
grammar(ex4, gramatyka('A',
  [prod('A', [[x, nt('B')], [nt('B'), y], []]),
  prod('B', [[]])])).
grammar(ex5, gramatyka('S',
  [prod('S', [[id], [nt('V'), ':=', nt('E')]]),
  prod('V', [[id], [id, '[', nt('E'), ']']]),
  prod('E', [[v]])])).
grammar(ex6, gramatyka('A',
  [prod('A', [[x], [nt('B'), nt('B')]]),
  prod('B', [[x]])])).
