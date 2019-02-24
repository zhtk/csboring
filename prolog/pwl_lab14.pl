% II. Struktury różnicowe
% -------------------
% 
% Zdefiniuj predykat:       normalize(Wyrażenie, PostaćNormalna)
% Wyrażenie: stała, wyrażenie + wyrażenie
% Postać normalna: nawiasowanie prawe
%     Np. ?- normalize((a+b)+(c+d), N).
%            N = a+(b+(c+d))

:- op(500, xfx, ++).

normalize(W, N) :- norma(W, N ++ 0).

norma(A, A ++ 0) :- atomic(A).
norma(A, (A + K) ++ K) :- atomic(A), var(K).
norma(A + B, PA ++ KB) :-
	norma(A, PA ++ PB),
	norma(B, PB ++ KB).

% 2. Rozważamy drzewa binarne, w których wierzchołkach są przechowywane listy różnicowe.
%     Reprezentacja drzew: nil, tree(L, w(W, ListaR), P).
%     Zdefiniować predykat  scal(Drzewo, W, L) zachodzący wtw, gdy L jest konkatenacją
%     list z wierzchołków Drzewa położonych na ścieżce od korzenia do W.

:- op(500, xfx, --).

%scal(D, W, L) :- scalpom(D, W, A -- A, L).

%scal(tree(_, w(W, AK -- []), _), W, AP -- AK, AP).
%scal(tree(L, w(_, LP -- AK), _), W, AP -- LP, AP) :-
%	scal(L, W, AP -- AK , AP).
%scal(tree(_, w(_, LP -- AK), P), W, AP -- LP, AP) :-
%	scal(P, W, AP -- AK , AP).

% Rozważamy drzewa binarne, w których wierzchołkach są przechowywane listy różnicowe.
% Reprezentacja drzew: nil, tree(L,W,P).
% Zdefiniować predykat:  poziomy(D, LP), który dla podanego drzewa binarnego D odniesie sukces wtw, gdy
% LP można uzgodnić z listą list [L1, ..., Lk] taką, że:
%   - lista Li jest konkatenacją wszystkich list różnicowych znajdujących się
%       w wierzchołkach D na i-tym poziomie, w kolejności od lewej do prawej,
%   - k jest numerem najgłębszego poziomu drzewa.
% Przyjmujemy, że poziom korzenia jest równy 1.
% 
% Przykladowe odpowiedzi obliczone:
%     | ?- poziomy(nil, W).
%     W = []
% 
%     | ?- poziomy(tree(nil,[2|Y]--Y,nil),W).
%     W = [[2]]
% 
%     | ?- poziomy(tree(tree(nil,[2|Y]--Y,nil),
% 	                    [1|X]--X,
%                  tree(tree(nil,[4|W]--W,nil), [3|Z]--Z, nil)), WW).
%     WW = [[1],[2,3],[4]]
% 
% Ewentualnie dozwolone są odpowiedzi, w których ostatnim elementem listy
% będzie lista pusta.

poziomy(D, W) :- poz([D, end | Q] -- Q, P -- P, W).

poz([end | Q] -- Q, _, []) :- var(Q), !.
poz([end | QP] -- [end | QK], P -- [], [P | W]) :- poz(QP -- QK, A -- A, W).
poz([nil | QP] -- QK, P, W) :- poz(QP -- QK, P, W).
poz([tree(L, WP -- WK, P) | QP] -- [L, P | QK], PP -- WP, W) :-	poz(QP -- QK, PP -- WK, W).


