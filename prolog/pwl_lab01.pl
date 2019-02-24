dziecko(janusz, i1, i2).
dziecko(jasio, ewa, janusz).
dziecko(mirek, jasio, paulina).

dziecko(jasio, ewa, janusz, m).
dziecko(mirek, jasio, paulina, m).
dziecko(janusz, i1, i2, m).

% syn 
syn(X, Y) :- dziecko(X, Y, _).

% matka(Dziecko, Matka)
matka(D, M) :- dziecko(D, M, _).

% ojciec()
ojciec(D, T) :- dziecko(D, _, T).

rodzenstwo(R1, R2) :-
    dziecko(R1, M, T),
    dziecko(R2, M, T),
    R1 \= R2.

% rodzic(Dziecko, Rodzic)
rodzic(D, R) :-
    matka(D, R).
rodzic(D, R) :-
    ojciec(D, R).

% partner(Pierwszy, Drugi)
partner(X, Y) :-
    dziecko(_, X, Y).
partner(X, Y) :-
    dziecko(_, Y, X).

% babcia(Babcia, Wnuk).
babcia(B, W) :-
    rodzic(W, X),
    matka(X, B).

% wnuk(DziadekBabcia, Wnuk).
wnuk(D, W) :-
    rodzic(W, X),
    rodzic(X, D).

% przodek (Górny, Dolny)
przodek(G, D) :-
    rodzic(D, G).
przodek(G, D) :-
    rodzic(X, G),
    przodek(X, D).

% meski(Osoba)
meski(X) :- dziecko(X, _, _, m).

% zenski(Osoba)
zenski(X) :- dziecko(X, _, _, d).

% corka(Córka, Rodzic)
corka(C, R) :- (C, R, _, d).
corka(C, R) :- (C, _, R, d).
