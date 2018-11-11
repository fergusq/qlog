mt1(tosi).
mt1(A = B) :-
	A = B.
mt1((A, B)) :-
	mt1(A), mt1(B).
mt1(F) :-
	klasuuli(F, Vartalo),
	mt1(Vartalo).

syy(tosi, []).
syy(A = B, Todistus) :-
	A = B, Todistus = [A = B].
syy((A, B), Todistus) :-
	syy(A, TodistusA),
	syy(B, TodistusB),
	lisää(TodistusA, TodistusB, Todistus).
syy(F, Todistus) :-
	klasuuli(F, Vartalo),
	syy(Vartalo, TodistusF),
	Todistus = [(F :- Vartalo) | TodistusF].

miksi(Lause) :-
	syy(Lause, Todistus),
	näytä_lista(Todistus).

näytä_lista([]).
näytä_lista([A|L]) :-
	näytä(A),
	näytä_lista(L).

lisää([A], X, [A|X]).
lisää([A|X], Y, [A|Z]) :- lisää(X, Y, Z).

peano(0).
peano(s(N)) :- peano(N).

summa(0, B, B).
summa(s(A), B, s(S)) :- summa(A, B, S).
