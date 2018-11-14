mt1(tosi).
mt1(A = B) :-
	A = B.
mt1((A, B)) :-
	mt1(A), mt1(B).
mt1(F) :-
	klausuuli(F, Vartalo),
	mt1(Vartalo).

syy(tosi, []).
syy(A = B, Todistus) :-
	A = B, Todistus = [A = B].
syy((A, B), Todistus) :-
	syy(A, TodistusA),
	syy(B, TodistusB),
	Todistus = [ja(TodistusA, TodistusB)].
syy(F, Todistus) :-
	klausuuli(F, Vartalo),
	syy(Vartalo, TodistusF),
	Todistus = [Vartalo | TodistusF].

miksi(Lause) :-
	syy(Lause, Todistus),
	näytä(Lause),
	tulosta(", koska:"),
	rivinvaihto,
	näytä_syylista(Todistus, 0).

näytä_syylista([], _).
näytä_syylista([A|L], S) :-
	näytä(A),
	rivinvaihto,
	näytä_lista(L).

lisää([A], X, [A|X]).
lisää([A|X], Y, [A|Z]) :- lisää(X, Y, Z).

peano(0).
peano(s(N)) :- peano(N).

summa(0, B, B).
summa(s(A), B, s(S)) :- summa(A, B, S).
