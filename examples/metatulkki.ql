(A \= B) :- \+ (A = B).

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
syy(A on B, Todistus) :-
	A on B, Todistus = [A on B].
syy(A > B, Todistus) :-
	A > B, Todistus = [A > B].
syy(A < B, Todistus) :-
	A < B, Todistus = [A < B].
syy(A >= B, Todistus) :-
	A >= B, Todistus = [A >= B].
syy(A <= B, Todistus) :-
	A <= B, Todistus = [A <= B].
syy((A, B), Todistus) :-
	syy(A, TodistusA),
	syy(B, TodistusB),
	Todistus = [ja(TodistusA, TodistusB)].
syy(F, Todistus) :-
	klausuuli(F, Vartalo),
	syy(Vartalo, TodistusF),
	Todistus = [F | TodistusF].

miksi(Lause) :-
	syy(Lause, Todistus),
	näytä_syylista(Todistus, 0).

näytä_syylista([], _).
näytä_syylista([ja(A, B)], S) :-
	näytä_syylista(A, S),
	näytä_syylista(B, S).
näytä_syylista([A|L], S) :-
	A \= ja(_, _),
	toista(S, tulosta(" ")),
	näytä(A),
	rivinvaihto,
	T on S+1,
	näytä_syylista(L, T).

toista(I, P) :-
	I >= 1,
	\+((
		välillä(1, I, _),
		P,
		epätosi
	)).

peano(0).
peano(s(N)) :- peano(N).

summa(0, B, B).
summa(s(A), B, s(S)) :- summa(A, B, S).
