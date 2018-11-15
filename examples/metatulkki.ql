(A \= B) :- \+ (A = B).

% Tavanomainen metatulkki

mt1(tosi).
mt1(A = B) :-
	A = B.
mt1((A, B)) :-
	mt1(A), mt1(B).
mt1(F) :-
	klausuuli(F, Vartalo),
	mt1(Vartalo).

% Leveyshakeva metatulkki

mt2(F) :- mt2_iteroi([F]).

mt2_iteroi([]).
mt2_iteroi([E|Et]) :-
	mt2_kaikki_totta([E|Et], Et2, []),
	mt2_iteroi(Et2).

mt2_kaikki_totta([]) --> [].
mt2_kaikki_totta([E|Et]) -->
	mt2_(E),
	mt2_kaikki_totta(Et).

mt2_(tosi) --> [].
mt2_(A = B) --> { A = B }.
mt2_((A, B)) --> [A, B].
mt2_((A; B)) --> [A].
mt2_((A; B)) --> [B].
mt2_(F) -->
	{ klausuuli(F, Vartalo) },
	[Vartalo].

% Metatulkki, joka tallentaa suorituspolun

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
syy(klausuuli(A, B), Todistus) :-
	klausuuli(A, B), Todistus = [klausuuli(A, B)].
syy((A, B), Todistus) :-
	syy(A, TodistusA),
	syy(B, TodistusB),
	Todistus = [ja(TodistusA, TodistusB)].
syy(F, Todistus) :-
	klausuuli(F, Vartalo),
	syy(Vartalo, TodistusF),
	Todistus = [F | TodistusF].

% Komento, jolla voi tulostaa suorituspolun

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
	I >= 0,
	\+((
		välillä(1, I, _),
		P,
		epätosi
	)).

% Predikaatteja, joilla testata metatulkkeja

peano(0).
peano(s(N)) :- peano(N).

summa(0, B, B).
summa(s(A), B, s(S)) :- summa(A, B, S).

huono(X) :- huono(X), epätosi, huono(X).
