:- [listat].

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

% Konjunktioita leveyshakeva metatulkki

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
mt2_(A > B) --> { A > B }.
mt2_(A < B) --> { A < B }.
mt2_(A >= B) --> { A >= B }.
mt2_(A <= B) --> { A <= B }.
mt2_(A on B) --> { A on B }.
mt2_((A, B)) --> [A, B].
mt2_((A; B)) --> [A].
mt2_((A; B)) --> [B].
mt2_(F) -->
	{ klausuuli(F, Vartalo) },
	[Vartalo].

% Metatulkki, joka näyttää suorituspolun

näytä_polku(tosi, S).
näytä_polku(A = B, S) :-
	A = B,
	näytä_sisennetty(A = B, S).
näytä_polku(A on B, S) :-
	A on B,
	näytä_sisennetty(A on B, S).
näytä_polku(A > B, S) :-
	A > B,
	näytä_sisennetty(A > B, S).
näytä_polku(A < B, S) :-
	A < B,
	näytä_sisennetty(A < B, S).
näytä_polku(A >= B, S) :-
	A >= B,
	näytä_sisennetty(A >= B, S).
näytä_polku(A <= B, S) :-
	A <= B,
	näytä_sisennetty(A <= B, S).
näytä_polku(välillä(A, B, C), S) :-
	välillä(A, B, C),
	näytä_sisennetty(välillä(A, B, C), S).
näytä_polku(A =.. B, S) :-
	A =.. B,
	näytä_sisennetty(A =.. B, S).
näytä_polku(klausuuli(A, B), S) :-
	klausuuli(A, B),
	näytä_sisennetty(klausuuli(A, B), S).
näytä_polku((A, B), S) :-
	näytä_polku(A, S),
	näytä_polku(B, S).
näytä_polku(F, S) :-
	klausuuli(F, Vartalo),
	S2 on S+1,
	näytä_polku(Vartalo, S2),
	näytä_sisennetty(F, S).

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
syy(A =.. B, Todistus) :-
	A =.. B, Todistus = [A =.. B].
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
	näytä_sisennetty(A, S),
	T on S+1,
	näytä_syylista(L, T).

toista(I, P) :-
	I >= 0,
	\+((
		välillä(1, I, _),
		P,
		epätosi
	)).

näytä_sisennetty(F, S) :-
	sisennys(S),
	näytä(F),
	rivinvaihto.

% Predikaatteja, joilla testata metatulkkeja

peano(0).
peano(s(N)) :- peano(N).

summa(0, B, B).
summa(s(A), B, s(S)) :- summa(A, B, S).

huono(X) :- huono(X), epätosi, huono(X).

% HELPO-kielen esimerkki Mikro-lehdestä 2/1985

lihava(X) :-
	painaa(X, Y),
	Y > 90.

painaa(ari, 95).
painaa(eppu, 65).
painaa(virpi, 60).
painaa(urpo, 55).

normaali(X) :-
	painaa(X, Y),
	Y <= 90,
	Y >= 60.

laiha(X) :-
	painaa(X, Y),
	Y < 60.

normaali(X) :- terve(X).
normaali(X) :- työssäkäyvä(X).
normaali(X) :- työtön(X).

terve(urpo).
työssäkäyvä(_) :- epätosi.
työtön(_) :- epätosi.

viisas(eppu) :- kaunis(virpi).
tyhmä(urpo) :- \+ viisas(eppu).
kaunis(virpi).
