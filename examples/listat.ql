pituus(L, P) :- muuttuja(P), \+ muuttuja(L), pituus_m(L, P) !; pituus_k(L, P).

pituus_m([], 0).
pituus_m([_|H], P) :- pituus_m(H, Z), P on Z+1.

pituus_k([], 0).
pituus_k([_|H], P) :- P>0, Z on P-1, pituus_k(H, Z).

ns0(N, L, X) :- nsK(0, N, L, X).
ns1(N, L, X) :- nsK(1, N, L, X).

nsK(K, N, L, X) :- muuttuja(N), \+ muuttuja(L), nsK_m(K, N, L, X) !; nsK_k(K, N, L, X).

nsK_m(K, K, [X|_], X).
nsK_m(K, N, [_|H], X) :- nsK_m(K, M, H, X), N on M+1.

nsK_k(K, K, [X|_], X).
nsK_k(K, N, [_|H], X) :- N>K, M on N-1, nsK_k(K, M, H, X).

jäsen(X, [X|_]).
jäsen(Y, [X|L]) :- jäsen(Y, L).

sisältää(L, X) :- jäsen(X, L).

lisää([], X, X).
lisää([A|X], Y, [A|Z]) :- lisää(X, Y, Z).

alijono([], []).
alijono([A|X], [A|Y]) :- alijono(X, Y).
alijono(X, [_|Y]) :- alijono(X, Y).

takaperin(A, B) :- muuttuja(B), takaperin_(A, B, []) !; takaperin_(B, A, []).

takaperin_([]) --> [].
takaperin_([A|At]) --> takaperin_(At), [A].

% alijonot järjestettynä suurimmasta pienimpään

alijono2(X, Y) :-
	muuttuja(X),
	pituus(Y, PituusY),
	välillä(0, PituusY, I),
	PituusX on PituusY - I,
	pituus(X, PituusX),
	alijono(X, Y).
alijono2(X, Y) :-
	\+ muuttuja(X),
	pituus(X, PituusX),
	PituusY >= PituusX,
	pituus(Y, PituusY),
	alijono(X, Y).

järjestyksessä([]).
järjestyksessä([_]).
järjestyksessä([A,B|X]) :- A<=B, järjestyksessä([B|X]).

lomitusjärjestys([], []).
lomitusjärjestys([A], [A]).
lomitusjärjestys([A,B|L], Valmis) :-
	jaa([A,B|L], L1, L2),
	lomitusjärjestys(L1, V1),
	lomitusjärjestys(L2, V2),
	lomita(V1, V2, Valmis).

lomita(X, [], X).
lomita([], Y, Y).
lomita([A|X], [B|Y], [A|Z]) :- A<=B, lomita(X, [B|Y], Z).
lomita([A|X], [B|Y], [B|Z]) :- A>B, lomita([A|X], Y, Z).

jaa([], [], []).
jaa([A], [A], []).
jaa([A,B|L], [A|X], [B|Y]) :- jaa(L, X, Y).

kaikki_välillä(_, _, []).
kaikki_välillä(A, B, [I|It]) :- välillä(A, B, I), kaikki_välillä(A, B, It).

kuvaa(_, []).
kuvaa(P, [A|At]) :-
	kutsu(P, [A]),
	kuvaa(P, At).

kuvaa(_, [], []).
kuvaa(P, [A|At], [B|Bt]) :-
	kutsu(P, [A, B]),
	kuvaa(P, At, Bt).

kutsu(P, At) :-
	P =.. Tt,
	lisää(Tt, At, Tt2),
	P2 =.. Tt2,
	P2.
