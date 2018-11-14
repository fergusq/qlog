pituus(L, P) :- muuttuja(P), \+ muuttuja(L), pituus_m(L, P) !; pituus_k(L, P).

pituus_m([], 0).
pituus_m([_|H], P) :- pituus_m(H, Z), P on Z+1.

pituus_k([], 0).
pituus_k([_|H], P) :- P>0, Z on P-1, pituus_k(H, Z).

jäsen(X, [X|_]).
jäsen(Y, [X|L]) :- jäsen(Y, L).

lisää([A], X, [A|X]).
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
