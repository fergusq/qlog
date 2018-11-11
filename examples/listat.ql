peano(nolla, 0).
peano(s(P), X) :- peano(P, Y), X on Y+1.

pituus(L, P) :- muuttuja(P), \+ muuttuja(L), pituus_m(L, P); (\+ muuttuja(P); muuttuja(L)), pituus_k(L, P).

pituus_m([], 0).
pituus_m([_|H], P) :- pituus_m(H, Z), P on Z+1.

pituus_k([], 0).
pituus_k([_|H], P) :- P>0, Z on P-1, pituus_k(H, Z).

jäsen([X|_], X).
jäsen([X|L], Y) :- jäsen(L, Y).

lisää([A], X, [A|X]).
lisää([A|X], Y, [A|Z]) :- lisää(X, Y, Z).
