tosi :- a=a.
epätosi :- a=b.

peano(nolla, 0) :- tosi.
peano(s(P), X) :- peano(P, Y), X on Y+1.

pituus(L, X) :- pituus_(L, P), peano(P, X).

pituus_([], nolla) :- tosi.
pituus_([_|H], s(P)) :- pituus(H, P).

jäsen([], Y) :- epätosi.
jäsen([X|_], X) :- tosi.
jäsen([X|L], Y) :- jäsen(L, Y).
