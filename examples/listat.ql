tosi :- a=a.
epätosi :- a=b.

peano(nolla, 0) :- tosi.
peano(s(P), X) :- peano(P, Y), X on Y+1.

pituus(L, X) :- pituus_(L, P), peano(P, X).

pituus_([], nolla) :- tosi.
pituus_([A|H], s(P)) :- pituus(H, P).

jäsen([], Y) :- epätosi.
jäsen([X|L], X) :- tosi.
jäsen([X|L], Y) :- jäsen(L, Y).
