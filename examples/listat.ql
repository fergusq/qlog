tosi :- a=a.
epätosi :- a=b.

pituus([], nolla) :- tosi.
pituus([A|H], s(P)) :- pituus(H, P).

jäsen([], Y) :- epätosi.
jäsen([X|L], X) :- tosi.
jäsen([X|L], Y) :- jäsen(L, Y).
