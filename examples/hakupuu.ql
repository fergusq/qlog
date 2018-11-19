hae(A, haara(A, X, _, _), X) !.
hae(A, haara(Avain, Arvo, Vasen, Oikea), X) :-
	A < Avain,
	hae(A, Vasen, X)
!;	A > Avain,
	hae(A, Oikea, X).

dhae(A, haara(A, X, _, _), X).
dhae(A, haara(Avain, Arvo, Vasen, Oikea), X) :-
	dhae(A, Vasen, X)
;	dhae(A, Oikea, X).

aseta(A, X, lehti, haara(A, X, lehti, lehti)) !.
aseta(A, X, haara(A, _, V, O), haara(A, X, V, O)) !.
aseta(A, X, haara(Avain, Arvo, V1, O1), haara(Avain, Arvo, V2, O2)) :-
	A < Avain,
	aseta(A, X, V1, V2),
	O1 = O2
!;	A > Avain,
	aseta(A, X, O1, O2),
	V1 = V2.

sisältö(P, At) :- sisältö_(P, At, []).

sisältö_(lehti) --> [].
sisältö_(haara(_, A, V, O)) --> [A], sisältö_(V), sisältö_(O).

aseta_monta([], [], P, P).
aseta_monta([A|At], [X|Xt]) --> aseta(A, X), aseta_monta(At, Xt).

indeksit_välillä(A, A, [A]) !.
indeksit_välillä(A, B, []) :- A>B !.
indeksit_välillä(A, B, [I|It]) :-
	I on (A+B)/2,
	B2 on I-1,
	A2 on I+1,
	indeksit_välillä(A, B2, It1),
	indeksit_välillä(A2, B, It2),
	lisää(It1, It2, It).

näytä_puu(lehti, _).
näytä_puu(haara(Avain, Arvo, Vasen, Oikea), S) :-
	S2 on S+1,
	näytä_puu(Vasen, S2),
	sisennys(S),
	näytä(Avain),
	tulosta("->"),
	näytä(Arvo),
	rv,
	näytä_puu(Oikea, S2).
