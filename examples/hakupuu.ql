hae(A, X, haara(A, X, _, _)) !.
hae(A, X, haara(Avain, Arvo, Vasen, Oikea)) :-
	A < Avain,
	hae(A, X, Vasen)
!;	A > Avain,
	hae(A, X, Oikea).

dhae(A, X, haara(A, X, _, _)).
dhae(A, X, haara(Avain, Arvo, Vasen, Oikea)) :-
	dhae(A, X, Vasen)
;	dhae(A, X, Oikea).

aseta(A, X, lehti, haara(A, X, lehti, lehti)) !.
aseta(A, X, haara(Avain, Arvo, V1, O1), haara(Avain, Arvo, V2, O2)) :-
	A < Avain,
	aseta(A, X, V1, V2),
	O1 = O2
!;	A > Avain,
	aseta(A, X, O1, O2),
	V1 = V2.

aseta_monta([], [], P, P).
aseta_monta([A|At], [X|Xt]) --> aseta(A, X), aseta_monta(At, Xt).
