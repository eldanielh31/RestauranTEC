%Ejercicio5 (miembro en listas)
miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).

pene([grueso, venudo, largo]).
