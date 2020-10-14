% Se importan los archivos que contiene la base de datos y el BNF
:-consult('restaurantecDataBase').
:-consult('restaurantecBNF').
:-style_check(-singleton).

%******************************************************************************
%***********************    Operaciones basicas    ****************************
%******************************************************************************


%  Descripción		:	Determinantes masculinos
% Nombre de Hecho	:	determinante_m(X)
% Parámetro			:	determinantes masculinos
% Uso				:	sintagma_nominal([A],[B])
lista_vacia(List, Empty) :-
    length(List, Len),
    (   Len =< 1
    ->  Empty = true
    ;   Empty = false
    ).

input_to_list(L):-
	read_line_to_codes(user_input,Cs),
	atom_codes(A,Cs),
	atomic_list_concat(L,' ',A).

input_to_string(A):-
	read_line_to_codes(user_input,Cs),
	atom_codes(A,Cs).

list_to_string(List, String):-
	atomic_list_concat(List, ' ', String).

string_to_list_of_atoms(X,L):-
	atom_codes(X,A),
	atomic_list_concat(L,' ',X).

miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).



%******************************************************************************
%******************************************************************************
%******************************************************************************




%******************************************************************************
%***************************    Sistema Experto    ****************************
%******************************************************************************


% Se busca el tipo de menu
listaTipoDeMenu(L) :- findall(X, (restaurante([_,X,_,_,_])) , L).

compareTipoDeMenu([],X):- nl, writeln('El tipo de menu ingresado no esta disponible'),
						  writeln('Intente de nuevo'),nl,
						  input_to_list(Oracion),
						  compareTipoDeMenu(Oracion,X).
compareTipoDeMenu([H|_], X):- listaTipoDeMenu(L),
							  miembro(H,L),
							  X = H, !.
compareTipoDeMenu([H|T], X):- listaTipoDeMenu(L),
	                          \+miembro(H,L),
	                          compareTipoDeMenu(T,X).

% Se buscan los restaurantes 
listaRestaurantes(L) :- findall(X, (restaurante([X|_])), L).

compareRest([],X):- nl, writeln('El restaurante ingresado no esta disponible'),
					writeln('Intente de nuevo'),nl,
					input_to_list(Oracion),
					compareRest(Oracion,X).
compareRest([H|_], X):- listaRestaurantes(L),
						miembro(H,L),
						X = H, !.
compareRest([H|T], X):- listaRestaurantes(L),
	                    \+miembro(H,L),
	                    compareRest(T,X).

% Se busca la comida
listaComidas(L) :- findall(X, (menu([X|_])), L).

compareComida([],X):- nl, writeln('La comida ingresada no esta disponible'),
					  writeln('Intente de nuevo'),nl,
					  input_to_list(Oracion),
					  compareComida(Oracion,X).
compareComida([H|_], X):- listaComidas(L),
						  miembro(H,L), 
						  X = H, !.
compareComida([H|T], X):- listaComidas(L),
	                      \+miembro(H,L),
	                      compareComida(T,X).

% Se buscan el sabor especifico de la comida
listaSaborComida(L) :- findall(X, (menu([_,_,X])), L).

compareSaborComidaAux([H|_], X):- listaSaborComida(L),
						          flatten(L,Y),
						          X = Y.

compareSaborComida([],X):- nl, writeln('El sabor de la comida ingresado no esta disponible'),
						   writeln('Intente de nuevo'),nl,
						   input_to_list(Oracion),
						   compareSaborComida(Oracion,X).
compareSaborComida([H|_], X):- compareSaborComidaAux(C, Y),
							   miembro(H,Y), 
							   X = H, !.
compareSaborComida([H|T], X):- compareSaborComidaAux(C, Y),
	                           \+miembro(H,Y),
	                           compareSaborComida(T,X).

% Se buscan los refrescos
listaBebidas(L) :- findall(X, (bebida([X|_])) , L).

compareBebida([],X):- nl, writeln('El sabor de la bebida ingresada no esta disponible'),
					  writeln('Intente de nuevo'),nl,
					  input_to_list(Oracion),
					  compareBebida(Oracion,X).
compareBebida([H|_], X):- listaBebidas(L),
						  miembro(H,L),
						  X = H, !.
compareBebida([H|T], X):- listaBebidas(L),
	                      \+miembro(H,L),
	                      compareBebida(T,X).

% Se buscan los lugares
listaLugar(L) :- findall(X, (restaurante([_,_,X,_,_])) , L).

compareLugarAux([H|_], X):- listaLugar(L),
						    flatten(L,Y),
						    X = Y.

compareLugar([],X):- nl, writeln('El lugar ingresado no esta disponible'),
					 writeln('Intente de nuevo'),nl,
					 input_to_list(Oracion),
					 compareLugar(Oracion,X).
compareLugar([H|_], X):- compareLugarAux(C, Y),
						 miembro(H,Y), 
						 X = H, !.
compareLugar([H|T], X):- compareLugarAux(C, Y),
	                     \+miembro(H,Y),
	                     compareLugar(T,X).

% Se busca la cantidad
listaCantidad(Cantidad) :- numlist(1, 40, CantidadTemp),
							list_to_string(CantidadTemp, Str),
							string_to_list_of_atoms(Str,Cantidad).	

compareCantidad([],X):-  nl, writeln('No hay cupo para la cantidad de personas ingresadas'),
						 writeln('Intente de nuevo'),nl,
						 input_to_list(Oracion),
						 compareCantidad(Oracion,X).
compareCantidad([H|_], X):- listaCantidad(L), 
							miembro(H,L) , 
							atom_number(H, Y),
							X = Y, !.
compareCantidad([H|T], X):- listaCantidad(L),
	                        \+miembro(H,L),
	                        compareCantidad(T,X).


pedirDatos(NombreRest, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada):- 
	% Se busca el tipo de menu
	nl, writeln('¿Que tipo de menu desea comer? (Puede se rapida, china, etc.)'),
	input_to_list(Oracion),
	validacion_gramatical(Oracion),
    compareTipoDeMenu(Oracion, TipoMenuTemp),
	TipoMenu = TipoMenuTemp,
	
	% Se busca el restaurante
	nl, writeln('¿En cual restaurante desea comer?'),
	input_to_list(Oracion2),
	validacion_gramatical(Oracion2),
    compareRest(Oracion2, RestTemp),
	NombreRest = RestTemp,	

	% Se busca la comida
	nl, write('¿Cual comida de '), write(NombreRest), write(' desea?'), nl,
	input_to_list(Oracion3),
	validacion_gramatical(Oracion3),
	compareComida(Oracion3, ComidaTemp),
	TipoComida = ComidaTemp,

	% Se busca algun sabor especifico
	nl, write('¿Cual tipo de '), write(TipoComida), write(' desea?'), nl,
	input_to_list(Oracion4),
	validacion_gramatical(Oracion4),
	compareSaborComida(Oracion4, SaborComidaTemp),
	SaborComida = SaborComidaTemp,

	% Se busca la bebida
	nl, writeln('¿Que le gustaria de tomar?'),
	input_to_list(Oracion5),
	validacion_gramatical(Oracion5),
	compareBebida(Oracion5, BebidaTemp),
	TipoBebida = BebidaTemp,

	% Se busca el lugar
	nl, write('¿En cual provincia le gustaria buscar '), write(NombreRest), write(' ?'), nl,
	input_to_list(Oracion6),
	validacion_gramatical(Oracion6),
	compareLugar(Oracion6, LugarTemp),
	LugarDeseado = LugarTemp,

	% Se busca la cantidad
	nl, writeln('¿Para cuantas personas le gustaria buscar?'),
	input_to_list(Oracion7),
	validacion_gramatical(Oracion7),
	compareCantidad(Oracion7, CantidadTemp),
	CantidadDeseada = CantidadTemp,

	% Se validan los datos y se busca la referencia en caso de existir
	buscarRestauranteConDatosIngresados(NombreRest, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada).


verificarDatos(NombreRest, NombreIngresado, NombreIngresado2, SaborComida, SaboresDisponibles):-	
	miembro(NombreRest, NombreIngresado), 
	miembro(NombreRest, NombreIngresado2), 
	miembro(SaborComida, SaboresDisponibles).

%busca el mejor restaurante segun los parametros dados.
buscarRestauranteConDatosIngresados(NombreRest, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada):-
    restaurante([NombreRest, TipoMenu, [LugarDeseado | Direccion], RCapacidad, Obligaciones]),
	CantidadDeseada > 0,
	CantidadDeseada =< RCapacidad,
	menu([TipoComida, NombreIngresado, SaboresDisponibles]),
	bebida([TipoBebida, NombreIngresado2]),
	verificarDatos(NombreRest, NombreIngresado, NombreIngresado2, SaborComida, SaboresDisponibles),
	crearReferencia(NombreRest, Direccion, Obligaciones),
	buscarNuevamente(), !.

buscarRestauranteConDatosIngresados(NombreRest, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada):-
	restaurante([NombreRest, TipoMenu, [LugarDeseado | Direccion], RCapacidad, Obligaciones]),
	CantidadDeseada > 0,
	CantidadDeseada =< RCapacidad,
	menu([TipoComida, NombreIngresado, SaboresDisponibles]),
	bebida([TipoBebida, NombreIngresado2]),
	\+verificarDatos(NombreRest, NombreIngresado, NombreIngresado2, SaborComida, SaboresDisponibles),
	nl, writeln('**************************************************************************************'), nl,
	nl, writeln('--> No se pudo encuentrar ningun restaurante con los datos ingresados. Lo sentimos <--'), nl,
	nl, writeln('**************************************************************************************'), nl,
	buscarNuevamente(), !.

crearReferencia(NombreRest, DireccionTemp, Obligaciones):-
	list_to_string(DireccionTemp,Direccion),
	%TempRef = [],

	%list_to_string(TempRef, Ref), nl,
	nl, writeln('*******************************************************************************************'), nl,
	write('--> Nuestra sugerencia de restaurante es: '), write(NombreRest), writeln(' <--'), 
	write('--> La direccion es: '), write(Direccion), writeln(' <--'), 
	write('--> Tenga en cuenta que para ingresar al restaurante '), write(Obligaciones), writeln(' <--'),
	nl, writeln('*******************************************************************************************'), nl.

buscarNuevamenteAux(Respuesta):- miembro('si',Respuesta),
								 pedirDatos(A,B,C,D,E,F,G).
buscarNuevamenteAux(Respuesta):- \+miembro('si',Respuesta),
								 despedida(), !.

buscarNuevamente():- nl, writeln('--> Desea buscar nuevamente? (Debe responder con si o no a la pregunta) <--'), nl,
						input_to_list(Respuesta),
						buscarNuevamenteAux(Respuesta), !.


encabezado():-
	sleep(0.02),
	write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl,
	sleep(0.02),
	write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl,
	sleep(0.02),
	write('       ||||||||||||||||||||||||||| RestauranTEC |||||||||||||||||||||||||       '),nl,
	sleep(0.02),
	write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl,
	sleep(0.02),
	write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl, nl,
	write('          Bienvenido(a)(os)(as) a RestauranTEC, es un placer ayudarle(s) '), nl,nl.



despedida():- nl, writeln('**************************************************************************************'), nl,
			  nl, write('         --> Muchas gracias por preferirnos. '), write('Lo esperamos pronto. <--'), nl, nl,
			  nl, writeln('**************************************************************************************'), nl, !. 

inicio():-
	encabezado(),
	pedirDatos(A,B,C,D,E,F,G).


%******************************************************************************
%******************************************************************************
%******************************************************************************