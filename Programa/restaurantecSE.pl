% Se importan los archivos que contienen la base de datos y el BNF
:-consult('restaurantecDataBase').
:-consult('restaurantecBNF').
:-style_check(-singleton).

%******************************************************************************
%***********************    Operaciones basicas    ****************************
%******************************************************************************

% Descripción		:	Se verifica si una lista esta vacia
% Nombre de Hecho	:	lista_vacia([],X)
% Parámetro			:	lista a verificar, resultado
% Salida            :   boolean que indica si la lista esta vacia o no
lista_vacia(List, Empty) :-
    length(List, Len),
    (   Len =< 1
    ->  Empty = true
    ;   Empty = false
    ).

% Descripción		:	Se convierte la entrada a lista
% Nombre de Hecho	:	input_to_list(X)
% Parámetro			:	resultado
% Salida            :   entrada convertida en lista
input_to_list(L):-
	read_line_to_codes(user_input,Cs),
	atom_codes(A,Cs),
	atomic_list_concat(L,' ',A).

% Descripción		:	Se convierte la entrada a string
% Nombre de Hecho	:	input_to_string(X)
% Parámetro			:	resultado
% Salida            :   entrada convertida en string
input_to_string(A):-
	read_line_to_codes(user_input,Cs),
	atom_codes(A,Cs).

% Descripción		:	Se convierte la entrada a string
% Nombre de Hecho	:	list_to_string([],X)
% Parámetro			:	lista, resultado
% Salida            :   lista ingresada convertida en string
list_to_string(List, String):-
	atomic_list_concat(List, ' ', String).

% Descripción		:	Se convierte la entrada a lista
% Nombre de Hecho	:	string_to_list_of_atoms(X,[])
% Parámetro			:	string, resultado
% Salida            :   entrada convertida en lista
string_to_list_of_atoms(X,L):-
	atom_codes(X,A),
	atomic_list_concat(L,' ',X).

% Descripción		:	Se verifica si un elemento pertenece a la lista ingresada
% Nombre de Hecho	:	miembro(X,[])
% Parámetro			:	elemento, lista
% Salida            :   boolean que indica si el elemento pertenece a la lista dada
miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).



%******************************************************************************
%******************************************************************************
%******************************************************************************




%******************************************************************************
%***************************    Sistema Experto    ****************************
%******************************************************************************


% Descripción		:	Se busca el tipo de menu
% Nombre de Hecho	:	compareTipoDeMenu([], X)
% Parámetro			:	lista, resultado
% Salida            :   nombre del menu 
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

% Descripción		:	Se obtiene la lista de menus disponibles
% Nombre de Hecho	:	listaTipoDeMenu(L)
% Parámetro			:	resultado
% Salida            :   lista de menus disponibles
listaTipoDeMenu(L) :- findall(X, (restaurante([_,X,_,_,_])) , L).

% Descripción		:	Se buscan los restaurantes 
% Nombre de Hecho	:	compareRest([], X)
% Parámetro			:	lista, resultado
% Salida            :   nombre del restaurante 
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

% Descripción		:	Se obtiene la lista de restaurantes disponibles
% Nombre de Hecho	:	listaRestaurantes(L)
% Parámetro			:	resultado
% Salida            :   lista de restaurantes disponibles
listaRestaurantes(L) :- findall(X, (restaurante([X|_])), L).

% Descripción		:	Se busca la comida
% Nombre de Hecho	:	compareComida([], X)
% Parámetro			:	lista, resultado
% Salida            :   nombre de la comida 
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

% Descripción		:	Se obtiene la lista de comidas disponibles
% Nombre de Hecho	:	listaComidas(L)
% Parámetro			:	resultado
% Salida            :   lista de comidas disponibles
listaComidas(L) :- findall(X, (menu([X|_])), L).

% Descripción		:	Se busca el sabor especifico de la comida
% Nombre de Hecho	:	compareSaborComida([], X)
% Parámetro			:	lista, resultado
% Salida            :   nombre del sabor de la comida 
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

% Descripción		:	Se obtiene la lista de sabores de comida disponibles
% Nombre de Hecho	:	listaSaborComida(L)
% Parámetro			:	resultado
% Salida            :   lista de sabores de comida disponibles
listaSaborComida(L) :- findall(X, (menu([_,_,X])), L).

% Descripción		:	Se buscan los refrescos
% Nombre de Hecho	:	compareBebida([], X)
% Parámetro			:	lista, resultado
% Salida            :   nombre del sabor de la bebida
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

% Descripción		:	Se obtiene la lista de bebidas disponibles
% Nombre de Hecho	:	listaBebidas(L)
% Parámetro			:	resultado
% Salida            :   lista de bebidas disponibles
listaBebidas(L) :- findall(X, (bebida([X|_])) , L).

% Descripción		:	Se buscan los lugares
% Nombre de Hecho	:	compareLugar([], X)
% Parámetro			:	lista, resultado
% Salida            :   nombre del lugar
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

% Descripción		:	Se obtiene la lista de lugares disponibles
% Nombre de Hecho	:	listaLugar(L)
% Parámetro			:	resultado
% Salida            :   lista de lugares disponibles
listaLugar(L) :- findall(X, (restaurante([_,_,X,_,_])) , L).

% Descripción		:	Se busca la cantidad
% Nombre de Hecho	:	compareCantidad([], X)
% Parámetro			:	lista, resultado
% Salida            :   cantidad deseada de personas
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

% Descripción		:	Se obtiene la lista de cantidades disponibles
% Nombre de Hecho	:	listaCantidad(L)
% Parámetro			:	resultado
% Salida            :   lista de cantidades disponibles
listaCantidad(Cantidad) :- numlist(1, 40, CantidadTemp),
							list_to_string(CantidadTemp, Str),
							string_to_list_of_atoms(Str,Cantidad).	

% Descripción		:	Se piden los datos al usuario
% Nombre de Hecho	:	pedirDatos(NombreRest, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada)
% Parámetro			:	NombreRestaurante, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada
% Salida            :   resultado de buscar los datos ingresados en la base de datos
pedirDatos(NombreRest, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada):- 
	% Se busca el tipo de menu
	nl, writeln('¿Que tipo de menu desea comer? (Puede ser rapida, china, etc.)'),
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

% Descripción		:	Se verifican los datos ingresados por el usuario
% Nombre de Hecho	:	verificarDatos(NombreRest, NombreIngresado, NombreIngresado2, SaborComida, SaboresDisponibles)
% Parámetro			:	NombreRestaurante, NombreIngresado, NombreIngresado2, SaborComida, SaboresDisponibles
% Salida            :   boolean que indica si los datos se encuentran en la base de datos o no
verificarDatos(NombreRest, NombreIngresado, NombreIngresado2, SaborComida, SaboresDisponibles):-	
	miembro(NombreRest, NombreIngresado), 
	miembro(NombreRest, NombreIngresado2), 
	miembro(SaborComida, SaboresDisponibles).

% Descripción		:	Busca el mejor restaurante segun los parametros dados.
% Nombre de Hecho	:	buscarRestauranteConDatosIngresados(NombreRest, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada)
% Parámetro			:	NombreRestaurante, TipoMenu, TipoComida, SaborComida, TipoBebida, LugarDeseado, CantidadDeseada
% Salida            :   referencia generada si se encuentran los datos ingresados
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

% Descripción		:	Se crea la referencia para el mejor restaurante segun los parametros dados.
% Nombre de Hecho	:	crearReferencia(NombreRest, DireccionTemp, Obligaciones)
% Parámetro			:	NombreRestaurante, DireccionTemporal, Obligaciones
% Salida            :   referencia generada si se encuentran los datos ingresados
crearReferencia(NombreRest, DireccionTemp, Obligaciones):-
	list_to_string(DireccionTemp,Direccion),

	nl, writeln('*******************************************************************************************'), nl,
	write('--> Nuestra sugerencia de restaurante es: '), write(NombreRest), writeln(' <--'), 
	write('--> La direccion es: '), write(Direccion), writeln(' <--'), 
	write('--> Tenga en cuenta que para ingresar al restaurante '), write(Obligaciones), writeln(' <--'),
	nl, writeln('*******************************************************************************************'), nl.

% Descripción		:	Se pregunta al usuario si desea buscar nuevamente.
% Nombre de Hecho	:	buscarNuevamenteAux(Respuesta)
% Parámetro			:	respuesta del usuario si desea buscar nuevamente o no
% Salida            :   mensaje de despedida en caso de no querer volver a buscar
buscarNuevamenteAux(Respuesta):- miembro('si',Respuesta),
								 pedirDatos(A,B,C,D,E,F,G).
buscarNuevamenteAux(Respuesta):- \+miembro('si',Respuesta),
								 despedida(), !.
buscarNuevamente():- nl, writeln('--> Desea buscar nuevamente? (Debe responder con si o no a la pregunta) <--'), nl,
						input_to_list(Respuesta),
						buscarNuevamenteAux(Respuesta), !.

% Descripción		:	Se muestra el mensaje de bienvenida al usuario.
% Nombre de Hecho	:	encabezado()
% Parámetro			:	-
% Salida            :   mensaje de bienvenida al usuario
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


% Descripción		:	Se muestra el mensaje de despedida al usuario.
% Nombre de Hecho	:	despedida()
% Parámetro			:	-
% Salida            :   mensaje de despedida al usuario
despedida():- nl, writeln('**************************************************************************************'), nl,
			  nl, write('         --> Muchas gracias por preferirnos. '), write('Lo esperamos pronto. <--'), nl, nl,
			  nl, writeln('**************************************************************************************'), nl, !. 

% Descripción		:	Regla principal que ejecuta el programa.
% Nombre de Hecho	:	inicio()
% Parámetro			:	-
% Salida            :   -
inicio():-
	encabezado(),
	pedirDatos(A,B,C,D,E,F,G).


%******************************************************************************
%******************************************************************************
%******************************************************************************