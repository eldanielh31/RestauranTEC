% application_db.pl -- archivo secundario, contiene hechos de Prolog.
%
% Este archivo es parte de  CallCenterLog, El presente tiene como objetivo el desarrollo de una aplicacion que se comporte
% como un experto en la solucion de problemas comunes de un Call Center de TI utilizando Prolog. Los Sistemas expertos,
% de ahora en adelante SE, son aplicaciones de computo que involucran experiencia no algoritmica, para resolver cierto
% tipo de problema. La interfaz debe ser completamente natural utilizando el lenguaje espanol. El usuario que presenta
% el problema, ingresa e informa al SE de todos los inconvenientes que tiene (hardware y software) que le impiden realizar
% sus tareas normalmente y finalmente puede consultar.
%
% Version de Archivo		: 0.1
% Autores					: GitHub@angelortizv, GitHub@jesquivel48, GitHub@isolis2000
% Úlitma Modificacion		: 07/09/2019, 16:00, @angelortizv


%:-consult('restaurantec_db').
:- discontiguous miembro/2.

% Palabras Clave de Usuario ---------------------------------------------------------------------------------------------------------------

% Descripción		:	Inicio de una Conversacion
% Nombre de Hecho	:	saludo([X])
% Parámetro			:	palabra clave de saludo
% Uso				:	sintagma_saludo([B])
saludo([hola|S],S).
saludo([saludos|S],S).
saludo([disculpe|S],S).
saludo([buenos,dias|S],S).
saludo([buenas,tardes|S],S).
saludo([buenas,noches|S],S).

% Descripción		:	Fin de una Conversacion
% Nombre de Hecho	:	despedida([X])
% Parámetro			:	palabra clave de despedida
% Uso				:	sintagma_saludo([B])
despedida([gracias|S],S).
despedida([muchas,gracias|S],S).
despedida([adios|S],S).
despedida([hasta_luego|S],S).
despedida([chao|S],S).

% Descripción		:	Nombre del Programa
% Nombre de Hecho	:	nombre_programa([X])
% Parámetro			:	nombre del Sistema Experto
% Uso				:	sintagma_saludo([B])
nombre_programa([callCenterLog|S],S).
nombre_programa([log|S],S).
nombre_programa([callCenter|S],S).

% Palabras Clave para el BNF --------------------------------------------------------------------------------------------------------------

%  Descripción		:	Determinantes masculinos
% Nombre de Hecho	:	determinante_m(X)
% Parámetro			:	determinantes masculinos
% Uso				:	sintagma_nominal([A],[B])
determinante_m([el|S],S).
determinante_m([lo|S],S).
determinante_m([los|S],S).
determinante_m([un|S],S).
determinante_m([unos|S],S).

% Descripción		:	Determinantes femeninos
% Nombre de Hecho	:	determinante_f([X])
% Parámetro			:	determinantes femeninos
% Uso				:	sintagma_nominal([A],[B])
determinante_f([la|S],S).
determinante_f([las|S],S).
determinante_f([una|S],S).
determinante_f([unas|S],S).

% Descripción		:	Determinantes neutros
% Nombre de Hecho	:	determinante_n([X])
% Parámetro			:	determinantes neutros
% Uso				:	sintagma_nominal([A],[B])
determinante_n([nosotros|S],S).
determinante_n([yo|S],S).

% Descripción		:	Negaciones
% Nombre de Hecho	:	negacion([X])
% Parámetro			:	adverbios de negacion
% Uso				:	oracion([A],[B])
negacion([no|S],S).
negacion([nunca|S],S).

% Descripción		:	sustantivos masculinos
% Nombre de Hecho	:	sustantivo_m([X])
% Parámetro			:	sustantivos masculinos
% Uso				:	sintagma_nominal([A],[B])
sustantivo_m([sushi|S],S).
sustantivo_m([pescado|S],S).
sustantivo_m([pescados|S],S).
sustantivo_m([nacho|S],S).
sustantivo_m([nachos|S],S).
sustantivo_m([taco|S],S).
sustantivo_m([tacos|S],S).
sustantivo_m([calzone|S],S).
sustantivo_m([calzones|S],S).
sustantivo_m([refresco|S],S).
sustantivo_m([refrescos|S],S).


% Descripción		:	sustantivos femeninos
% Nombre de Hecho	:	sustantivo_f([X])
% Parámetro			:	sustantivos femeninos
% Uso				:	sintagma_nominal([A],[B])

sustantivo_f([hamburguesa|S],S).
sustantivo_f([hamburguesas|S],S).
sustantivo_f([pizza|S],S).
sustantivo_f([pizzas|S],S).
sustantivo_f([pasta|S],S).
sustantivo_f([pastas|S],S).
sustantivo_f([papa|S],S).
sustantivo_f([papas|S],S).
sustantivo_f([bebida|S],S).
sustantivo_f([bebidas|S],S).

% Descripción		:	sustantivos de lugar
% Nombre de Hecho	:	sustantivo_l([X])
% Parámetro			:	sustantivos de lugar
% Uso				:	sintagma_nominal([A],[B])

sustantivo_l([san,pedro|S],S).
sustantivo_l([san,jose|S],S).
sustantivo_l([cartago|S],S).
sustantivo_l([puntarenas|S],S).
sustantivo_l([heredia|S],S).
sustantivo_l([guanacaste|S],S).
sustantivo_l([alajuela|S],S).
sustantivo_l([limon|S],S).
/*
sustantivo_l([hamburguesas|S],S).
sustantivo_l([pizza|S],S).
sustantivo_l([pizzas|S],S).
sustantivo_l([pasta|S],S).
sustantivo_l([pastas|S],S).
sustantivo_l([papa|S],S).
sustantivo_l([papas|S],S).
sustantivo_l([bebida|S],S).
sustantivo_l([bebidas|S],S).
*/

% Descripción		:
% Nombre de Hecho	:	inicio causa_ref([X])
% Parámetro			:
% Uso				:
inicio_cr([posibles,causas,del,problema|S],S).
inicio_cr([algunas,referencias,para,el,problema|S],S).

% Descripción		:	Verbos
% Nombre de Hecho	:	verbo([X])
% Parámetro			:	verbos utilizables
% Uso				:	sintagma_verbal([A],[B])

verbo([comer|S], S).
verbo([tomar|S], S).
verbo([beber|S], S).
verbo([quiero|S],S).
verbo([quiero,comer|S],S).
verbo([quiero,tomar|S],S).
verbo([queremos,comer|S],S).
verbo([queremos,tomar|S],S).
verbo([queremos,beber|S],S).

verbo([quiero,estar,cerca,de|S],S).
verbo([quiero,algo,cerca,de|S],S).
verbo([quiero,estar,alrededor,de|S],S).
verbo([quiero,algo,alrededor,de|S],S).
verbo([queremos,estar,cerca,de|S],S).
verbo([queremos,algo,cerca,de|S],S).
verbo([queremos,estar,alrededor,de|S],S).
verbo([queremos,algo,alrededor,de|S],S).
















% main.pl -- archivo principal para correr CallCenterLog, contiene reglas de Prolog.
%
% Este archivo es parte de  CallCenterLog, El presente tiene como objetivo el desarrollo de una aplicación que se comporte
% como un experto en la solución de problemas comunes de un Call Center de TI utilizando Prolog. Los Sistemas expertos,
% de ahora en adelante SE, son aplicaciones de cómputo que involucran experiencia no algorítmica, para resolver cierto
% tipo de problema. La interfaz debe ser completamente natural utilizando el lenguaje español. El usuario que presenta
% el problema, ingresa e informa al SE de todos los inconvenientes que tiene (hardware y software) que le impiden realizar
% sus tareas normalmente y finalmente puede consultar.
%
% Version de Archivo		: 0.1
% Autores					: GitHub@angelortizv, GitHub@jesquivel48, GitHub@isolis2000
% Úlitma Modificacion		: 07/09/2019, 16:00, @angelortizv
%Hola como esras?

%:-consult('application_db').
:-style_check(-singleton).
:-dynamic(soluciones/1).

% BNF -------------------------------------------------------------------------------------------------------------------------------------

% Descripción		:	recibe una lista de palabras y una lista vacía y verifica si es una oración gramaticalmente correcta según la estructura establecida
% Nombre de Regla	:	oracion([A],[B])
% Parámetro			:	lista para revisar y lista vacía
% Uso				:	se utiliza para validar oraciones

oracion(A,B):-
    sintagma_nominal(A,C).
    %sintagma_verbal(C,B).

/*
oracion(A,B):-
    sintagma_nominal(A,C),
    sintagma_verbal(C,B).
	%sintagma_nominal(A,C).



oracion(A,B):-
	sintagma_nominal(A,C),
	sintagma_verbal(C,B).
oracion(A,B):-
	sintagma_nominal(A,C),
	negacion(C,D),
	sintagma_verbal(D,B).
oracion(A,B):-
	inicio_cr(A,C),
	sintagma_nominal(C,D),
	sintagma_verbal(D,B).
oracion(A,B):-
	inicio_cr(A,C),
	sintagma_nominal(C,D),
	negacion(D,E),
	sintagma_verbal(E,B).
*/

% Descripción		:	recibe una lista de palabras y una lista vacía; elimina el primer sintagma nominal encontrado y devuelve el resto de las palabras
% Nombre de Regla	:	sintagma_nominal([A],[B])
% Parámetro			:	lista a revisar y lista vacía
% Uso				:	se utiliza para encontrar el primer sintagma nominal en una lista de palabras

/*
% Tipos de comida
sintagma_nominal(A,B):-
    determinante_m(A,C),
    verbo(C,Z),
	sustantivo_m(Z,B).
sintagma_nominal(A,B):-
    determinante_f(A,C),
    verbo(C,Z),
	sustantivo_f(Z,B).
sintagma_nominal(A,B):-
    determinante_n(A,C),
    verbo(C,Z),
	sustantivo_f(Z,B).
sintagma_nominal(A,B):-
    determinante_n(A,C),
    verbo(C,Z),
	sustantivo_m(Z,B).

sintagma_nominal(A,B):-
    determinante_n(A,C),
    verbo(C,Z),
    determinante_m(Z,Y),
	sustantivo_m(Y,B).
sintagma_nominal(A,B):-
    determinante_n(A,C),
    verbo(C,Z),
    determinante_f(Z,Y),
	sustantivo_f(Y,B).
sintagma_nominal(A,B):-
    verbo(A,Z),
	sustantivo_m(Z,B).
sintagma_nominal(A,B):-
    verbo(A,Z),
	sustantivo_f(Z,B).
*/

% Lugar deseado
sintagma_nominal(A,B):-
    determinante_n(A,C),
    verbo(C,Z),
	sustantivo_l(Z,B).


% Descripción		:	recibe una lista de palabras y una lista vacía; elimina el primer sintagma verbal encontrado y devuelve el resto de las palabras
% Nombre de Regla	:	sintagma_verbal([A],[B])
% Parámetro			:	lista a revisar y lista vacía
% Uso				:	se utiliza para encontrar el primer sintagma verbal en una lista de palabras
sintagma_verbal(A,B):-
	verbo(A,B).

sintagma_verbal(A,B):-
	verbo(A,C),
	sintagma_nominal(C,B).


% Descripción		:	recibe una lista de palabras y una lista vacía y verifica si estas palabras componen un saludo al programa (Ej. “hola log”)
% Nombre de Regla	:	sintagma_saludo([B])
% Parámetro			:	lista a revisar y lista vacía
% Uso				:	se utiliza para encontrar el primer sintagma saludo en una lista de palabras
sintagma_saludo(B):-
	input_to_list(L),
	saludo(L,C),
	nombre_programa(C,B),!.
sintagma_saludo(B):-
	sintagma_saludo([]).

% ValidaciÓn Gramatical, Saludo, Despedida ------------------------------------------------------------------------------------------------

% Descripción		:	valida si la oración digitada por el usuario está gramaticalmente correcta según el BNF establecido
% Nombre de Regla	:	validacion_gramatical()
% Parámetro			:	lista a revisar
% Uso				:	Se utiliza para verificar gramaticalmente una oración, de lo contrario, devolver un mensaje al usuario
validacion_gramatical(Oracion):-
    oracion(Oracion,[]),
	!.
validacion_gramatical(Oracion):-
	is_list(Oracion),
	lista_vacia(Oracion,true),
	writeln('En que lo puedo ayudar?'),nl,
	%inicio_aux(),
	!.
validacion_gramatical(Oracion):-
	writeln('Oracion gramaticalmente incorrecta #1'),
	writeln('Escriba de nuevo su oracion #1'),nl,
	%inicio_aux(),
    false,
	!.

respuesta_saludo():-
	write('Hola '),
	%writeln(Nombre),
	writeln('En que lo puedo ayudar?').

respuesta_despedida():-
	writeln('Algo mas en que pueda servirle?'),nl,
	read(R),
	opcion_despedida(R).
opcion_despedida(R):-
	consulta_general(no,R),nl,writeln('Gracias por preferirnos'),nl,!;
	inicio_aux().

% Operaciones Basicas ------------------------------------------------------------------------------------------------------------

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

concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):-
	concatenar(L1,L2,L3).

eliminar_primeros(L,Y,B):- length(X, B), append(X,Y,L).

obtener_elemento([Y|_], 1, Y).
obtener_elemento([_|Xs], N, Y):-
          N2 is N - 1,
          obtener_elemento(Xs, N2, Y).


% Causas y referencias --------------------------------------------------------------------------------------------------------------------

% Descripción		:	Obtiene las causas a un determinado problema
% Nombre de Regla	:	obtener_causas(X,A)
% Parámetro			:	problema definido en application_db
% Uso				:
obtener_causas(X,A):-
	split_string(A, "', ,?" ,"', ,?", L),
	eliminar_primeros(L,Y,4),
	atomic_list_concat(Y, ' ', X),
	causas(X).

causas(A):-
	write('Las principales causas que pueden estar asociadas a: '),
	write(A), write(' son:'), nl,nl,
	causa(B,A),
	write(B),nl.

% Descripción		:	Obtiene las referencias a un determinado problema
% Nombre de Regla	:	obtener_referencias(X,A)
% Parámetro			:	probolema definido en application_db
% Uso				:
obtener_referencias(X,A):-
	split_string(A, "', ,?" ,"', ,?", L),
	eliminar_primeros(L,Y,5),
	atomic_list_concat(Y, ' ', X),
	referencias(X).

referencias(A):-
	write('Algunas referencias para su problema son: '),nl,
	referencia(E,A),
	write(E),nl.

% Consultas, Solución de Problemas, Conversación usuario-se -------------------------------------------------------------------------------

% Descripción		:	Envía a consulta_no(A,D) pregunta al usuario sobre determinado problema
% Nombre de Regla	:	hoja_izquierda(B)
% Parámetro			:	causa de un problema
% Uso				:	raiz(B,A)
hoja_izquierda(B):-
    pregunta(D,B),
    consulta_no(B, D).

% Descripción		:	concatena las soluciones a un determinado problema
% Nombre de Regla	:	consulta_no(A,P)
% Parámetro			:	(causa de un problema, pregunta asociada)
% Uso				:	hoja_izquierda(B)
consulta_no(A, D):-
    write(D), nl,
    read(R), nl,
    soluciones(L),
    concatenar(L, [A, R], NL),
    retractall(soluciones(_)),
    assert(soluciones(NL)),
    consulta_general(no, R).

consulta_caso_base(B):-
	solucion(C,B).
consulta_general(R,R).

% Descripción		:	Realiza el ciclo de conversación entre preguntas y respuestas, y despedida
% Nombre de Regla	:	conversascion(Oracion)
% Parámetro			:	String de una oración
% Uso				:	inicio_aux()

conversacion(Oracion,'referencias'):-
	!,
	obtener_referencias(_,Oracion),
	respuesta_despedida().

conversacion(Oracion,'causas'):-
	!,
	obtener_causas(_,Oracion),
	respuesta_despedida().

conversacion(Oracion,_):-
	write('Responda con si. o no. a las siguientes preguntas'),nl,nl,
	retractall(soluciones(_)),
	assert(soluciones([])),
	% write(Oracion),
	raiz(A,Oracion),
	solucion(B,A),
	write(B),nl,
	respuesta_despedida().

% Ejecutor SE -----------------------------------------------------------------------------------------------------------------------------

encabezado():-
	sleep(0.02),
		write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl,
		sleep(0.02),
		write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl,
		sleep(0.02),
		write('       |||||||||||||||||||||||| Call Center Log |||||||||||||||||||||||||       '),nl,
		sleep(0.02),
		write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl,
		sleep(0.02),
		write('       ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||       '),nl.

/*
inicio():-
	encabezado(),
	sintagma_saludo([]),
	writeln('Hola usuario'),
	writeln('¿Cual es su nombre?'),
	input_to_string(Nombre),
	respuesta_saludo(Nombre),
	inicio_aux().

inicio_aux():-
	input_to_list(Oracion),
	validacion_gramatical(Oracion),nl,nl,
	writeln('Para CallCenterLog es un gusto ayudarle con su problema,'),nl,
	obtener_elemento(Oracion,2,A),
	removehead(Oracion,B),
	list_to_string(B,Y),
	conversacion(Y,A),nl.
*/

removehead([_|Tail], Tail).




restaurante([tacobell, wjdwdbnewd, daddad, adae]).
restaurante([mcdonald, edef, edecew, dscewc]).


listaRestaurantes(L) :- findall(X, (restaurante([X|_])), L).

miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).

compareRest([],X):- X = 'no hay restaurante'.
compareRest([H|_], X):- listaRestaurantes(L),
		                miembro(H,L) , X = H, !.
compareRest([H|T], X):- listaRestaurantes(L),
	                    \+miembro(H,L),
	                    compareRest(T,X).



getRest(NombreRest,Y):- not(restaurante([NombreRest|_])),
                        Y = 'no hay restaurante',
                        !.
getRest(NombreRest, Y):-restaurante([NombreRest|_]),
                        Y = NombreRest.


start(Y):-
    %input_to_list(Oracion),
    oracion(X,[]),
    Y = X.

iniciar():-
    respuesta_saludo(),  
    input_to_list(Oracion),
    % Se busca el restaurante
    compare(Oracion, Rest),
    X = Rest.






?- write(' '),nl.
?- write('Sistema desarrollado por: angelortizv, isolis2000, jesquivel48'),nl.
?- write('Inserte inicio(). para iniciar con el sistema experto.'),nl,nl.

