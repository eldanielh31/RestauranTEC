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

:-consult('application_db').
:-style_check(-singleton).
:-dynamic(soluciones/1).

% BNF -------------------------------------------------------------------------------------------------------------------------------------

% Descripción		:	recibe una lista de palabras y una lista vacía y verifica si es una oración gramaticalmente correcta según la estructura establecida
% Nombre de Regla	:	oracion([A],[B])
% Parámetro			:	lista para revisar y lista vacía
% Uso				:	se utiliza para validar oraciones
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

% Descripción		:	recibe una lista de palabras y una lista vacía; elimina el primer sintagma nominal encontrado y devuelve el resto de las palabras
% Nombre de Regla	:	sintagma_nominal([A],[B])
% Parámetro			:	lista a revisar y lista vacía
% Uso				:	se utiliza para encontrar el primer sintagma nominal en una lista de palabras
sintagma_nominal(A,B):-
	determinante_m(A,C),
	sustantivo_m(C,B).
sintagma_nominal(A,B):-
	determinante_f(A,C),
	sustantivo_f(C,B).
sintagma_nominal(A,B):-
	determinante_n(A,C),
	sustantivo_f(C,B).
sintagma_nominal(A,B):-
	determinante_n(A,C),
	sustantivo_m(C,B).

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
	inicio_aux(),
	!.
validacion_gramatical(Oracion):-
	writeln('Oracion gramaticalmente incorrecta'),nl,
	writeln('Escriba de nuevo su oracion'),
	inicio_aux(),
	!.

respuesta_saludo(Nombre):-
	write('Hola '),
	writeln(Nombre),
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

removehead([_|Tail], Tail).

?- write(' '),nl.
?- write('Sistema desarrollado por: angelortizv, isolis2000, jesquivel48'),nl.
?- write('Inserte inicio(). para iniciar con el sistema experto.'),nl,nl.