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


%:-consult('restaurantec_db.pl').
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

% Descripción		:	Sustantivos
% Nombre de Hecho	:	sustantivo_g([X])
% Parámetro			:	sustantivo general (anónimo)
% Uso				:	sintagma_nominal([A],[B])

sustantivo_g([_|S],S).

% Descripción		:	Verbos
% Nombre de Hecho	:	verbo([X])
% Parámetro			:	verbos utilizables
% Uso				:	sintagma_verbal([A],[B])

verbo([comer|S], S).
verbo([tomar|S], S).
verbo([beber|S], S).
verbo([quiero|S],S).
verbo([queremos|S],S).
verbo([quiero,comer|S],S).
verbo([quiero,tomar|S],S).
verbo([quiero,beber|S],S).
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

verbo([seria|S], S).
verbo([seriamos|S], S).
verbo([somos|S], S).













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


% Descripción		:	recibe una lista de palabras y una lista vacía; elimina el primer sintagma nominal encontrado y devuelve el resto de las palabras
% Nombre de Regla	:	sintagma_nominal([A],[B])
% Parámetro			:	lista a revisar y lista vacía
% Uso				:	se utiliza para encontrar el primer sintagma nominal en una lista de palabras

sintagma_nominal(A,B):-
    determinante_m(A,C),
    sintagma_verbal(C,Z),
	sustantivo_g(Z,B).
sintagma_nominal(A,B):-
    determinante_f(A,C),
    sintagma_verbal(C,Z),
	sustantivo_g(Z,B).
sintagma_nominal(A,B):-
    determinante_n(A,C),
    sintagma_verbal(C,Z),
	sustantivo_g(Z,B).
sintagma_nominal(A,B):-
    determinante_n(A,C),
    sintagma_verbal(C,Z),
    determinante_m(Z,Y),
	sustantivo_g(Y,B).
sintagma_nominal(A,B):-
    determinante_n(A,C),
    sintagma_verbal(C,Z),
    determinante_f(Z,Y),
	sustantivo_g(Y,B).
sintagma_nominal(A,B):-
    determinante_n(A,C),
    sintagma_verbal(C,Z),
	sustantivo_g(Z,B).
sintagma_nominal(A,B):-
	sintagma_verbal(A,C),
    sustantivo_g(C,B).
sintagma_nominal(A,B):-
    sintagma_verbal(A,B).


% Descripción		:	recibe una lista de palabras y una lista vacía; elimina el primer sintagma verbal encontrado y devuelve el resto de las palabras
% Nombre de Regla	:	sintagma_verbal([A],[B])
% Parámetro			:	lista a revisar y lista vacía
% Uso				:	se utiliza para encontrar el primer sintagma verbal en una lista de palabras

sintagma_verbal(A,B):-
	verbo(A,B).

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








%******************************************************************************
%******************************************************************************
%******************************************************************************


%Restaurantes disponibles en la app
%formato: [nombre, tipoMenu, [direccion], [capacidad], [disposiciones]]

%San Jose
restaurante([mcdonald, rapida, [sanjose, 'Plaza del Sol'], 30, 'el uso de mascarilla obligatorio']).
restaurante([woods, italiana, [sanjose, 'Curridabat'], 20, 'el uso de mascarilla obligatorio']).
restaurante([tacobell, mexicana, [sanjose, 'San Pedro'], 30, 'el uso de mascarilla obligatorio']).
restaurante([kfc, rapida,[sanjose, 'San Pedro'],20,'el uso de mascarilla obligatorio']).
restaurante([marisqueando, mariscos,[sanjose, 'Centro de Desamparados'],20,'el uso de mascarilla obligatorio']).
restaurante([hongkong,china,[sanjose, 'Moravia'],20,'el uso de mascarilla obligatorio']).
restaurante([yokohama, japonesa, [sanjose, '200 mts al oeste de la Universidad de Costa Rica'], 20, 'el uso de mascarilla obligatorio']).

%Cartaguito campeon
restaurante([autogrill, bar, [cartago, 'Mall Paseo Metropoli'], 40, 'el uso de mascarilla obligatorio']).
restaurante([mcdonald, rapida, [cartago, 'MetroCentro'], 30, 'el uso de mascarilla obligatorio']).
restaurante([pizzahut, italiana, [cartago, 'ruinas'], 5, 'el uso de mascarilla obligatorio']).
restaurante([tejarena, rapida, [cartago, 'tejar'], 10, 'el uso de mascarilla obligatorio']).
restaurante([tacobell, mexicana, [cartago, '100mts norte de las Ruinas'], 30, 'el uso de mascarilla obligatorio']).
restaurante([yokohama, japonesa, [cartago, 'Plaza Boulevard, Blvd. el Molino, Provincia de Cartago, Cartago'], 20, 'el uso de mascarilla obligatorio']).
restaurante([kfc, rapida,[cartago, '50 mts al sur de las Ruinas'],20,'el uso de mascarilla obligatorio']).
restaurante([linfei,china,[cartago, '200 mts al este de la Basilica de Los Angeles'],20,'el uso de mascarilla obligatorio']).

%Puntarenas
restaurante([mcdonald, rapida, [puntarenas, 'Plaza Centenario'], 30, 'el uso de mascarilla obligatorio']).
restaurante([rostipollos, rapida, [puntarenas, 'Hotel Alamar'], 30, 'el uso de mascarilla obligatorio']).
restaurante([pizzahut, italiana, [puntarenas, 'Ocean Mall'], 20, 'el uso de mascarilla obligatorio']).
restaurante([fishy, mariscos, [puntarenas, 'Parque Mora'], 30, 'el uso de mascarilla obligatorio']).
restaurante([kfc, rapida,[puntarenas, 'Puntarenas Down Town'],20,'el uso de mascarilla obligatorio']).
restaurante([yokohama, japonesa, [puntarenas, 'Puntarenas Down Town'],20,'el uso de mascarilla obligatorio']).
restaurante([tacobell, mexicana, [puntarenas, 'Plaza La Rioja'], 30, 'el uso de mascarilla obligatorio']).
restaurante([wingshun,china,[puntarenas, 'Centro de la capital'],20,'el uso de mascarilla obligatorio']).

%Heredia
restaurante([mcdonald, rapida, [heredia, 'Oxigeno Human Playground'], 30, 'el uso de mascarilla obligatorio']).
restaurante([pizzahut, italiana, [heredia, 'Mall Paseo de Las Flores'], 20, 'el uso de mascarilla obligatorio']).
restaurante([mariscosymas, mariscos, [heredia, 'Entrada principal de la UNA 100 oeste y 25 Sur'], 30, 'el uso de mascarilla obligatorio']).
restaurante([kfc, rapida,[heredia, 'Mall Paseo de Las Flores'],20,'el uso de mascarilla obligatorio']).
restaurante([yokohama, japonesa, [heredia, 'Contiguo Al AYA De San Pablo 112, Heredia, San Pablo'],20,'el uso de mascarilla obligatorio']).
restaurante([tacobell, mexicana, [heredia, 'Mall Paseo de Las Flores'], 30, 'el uso de mascarilla obligatorio']).
restaurante([lungfung,china,[heredia, 'Limón, Heredia'],20,'el uso de mascarilla obligatorio']).

%Alajuela
restaurante([mcdonald, rapida, [alajuela, 'Diagonal Bomba Provincia de Alajuela La Tropicana, Provincia de Alajuela'], 30, 'el uso de mascarilla obligatorio']).
restaurante([pizzahut, italiana, [alajuela, 'Provincia de Alajuela, Alajuela'], 20, 'el uso de mascarilla obligatorio']).
restaurante([limonta, mariscos, [alajuela, 'Avenida 6 y calle 5, casa esquinera mano derecha Alajuela'], 30, 'el uso de mascarilla obligatorio']).
restaurante([kfc, rapida,[alajuela, 'Cruce calle 2 Obispo Tristán y avenida 10 Jesús Ocaña'],20,'el uso de mascarilla obligatorio']).
restaurante([matsuri, japonesa, [alajuela, 'Tropicana, Provincia de Alajuela, Alajuela'],20,'el uso de mascarilla obligatorio']).
restaurante([tacobell, mexicana, [alajuela, 'Provincia de Alajuela, Alajuela'], 30, 'el uso de mascarilla obligatorio']).
restaurante([meywah,china,[alajuela, 'Av. 3 Tomás Guardia, Provincia de Alajuela, Alajuela'],20,'el uso de mascarilla obligatorio']).

%Guanacaste
restaurante([mcdonald, rapida, [guanacaste, 'Carretera Interamericana Esq, Av. Central, Provincia de Guanacaste, Liberia'], 30, 'el uso de mascarilla obligatorio']).
restaurante([pizzahut, italiana, [guanacaste, 'Provincia de Guanacaste, Santa Cruz'], 20, 'el uso de mascarilla obligatorio']).
restaurante([reina, mariscos, [guanacaste, 'Provincia de Guanacaste, Curime'], 30, 'el uso de mascarilla obligatorio']).
restaurante([kfc, rapida,[guanacaste, 'Diagonal a Plaza Santa Rosa, Liberia, Guanacaste, Provincia de Guanacaste, Liberia'],20,'el uso de mascarilla obligatorio']).
restaurante([cos-ita, japonesa, [guanacaste, 'Parada de Buses Samara - San Jose, 160, Provincia de Guanacaste, Sámara'],20,'el uso de mascarilla obligatorio']).
restaurante([tacobell, mexicana, [guanacaste, 'Limonal, Provincia de Guanacaste, La Palma'], 30, 'el uso de mascarilla obligatorio']).
restaurante([yongxin,china,[guanacaste, 'Provincia de Guanacaste, Liberia'],20,'el uso de mascarilla obligatorio']).

%Limon
restaurante([mcdonald, rapida, [limon, 'Calle 8, Limon'], 30, 'el uso de mascarilla obligatorio']).
restaurante([pizzahut, italiana, [limon, 'Av 2, Limón'], 20, 'el uso de mascarilla obligatorio']).
restaurante([rancho, mariscos, [limon, 'Unnamed Road, Liman, Guapiles'], 30, 'el uso de mascarilla obligatorio']).
restaurante([kfc, rapida,[limon, 'Centro de Limon'],20,'el uso de mascarilla obligatorio']).
restaurante([tacobell, mexicana, [limon, 'Carr Braulio Carrillo, Limon, Guapiles'], 30, 'el uso de mascarilla obligatorio']).
restaurante([china-garden,china,[limon, 'Pocora, Limon, sobre ruta 32, 100 este de Almacenes El Colono de, Limon, Pocora'],20,'el uso de mascarilla obligatorio']).


%tipos menu
%formato: (tipo comida, [restaurantes], [sabores])
menu([hamburguesas, [mcdonald, tejarena, autogrill], [simple, conQueso, dobleTorta, vegana]]).
menu([hamburguesas, [autogrill], [artesanal]]).

menu([arroz,[hongkong,linfei,wingshun,china-garden,yongxin,meywah,lungfung],[chino,conPollo,cantones]]).

menu([pollo, [kfc,rostipollos], [crispy, frito]]).
menu([pollos, [kfc,rostipollos], [crispy, frito]]).

menu([pescado,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[fileteado, empanizado, crispy]]).
menu([pescados,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[fileteado, empanizado, crispy]]).

menu([ceviche,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[conLimon]]).
menu([ceviches,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[conLimon]]).

menu([taco, [tacobell, autogrill], [simple, doble]]).
menu([tacos, [tacobell, autogrill], [simple, doble]]).

menu([nacho, [tacobell, autogrill, tejarena], [simple, res, pollo, combinado]]).
menu([nachos, [tacobell, autogrill, tejarena], [simple, res, pollo, combinado]]).

menu([sushi, [yokohama,matsuri,cos-ita], [tempura, soba, domburi, niguiri]]).

menu([pizza, [pizzahut, woods], [hongos, jamon, hawaiana, brasilena, margarita]]).
menu([pizzas, [pizzahut, woods], [hongos, jamon, hawaiana, brasilena, margarita]]).

menu([calzone, [pizzahut, woods], [tomate, pina]]).
menu([calzones, [pizzahut, woods], [tomate, pina]]).

menu([papa, [mcdonald, tejarena], [grandes, pequenas]]).
menu([papas, [mcdonald, tejarena], [grandes, pequenas]]).

bebida([cocaCola, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena]]).
bebida([sevenUp, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena]]).
bebida([uva, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena]]).
bebida([pepsi, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena]]).
bebida([fresa, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena]]).
bebida([cas, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena]]).
bebida([cerveza, [yokohama, woods]]).
bebida([artesanal, [yokohama, woods]]).
bebida([vino, [yokohama, woods]]).
bebida([sangria, [yokohama, woods]]).



%******************************************************************************
%******************************************************************************
%******************************************************************************



miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).

% Se buscan los restaurantes 
listaRestaurantes(L) :- findall(X, (restaurante([X|_])), L).

compareRest([],X):- X = 'no hay restaurante'.
compareRest([H|_], X):- listaRestaurantes(L),
		                miembro(H,L) , X = H, !.
compareRest([H|T], X):- listaRestaurantes(L),
	                    \+miembro(H,L),
	                    compareRest(T,X).

% Se buscan las comidas
listaComidas(L) :- findall(X, (menu([X|_])), L).

compareComida([],X):- X = 'no hay comida'.
compareComida([H|_], X):- listaComidas(L),
		                  miembro(H,L) , X = H, !.
compareComida([H|T], X):- listaComidas(L),
	                      \+miembro(H,L),
	                      compareComida(T,X).

% Se buscan los refrescos
listaBebidas(L) :- findall(X, (bebida([X|_])) , L).

compareBebida([],X):- X = 'no hay refresco'.
compareBebida([H|_], X):- listaBebidas(L),
		                  miembro(H,L) , X = H, !.
compareBebida([H|T], X):- listaBebidas(L),
	                      \+miembro(H,L),
	                      compareBebida(T,X).

% Se buscan los lugares
listaLugar(L) :- findall(X, (restaurante([_,_,X,_,_])) , L).

compareLugarAux([H|_], X):- listaLugar(L),
						    flatten(L,Y),
						    X = Y.

compareLugar([],X):- X = 'no hay lugar'.
compareLugar([H|_], X):- compareLugarAux(C, Y),
		                 miembro(H,Y) , X = H, !.
compareLugar([H|T], X):- compareLugarAux(C, Y),
	                     \+miembro(H,Y),
	                     compareLugar(T,X).

% Se busca la cantidad
listaCantidad(Cantidad) :- numlist(1, 40, Cs),
							atom_codes(A,Cs),
							atomic_list_concat(Cantidad,' ',A).	




compareCantidad([],X):- X = 'no hay campo'.
compareCantidad([H|_], X):- listaCantidad(L), writeln(L), writeln(H),
		                    miembro(H,L) , X = H, !.
compareCantidad([H|T], X):- listaCantidad(L),
	                        \+miembro(H,L),
	                        compareCantidad(T,X).


start(Y):-
    %input_to_list(Oracion),
    oracion(X,[]),
    Y = X.

iniciar(A,B,C,D,E):- 
	/*
	% Se busca el restaurante
	input_to_list(Oracion),
    compareRest(Oracion, Rest),
	A = Rest,
	% Se busca la comida
	input_to_list(Oracion2),
	compareComida(Oracion2, Comida),
	B = Comida,
	% Se busca la bebida
	input_to_list(Oracion3),
	compareBebida(Oracion3, Bebida),
	C = Bebida,
	% Se busca el lugar
	input_to_list(Oracion4),
	compareLugar(Oracion4, Lugar),
	D = Lugar,
	*/
	% Se busca la cantidad
	input_to_list(Oracion5),
	compareCantidad(Oracion5, Cantidad),
	E = Cantidad.





%******************************************************************************
%******************************************************************************
%******************************************************************************


%aperaciones basicas
%miembro(X,[X|_]).
%miembro(X,[_|T]):-miembro(X,T).

%busca el mejor restaurante segun los parametros dados.
buscarResta(TipoMenu, Comida, TipoComida, Lugar, Capacidad):-
    restaurante(Nombre, TipoMenu, [Lugar | Direccion], RCapacidad, Obligaciones),
    Capacidad =< RCapacidad,
    menu(Comida, LNombre, T),
    miembro(Nombre, LNombre),
    miembro(TipoComida, T),
    crearReferencia(Nombre, Direccion, Obligaciones), !,
    write('No se encuentra ningun restaurante con estas caracteristicas :(. Lo sentimos').

crearReferencia(Nombre, Direccion, Obligaciones):-
    write('Nuestra sugerencia es '),
    write(Nombre),
    write( '. Su direccion es: '),
    write(Direccion),
    write('. Tenga en cuenta que para ingresar '),
    write(Obligaciones).

inicio():-
    write('Menu'), nl, read(M),
    write('Comida'), nl, read(Co), 
    write('Tipo Comida'), nl, read(TC),
    write('Lugar'), nl, read(L), 
    write('Cantidad'), nl, read(C), 
    buscarResta(M , Co, TC, L, C), !.



%************************************************
%************************************************


?- write(' '),nl.
?- write('Sistema desarrollado por: angelortizv, isolis2000, jesquivel48'),nl.
?- write('Inserte inicio(). para iniciar con el sistema experto.'),nl,nl.

