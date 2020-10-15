
% restaurantecBNF.pl -- archivo secundario que contiene el BNF utilizado por el sistem experto.

:-style_check(-singleton).

%******************************************************************************
%******************************    BNF    *************************************
%******************************************************************************

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
determinante_n(['Nosotros'|S],S).
determinante_n([yo|S],S).
determinante_n(['Yo'|S],S).
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

verbo(['Quiero'|S],S).
verbo(['Queremos'|S],S).
verbo(['Quiero,comer'|S],S).
verbo(['Quiero,tomar'|S],S).
verbo(['Quiero,beber'|S],S).
verbo(['Queremos,comer'|S],S).
verbo(['Queremos,tomar'|S],S).
verbo(['Queremos,beber'|S],S).

verbo(['Quiero,estar,cerca,de'|S],S).
verbo(['Quiero,algo,cerca,de'|S],S).
verbo(['Quiero,estar,alrededor,de'|S],S).
verbo(['Quiero,algo,alrededor,de'|S],S).
verbo(['Queremos,estar,cerca,de'|S],S).
verbo(['Queremos,algo,cerca,de'|S],S).
verbo(['Queremos,estar,alrededor,de'|S],S).
verbo(['Queremos,algo,alrededor,de'|S],S).

verbo(['Seria'|S], S).
verbo(['Seriamos'|S], S).
verbo(['Somos'|S], S).


% Descripción		:	recibe una lista de palabras y una lista vacía y verifica si es una oración gramaticalmente 
%                       correcta según la estructura establecida
% Nombre de Regla	:	oracion([A],[B])
% Parámetro			:	lista para revisar y lista vacía
% Uso				:	se utiliza para validar oraciones

oracion(A,B):-
    sintagma_nominal(A,C).


% Descripción		:	recibe una lista de palabras y una lista vacía; elimina el primer sintagma nominal encontrado
%                       y devuelve el resto de las palabras
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


% Descripción		:	recibe una lista de palabras y una lista vacía; elimina el primer sintagma verbal encontrado
%                       y devuelve el resto de las palabras
% Nombre de Regla	:	sintagma_verbal([A],[B])
% Parámetro			:	lista a revisar y lista vacía
% Uso				:	se utiliza para encontrar el primer sintagma verbal en una lista de palabras

sintagma_verbal(A,B):-
	verbo(A,B).

% Descripción		:	valida si la oración digitada por el usuario está gramaticalmente correcta según el BNF establecido
% Nombre de Regla	:	validacion_gramatical()
% Parámetro			:	lista a revisar
% Uso				:	Se utiliza para verificar gramaticalmente una oración, de lo contrario, devolver un mensaje al usuario

validacion_gramatical(Oracion):-
    oracion(Oracion,[]),
	!.

validacion_gramatical(Oracion):-
	nl, writeln('Oracion gramaticalmente incorrecta'),
	writeln('Escriba de nuevo su oracion'),nl,
	input_to_list(Oracion2),
	validacion_gramatical(Oracion2).



%******************************************************************************
%******************************************************************************
%******************************************************************************

