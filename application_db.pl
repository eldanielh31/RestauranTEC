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

% Palabras Clave de Usuario ---------------------------------------------------------------------------------------------------------------

% Descripción		:	Inicio de una Conversacion
% Nombre de Hecho	:	saludo([X])
% Parámetro			: 	palabra clave de saludo 	
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
determinante_m([este|S],S).
determinante_m([estos|S],S).
determinante_m([nuestro|S],S).
determinante_m([otro|S],S).
determinante_m([alguno|S],S).
determinante_m([algunos|S],S).
determinante_m([del|S],S).

% Descripción		:	Determinantes femeninos
% Nombre de Hecho	:	determinante_f([X])
% Parámetro			:	determinantes femeninos
% Uso				:	sintagma_nominal([A],[B])
determinante_f([la|S],S).
determinante_f([las|S],S).
determinante_f([una|S],S).
determinante_f([unas|S],S).
determinante_f([esta|S],S).
determinante_f([estas|S],S).
determinante_f([nuestra|S],S).
determinante_f([otra|S],S).
determinante_f([alguna|S],S).
determinante_f([algunas|S],S).
determinante_f([mala|S],S).

% Descripción		:	Determinantes neutros
% Nombre de Hecho	:	determinante_n([X])
% Parámetro			:	determinantes neutros
% Uso				:	sintagma_nominal([A],[B])
determinante_n([mi|S],S).
determinante_n([mis|S],S).
determinante_n([posibles|S],S).

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
sustantivo_m([internet|S],S).
sustantivo_m([celular|S],S).
sustantivo_m([televisor|S],S).
sustantivo_m([archivos|S],S).
sustantivo_m([papel|S],S).
sustantivo_m([problema|S],S).

% Descripción		:	sustantivos femeninos
% Nombre de Hecho	:	sustantivo_f([X])
% Parámetro			:	sustantivos femeninos
% Uso				:	sintagma_nominal([A],[B])
sustantivo_f([computadora|S],S).
sustantivo_f([impresora|S],S).
sustantivo_f([imagen|S],S).
sustantivo_f([referencia,para|S],S).
sustantivo_f([causas|S],S).

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
verbo([sirve|S],S).
verbo([me,sirve|S],S).
verbo([funciona|S],S).
verbo([funcione|S],S).
verbo([se,descompuso|S],S).
verbo([se,rompio|S],S).
verbo([enciende|S],S).
verbo([se,enciende|S],S).
verbo([me,enciende|S],S).
verbo([se,conecta|S],S).
verbo([suena|S],S).
verbo([se,ve|S],S).
verbo([ocupo|S],S).
verbo([cambiar|S],S).
verbo([editar|S],S).
verbo([remover|S],S).
verbo([se,sobrecalienta|S],S).
verbo([se,le,atasca|S],S).
verbo([muestra|S],S).
verbo([esta,lento|S],S).
verbo([tiene|S],S).
verbo([tiene,conexion|S],S).
verbo([imprime|S],S).


% Lista de Problemas,Causas,Soluciones,Preguntas y Referencias ----------------------------------------------------------------------------

% Descripción		:	Lista de Problemas, los problemas son tratados como strings
% Nombre de Hecho	:	problema(A)
% Parámetro			:	problema reconocido por el SE
% Uso				:	causa(B,A), referencia(E,A), raiz(B,A)
problema('computadora no enciende').
problema('computadora no muestra mis archivos').
problema('impresora no imprime').
problema('impresora se le atasca el papel').
problema('internet no tiene conexion').
problema('internet esta lento').
problema('televisor no funciona').
problema('televisor tiene mala imagen').
problema('celular esta lento').

% Descripción		:	Lista de Causas para cada problema
% Nombre de Hecho	:	causa(B,A)
% Parámetro			:	(causa asociado a un determinado problema, problema al que pertenece)
% Uso				:	solucion(C,B), pregunta(D,B), raiz(B,A), causas(A)

% Causas relacionados a computadora
causa('la computadora esta desconectada','computadora no enciende').
causa('el tomacorriente asociado a la computadora no funciona','computadora no enciende').
causa('los cables no estan bien conectados','computadora no enciende').
causa('los archivos no existen','computadora no muestra mis archivos').
causa('la unidad de almacenamiento no esta bien conectada','computadora no muestra mis archivos').
causa('los controladores necesarios no estan instalados','computadora no muestra mis archivos').

% Causas relacionados a impresora
causa('la impresora no tiene papel','impresora no imprime').
causa('la impresora no tiene tinta','impresora no imprime').
causa('la impresora no esta bien configurada','impresora no imprime').
causa('el tamano de papel no es el correcto','impresora se le atasca el papel').
causa('el papel no esta bien alineado','impresora se le atasca el papel').
causa('el numero de hojas cargadas en la impresora es mayor a la capacidad','impresora se le atasca el papel').

% Causas relacionados a internet
causa('el modem esta desconectado','internet no tiene conexion').
causa('la computadora no tiene habilitada la tarjeta WIFI','internet no tiene conexion').
causa('el router no esta conectado al modem','internet no tiene conexion').
causa('su dispositivo no esta cerca del router','internet esta lento').
causa('el dispositivo tiene muchas aplicaciones corriendo simultaneamente','internet esta lento').
causa('la capacidad de internet es inferior a 5MB','internet esta lento').

% Causas relacionados a televisor
causa('el televisor no esta conectado','televisor no funciona').
causa('el control remoto no tiene bateria','televisor no funciona').
causa('el interruptor de ahorro de energia del televisor no esta encedido','televisor no funciona').
causa('el tomacorriente asociado al televisor  no funciona','televisor no funciona').
causa('el televisor no esta bien conectado al cable o antena','televisor tiene mala imagen').
causa('el televisor no tiene la actualizacion mas reciente','televisor tiene mala imagen').
causa('hay dispositivos cerca que brindan interferencia del televisor','televisor tiene mala imagen').

% Causas relacionados a celular
causa('el celular ocupa un reinicio', 'celular esta lento').
causa('el celular no tiene suficiente espacio de almacenamiento','celular esta lento').
causa('su celular no esta actualizado','celular esta lento').

% Descripción		:	Lista de soluciones para cada subproblema
% Nombre de Hecho	:	solucion(C,B)
% Parámetro			:	(solucion asociada a una causa, causa a la que pertenece)
% Uso				:	consulta_caso_base(B), conversacion(Oracion)

% Soluciones relacionadas a computadora
solucion('conecte la computadora','la computadora esta desconectada').
solucion('intente con otro tomacorriente','el tomacorriente asociado a la computadora no funciona').
solucion('conecte bien los cables','los cables no estan bien conectados').
solucion('los archivos no existen','los archivos no existen').
solucion('conecte bien la unidad de almacenamiento','la unidad de almacenamiento no esta bien conectada').
solucion('instale los controladores necesarios','los controladores necesarios no estan instalados').

% Soluciones relacionadas a impresora
solucion('ponga papel en la impresora','la impresora no tiene papel').
solucion('inserte tinta el la impresora','la impresora no tiene tinta').
solucion('configure correctamente la impresora','la impresora no esta bien configurada').
solucion('utilice tamano de papel correcto','el tamano de papel no es el correcto').
solucion('alinee el papel','el papel no esta bien alineado').
solucion('disminuya la cantidad de hojas','el numero de hojas cargadas en la impresora es mayor a la capacidad').

% Soluciones relacionadas a internet
solucion('conecte el modem','el modem esta desconectado').
solucion('habilite la tarjeta WI-FI o conecte la computadora con un cable de red','la computadora no tiene habilitada la tarjeta WIFI').
solucion('conecte el router al modem','el router no esta conectado al modem').
solucion('acerque el router al dispositivo','su dispositivo no esta cerca del router').
solucion('cierre unas cuentas aplicaciones','el dispositivo tiene muchas aplicaciones corriendo simultaneamente').
solucion('aumente su bando','la capacidad de internet es inferior a 5MB').

% Soluciones relacionadas a televisor
solucion('conecte el televisor','el televisor no esta conectado').
solucion('cambie las baterias del control','el control remoto no tiene bateria').
solucion('encienda el interruptor','el interruptor de ahorro de energia del televisor no esta encendido').
solucion('cambie el toma que esta usando','el tomacorriente asociado al televisor  no funciona').
solucion('conecte correctamente el televisor al cable o la antena','el televisor no esta bien conectado al cable o antena').
solucion('instale la actualizacion mas reciente del televisor','el televisor no tiene la actualizacion mas reciente').
solucion('aleje los dispositivos del televisor','hay dispositivos cerca que brindan interferencia del televisor').

% Soluciones relacionadas a celular
solucion('reinicie el celular','el celular ocupa un reinicio').
solucion('borre algunas aplicaciones, fotos, videos o archivos','el celular no tiene suficiente espacio de almacenamiento').
solucion('busque la ultima version, revise si es compatible ','el celular no esta actualizado').

% Solucion para caso base
solucion('Este Sistema Experto no puede responder su problema, se recomienda consultar a un tecnico.','no hay solucion').

% Descripción		:	Lista de Preguntas para cada subproblema
% Nombre de Hecho	:	pregunta(D,B)
% Parámetro			:	(pregunta asociada a una causa, causa a la que pertenece)
% Uso				:	hoja_izquierda(B)

% Preguntas relacionados a computadora
pregunta('¿Reviso que este conectada la computadora?','la computadora esta desconectada').
pregunta('¿Ha intentado probar en otro enchufe?','el tomacorriente asociado a la computadora no funciona').
pregunta('¿Los cables estan bien conectados?','los cables no estan bien conectados').
pregunta('¿Ya se aseguro de que el archivo que esta buscando existe?','los archivos no existen').
pregunta('¿Ya se aseguro de que la unidad de almacenamiento esta bien conectada?','la unidad de almacenamiento no esta bien conectada').
pregunta('¿ya instalo los controladores necesarios?','los controladores necesarios no estan instalados').

% Preguntas relacionadas a impresora
pregunta('¿Ya reviso que tenga papel?','la impresora no tiene papel').
pregunta('¿Ya reviso que tenga tinta?','la impresora no tiene tinta').
pregunta('¿Ya se aseguro de que este bien configurada?','la impresora no esta bien configurada').
pregunta('¿El tamano de papel es el correcto?','el tamano de papel no es el correcto').
pregunta('¿El papel esta bien alineado?','el papel no esta bien alineado').
pregunta('¿El numero de hojas cargada en la impresora es mayor a la capacidad?','el numero de hojas cargadas en la impresora es mayor a la capacidad').

% Preguntas relacionadas a internet
pregunta('¿El modem esta conectado?','el modem esta desconectado').
pregunta('¿La computadora tiene habilitada la tarjeta WI-FI o esta conectada por cable?','la computadora no tiene habilitada la tarjeta WIFI').
pregunta('¿El router esta conectada al modem?','el router no esta conectado al modem').
pregunta('¿El router esta cerca del dispositivo que esta usando?','su dispositivo no esta cerca del router').
pregunta('¿El dispositivo no tiene muchas aplicaciones que estan utilizando internet simultaneamente?','el dispositivo tiene muchas aplicaciones corriendo simultaneamente').
pregunta('¿El internet es mayor a 5 MB?','la capacidad de internet es inferior a 5MB').

% Preguntas relacionadas a televisor
pregunta('¿El televisor esta conectado?','el televisor no esta conectado').
pregunta('¿El control remoto tiene bateria?','el control remoto no tiene bateria').
pregunta('¿El interruptor de ahorro de energia del televisor esta encendido?','el interruptor de ahorro de energia del televisor no esta encendido').
pregunta('¿Ha intentado cambiar el toma al que esta conectado el televisor?','el tomacorriente asociado al televisor  no funciona').
pregunta('¿El televisor esta bien conectado ya sea a cable o a antena?','el televisor no esta bien conectado al cable o antena').
pregunta('¿Su televisor tiene la actualizacion mas reciente?','el televisor no tiene la actualizacion mas reciente').
pregunta('¿Hay dispositivos cerca del televisor?','hay dispositvos cerca que brindan interferencia del televisor').

% Preguntas relacionadas a celular
pregunta('¿Ya probo apagandolo y volviendolo a encender?','el celular ocupa un reinicio').
pregunta('¿Verifico que aun tenga suficiente espacio de almacenamiento?','el celular no tiene suficiente espacio de almacenamiento').
pregunta('¿Su celular tiene la actualizacion mas reciente?','el celular no esta actualizado').

% Descripción		:	Lista de referencias para cada subproblema
% Nombre de Hecho	:	referencia(E,A)
% Parámetro			:	(link de internet para un determinado problema, problema al que pertenece)
% Uso				:	referencias(A)

% Referencias relacionadas a computadora
referencia('https://www.aboutespanol.com/soluciones-si-tu-pc-no-arranca-no-prende-o-no-enciende-3507940','computadora no enciende').
referencia('https://www.pcworld.es/tutoriales/ordenadores/ordenador-no-arranca-arreglar-3680944/','computadora no enciende').
referencia('https://www.diarioinformacion.com/vida-y-estilo/tecnologia/2016/04/15/pc-arranca-debo/1750203.html','computadora no enciende').
referencia('https://es.easeus.com/data-recovery-solution/pen-drive-not-showing-data-files.html','computadora no muestra mis archivos').
referencia('https://www.adslzone.net/windows-10/como-recuperar-archivos-pendrive-usb-aparece-vacio/','computadora no muestra mis archivos').
referencia('http://es.rescuedigitalmedia.com/resuelto-pen-drive-se-muestra-vacio-incluso-cuando-existen-datos','computadora no muestra mis archivos').

% Referencias relacionadas a impresora
referencia('https://www.pcworld.es/tutoriales/otros-dispositivos/impresora-no-funciona-problemas-impresion-3675127/','impresora no imprime').
referencia('http://support.epson-europe.com/onlineguides/es/sc86/ref_g/trble_5.htm','impresora no imprime').
referencia('https://support.hp.com/py-es/document/c00007100','impresora se le atasca el papel').
referencia('http://support.ricoh.com/bb_v1oi/pub_e/oi_view/0001054/0001054646/view/manual/int/0143.htm','impresora se le atasca el papel').
referencia('https://support.brother.com/g/b/faqend.aspx?c=es&lang=es&prod=dcp340cw2_eu&faqid=faq00000495_003','impresora se le atasca el papel').

% Referencias relacionadas a internet
referencia('https://www.adslzone.net/2015/06/17/que-hacer-cuando-no-funciona-tu-conexion-a-internet/','internet no tiene conexion').
referencia('https://tecnologia-informatica.com/tipos-conexion-internet/','internet no tiene conexion').
referencia('https://computerhoy.com/noticias/internet/que-me-va-lento-internet-problemas-habituales-internet-como-solucionarlos-73007','internet esta lento').
referencia('http://www.gadae.com/blog/5-motivos-por-los-que-puede-que-internet-va-lento/','internet esta lento').
referencia('https://www.testdevelocidad.es/causas-conexion-internet-mas-lenta/','internet esta lento').

% Referencias relacionadas a televisor
referencia('https://www.sony.es/electronics/support/articles/00094960','televisor no funciona').
referencia('https://www.lg.com/es/posventa/guias-y-soluciones/television/pantalla-negro-no-enciende-no-senal','televisor no funciona').
referencia('https://www.sony.es/electronics/support/articles/00094960','televisor tiene mala imagen').
referencia('https://www.lg.com/es/posventa/guias-y-soluciones/television/pantalla-negro-no-enciende-no-senal','televisor tiene mala imagen').

% Referencias relacionadas a celular
referencia('https://www.bbc.com/mundo/noticias/2015/03/150326_tecnologia_recomendaciones_para_un_celular_lento_kv','celular esta lento').
referencia('http://www.androidjefe.com/celular-lento/','celular esta lento').

% Descripción		:	Árbol de decisión, lectura de respuestas por parte del usuario.
% Nombre de Hecho	:	raiz(B,A)
% Parámetro			:	(causa asociado a un determinado problema, problema al que pertenece)
% Uso				:	conversacion()

% Árbol de decision referente a causas y problemas asociadas a computadora
raiz('la computadora esta desconectada','computadora no enciende'):-
	hoja_izquierda('la computadora esta desconectada'), !.
raiz('el tomacorriente asociado a la computadora no funciona','computadora no enciende'):-
	hoja_izquierda('el tomacorriente asociado a la computadora no funciona'), !.
raiz('los cables no estan bien conectados','computadora no enciende'):-
	hoja_izquierda('los cables no estan bien conectados'), !.
raiz('no hay solucion','computadora no enciende'):-
	consulta_caso_base('no hay solucion'), !.
raiz('los archivos no existen','computadora no muestra mis archivos'):-
	hoja_izquierda('los archivos no existen'), !.
raiz('la unidad de almacenamiento no esta bien conectada','computadora no muestra mis archivos'):-
	hoja_izquierda('la unidad de almacenamiento no esta bien conectada'), !.
raiz('los controladores necesarios no estan instalados','computadora no muestra mis archivos'):-
	hoja_izquierda('los controladores necesarios no estan instalados'), !.
raiz('no hay solucion','computadora no muestra mis archivos'):-
	consulta_caso_base('no hay solucion'), !.

% Árbol de decision referente a causas y problemas asociadas a impresora
raiz('la impresora no tiene papel','impresora no imprime'):-
	hoja_izquierda('la impresora no tiene papel'), !.
raiz('la impresora no tiene tinta','impresora no imprime'):-
	hoja_izquierda('la impresora no tiene tinta'), !.
raiz('la impresora no esta bien configurada','impresora no imprime'):-
	hoja_izquierda('la impresora no esta bien configurada'), !.
raiz('no hay solucion','impresora no imprime'):-
	consulta_caso_base('no hay solucion'), !.
raiz('el tamano de papel no es el correcto','impresora se le atasca el papel'):-
	hoja_izquierda('el tamano de papel no es el correcto'), !.
raiz('el papel no esta bien alineado','impresora se le atasca el papel'):-
	hoja_izquierda('el papel no esta bien alineado'), !.
raiz('el numero de hojas cargadas en la impresora es mayor a la capacidad','impresora se le atasca el papel'):-
	hoja_izquierda('el numero de hojas cargadas en la impresora es mayor a la capacidad'), !.
raiz('no hay solucion','impresora se le atasca el papel'):-
	consulta_caso_base('no hay solucion'), !.

% Árbol de decision referente a causas y problemas asociadas a internet
raiz('el modem esta desconectado','internet no tiene conexion'):-
	hoja_izquierda('el modem esta desconectado'), !.
raiz('la computadora no tiene habilitada la tarjeta WIFI','internet no tiene conexion'):-
	hoja_izquierda('la computadora no tiene habilitada la tarjeta WIFI'), !.
raiz('el router no esta conectado al modem','internet no tiene conexion'):-
	hoja_izquierda('el router no esta conectado al modem'), !.
raiz('no hay solucion','internet no tiene conexion'):-
	consulta_caso_base('no hay solucion'), !.
raiz('su dispositivo no esta cerca del router','internet esta lento'):-
	hoja_izquierda('su dispositivo no esta cerca del router'), !.
raiz('el dispositivo tiene muchas aplicaciones corriendo simultaneamente','internet esta lento'):-
	hoja_izquierda('el dispositivo tiene muchas aplicaciones corriendo simultaneamente'), !.
raiz('la capacidad de internet es inferior a 5MB','internet esta lento'):-
	hoja_izquierda('la capacidad de internet es inferior a 5MB'), !.
raiz('no hay solucion','internet esta lento'):-
	consulta_caso_base('no hay solucion'), !.

% Árbol de decision referente a causas y problemas asociadas a televisor
raiz('el televisor no esta conectado','televisor no funciona'):-
	hoja_izquierda('el televisor no esta conectado'), !.
raiz('lel control remoto no tiene bateria','televisor no funciona'):-
	hoja_izquierda('el control remoto no tiene bateria'), !.
raiz('el interruptor de ahorro de energia del televisor no esta encedido','televisor no funciona'):-
	hoja_izquierda('el interruptor de ahorro de energia del televisor no esta encedido'), !.
raiz('el tomacorriente asociado al televisor  no funciona','televisor no funciona'):-
	hoja_izquierda('el tomacorriente asociado al televisor  no funciona'), !.
raiz('no hay solucion','televisor no funciona'):-
	consulta_caso_base('no hay solucion'), !.
raiz('el televisor no esta bien conectado al cable o antena','televisor tiene mala imagen'):-
	hoja_izquierda('el televisor no esta bien conectado al cable o antena'), !.
raiz('el televisor no tiene la actualizacion mas reciente','televisor tiene mala imagen'):-
	hoja_izquierda('el televisor no tiene la actualizacion mas recientee'), !.
raiz('hay dispositivos cerca que brindan interferencia del televisor','televisor tiene mala imagen'):-
	hoja_izquierda('hay dispositivos cerca que brindan interferencia del televisor'), !.
raiz('no hay solucion','televisor tiene mala imagen'):-
	consulta_caso_base('no hay solucion'), !.

% Árbol de decision referente a causas y problemas asociadas a celular
raiz('el celular ocupa un reinicio','celular esta lento'):-
	hoja_izquierda('el celular ocupa un reinicio'), !.
raiz('el celular no tiene suficiente espacio de almacenamiento','celular esta lento'):-
	hoja_izquierda('el celular no tiene suficiente espacio de almacenamiento'), !.
raiz('su celular no esta actualizado','celular esta lento'):-
	hoja_izquierda('su celular no esta actualizado'), !.
raiz('no hay solucion','celular esta lento'):-
	consulta_caso_base('no hay solucion'), !.