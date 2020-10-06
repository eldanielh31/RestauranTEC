%Restaurantes disponibles en la app
%formato: [nombre, tipoMenu, [menu], [direccion], [capacidad], [disposiciones]]
Terrestaurante(mcdonald, comida_rapida, [sanjose, 'metrocentro'], 4, 'el uso de mascarilla obligatorio').
restaurante(pizzahut, italiana, [cartago, 'ruinar'], 5, 'el uso de mascarilla obligatorio').
restaurante(tejarena, comida_rapida, [cartago, 'tejar'], 10, 'el uso de mascarilla obligatorio').
restaurante(woods, italiana, [sanjose, 'curridabat'], 20, 'el uso de mascarilla obligatorio').

%tipos menu
%formato: (tipo comida, [sabores])
menu(hamburguesas, [mcdonald, tejarena], [conQueso, dobleTorta, vegana]).

menu(pizza, [pizzahut], [hongos, jamon]).
menu(pizza, [woods], [hongos, jamon, hawaiana]).

menu(calzone, [pizzahut, woods], [tomate, pina]).

menu(papas, [mcdonald, tejarena], [grandes, pequenas]).

menu(refresco, [woods, pizzahut, mcdonald, tejarena], [cocaCola, sevenUp, uva]).

%aperaciones basicas
miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).

%busca el mejor restaurante segun los parametros dados.
buscarResta(TipoMenu, Comida, TipoComida, Lugar, Capacidad):-
    restaurante(Nombre, TipoMenu, [Lugar | Direccion], RCapacidad, Obligaciones),
    Capacidad =< RCapacidad,
    menu(Comida, LNombre, T),
    miembro(Nombre, LNombre),
    miembro(TipoComida, T),
    crearReferencia(Nombre, Direccion, Obligaciones), !.

crearReferencia(Nombre, Direccion, Obligaciones):-
    write('Nuestra sugerencia es '),
    write(Nombre),
    write( '. Su direccion es: '),
    write(Direccion),
    write('. Tenga en cuenta que para ingresar debe tener en cuenta que '),
    write(Obligaciones).

inicio():-
    %write('Escribe'),
    %nl,
    %read(A),
    buscarResta(comida_rapida , hamburguesas, conQueso, cartago, 10),
