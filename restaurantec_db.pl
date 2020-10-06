%Restaurantes disponibles en la app
%formato: [nombre, tipoMenu, [menu], [direccion], [capacidad], [disposiciones]]
restaurante(mcdonald, comida_rapida, [sanjose, 'metrocentro'], 4, 'Uso de mascarilla obligatorio').
restaurante(pizzahut, italiana, [cartago, 'ruinar'], 5, 'Uso de mascarilla obligatorio').
restaurante(tejarena, comida_rapida, [cartago, 'tejar'], 10, 'Uso de mascarilla obligatorio').
restaurante(woods, italiana, [sanjose, 'curridabat'], 20, 'Uso de mascarilla obligatorio').

%Ejercicio5 (miembro en listas)
miembro(X,[X|_]).
miembro(X,[_|T]):-miembro(X,T).

%tipos menu
%formato: (tipo comida, [sabores])
menu(hamburguesas, mcdonald, [conQueso, dobleTorta, vegana]).
menu(hamburguesas, tejarena, [conQueso, dobleTorta, vegana]).

menu(pizza, pizzahut, [hongos, jamon]).
menu(pizza, woods, [hongos, jamon, hawaiana]).

menu(calzone, pizzahut, [tomate, pina]).
menu(calzone, woods, [tomate, pina, remolacha]).

menu(papas, mcdonald, [grandes, pequenas]).
menu(papas, tejarena, [grandes, pequenas]).

menu(refresco, woods, [cocaCola, sevenUp, uva]).
menu(refresco, pizzahut, [cocaCola, sevenUp, uva]).
menu(refresco, mcdonald, [cocaCola, sevenUp, uva]).
menu(refresco, tejarena, [cocaCola, sevenUp, uva]).

buscarResta(TipoMenu, Comida, TipoComida, Lugar, Capacidad, N, D):-
    restaurante(Nombre, TipoMenu, [Lugar | Direccion], Capacidad, Obligaciones),
    menu(Comida, Nombre, T),
    miembro(TipoComida, T),
    N = Nombre,
    D = Direccion.

inicio():-
    %write('Escribe'),
    %nl,
    %read(A),
    buscarResta(comida_rapida , hamburguesas, conQueso, cartago, 10, Nombre, Direccion),
    write(Nombre),nl,
    write(Direccion).
