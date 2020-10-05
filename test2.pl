
%Restaurantes disponibles en la app
%formato: [nombre, tipoMenu,[direccion], [capacidad], [disposiciones]]
restaurante(mcdonald, comida_rapida, [cartago, 'metrocentro'], 30, 'Uso de mascarilla obligatorio').

%tipos menu
%formato: (tipo comida, [sabores])
menu(hamburguesas, mcdonald).
menu(hamburguesas, tejarena).

menu(papas, mcdonald).
menu(papas, bk).

menu(refresco, mcdonald).

hamburguesas('con queso', mcdonald).

buscarResta(TipoMenu, Comida, Lugar, Capacidad, N, D):-
    restaurante(Nombre, TipoMenu, [Lugar | Direccion], Capacidad, Obligaciones),
    menu(Comida, Nombre),
    N = Nombre,
    D = Direccion.

inicio():-
    write('Escribe'),
    nl,
    read(A),
    buscarResta(A, hamburguesas, cartago, 30, Nombre, Direccion),
    write(Nombre),nl,
    write(Direccion).