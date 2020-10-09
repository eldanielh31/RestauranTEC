%Restaurantes disponibles en la app
%formato: [nombre, tipoMenu, [direccion], [capacidad], [disposiciones]]
restaurante(mcdonald, comida_rapida, [sanjose, 'Plaza del Sol'], 30, 'el uso de mascarilla obligatorio').
restaurante(woods, italiana, [sanjose, 'curridabat'], 20, 'el uso de mascarilla obligatorio').
restaurante(tacobell, mexicana, [sanjose, 'San Pedro'], 30, 'el uso de mascarilla obligatorio').

restaurante(autogrill, bar, [cartago, 'Mall Paseo Metropoli'], 40, 'el uso de mascarilla obligatorio').
restaurante(mcdonald, comida_rapida, [cartago, 'MetroCentro'], 30, 'el uso de mascarilla obligatorio').
restaurante(pizzahut, italiana, [cartago, 'ruinas'], 5, 'el uso de mascarilla obligatorio').
restaurante(tejarena, comida_rapida, [cartago, 'tejar'], 10, 'el uso de mascarilla obligatorio').
restaurante(tacobell, mexicana, [cartago, '100mts norte de las ruinas'], 30, 'el uso de mascarilla obligatorio').
restaurante(yokohama, japonesa, [cartago, 'Plaza Boulevard, Blvd. el Molino, Provincia de Cartago, Cartago'], 20, 'el uso de mascarilla obligatorio').

%tipos menu
%formato: (tipo comida, [restaurantes], [sabores])
menu(hamburguesas, [mcdonald, tejarenam, autogrill], [simple, conQueso, dobleTorta, vegana]).
menu(hamburguesas, [autogrill], [artesanal]).
menu(hamburguesa, [mcdonald, tejarenam, autogrill], [simple, conQueso, dobleTorta, vegana]).
menu(hamburguesa, [autogrill], [artesanal]).

menu(tacos, [tacobell, autogrill], [simple, doble]).

menu(nachos, [tacobell, autogrill, tejarena], [simple, res, pollo, combinado]).

menu(sushi, [yokohama], [tempura, soba, domburi, niguiri]).

menu(pizza, [pizzahut, woods], [hongos, jamon, hawaiana, brasilena, margarita]).

menu(calzone, [pizzahut, woods], [tomate, pina]).

menu(papas, [mcdonald, tejarena], [grandes, pequenas]).

menu(refresco, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena], [cocaCola, sevenUp, uva, pepsi, fresa, cas]).
menu(refresco, [yokohama, woods], [cerveza, cervezaArtesanal, vino, sangria]).

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
    write('. Tenga en cuenta que para ingresar '),
    write(Obligaciones).

inicio():-
    write('Menu'), nl, read(M),
    write('Comida'), nl, read(Co), 
    write('Tipo Comida'), nl, read(TC),
    write('Lugar'), nl, read(L), 
    write('Cantidad'), nl, read(C), 
    buscarResta(M , Co, TC, L, C), !.
