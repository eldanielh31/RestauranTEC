% restaurantecDataBase.pl -- archivo secundario que contiene la base de datos del sistema experto.

%******************************************************************************
%**************************    Restaurantes    ********************************
%******************************************************************************

% Descripción		:	Restaurantes disponibles en la base de datos
% Nombre de Hecho	:	restaurante([A,B,[C,D],E,F])
% Formato           :   [nombre, tipoMenu, [provincia,direccion], capacidad, [disposiciones]]

%San Jose
restaurante([mcdonald, rapida, ['SanJose', 'Plaza del Sol'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([woods, italiana, ['San', 'Jose', 'Curridabat'], 20, 'es obligatorio el uso de mascarilla']).
restaurante([tacobell, mexicana, ['San', 'Jose', 'San Pedro'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([kfc, rapida,['San', 'Jose', 'San Pedro'],20,'es obligatorio el uso de mascarilla']).
restaurante([marisqueando, mariscos,['San', 'Jose', 'Centro de Desamparados'],20,'es obligatorio el uso de mascarilla']).
restaurante([hongkong,china,['San', 'Jose', 'Moravia'],20,'es obligatorio el uso de mascarilla']).
restaurante([yokohama, japonesa, ['San', 'Jose', '200 mts al oeste de la Universidad de Costa Rica'], 20, 'es obligatorio el uso de mascarilla']).

%Cartago 
restaurante([autogrill, bar, ['Cartago', 'Mall Paseo Metropoli'], 40, 'es obligatorio el uso de mascarilla']).
restaurante([mcdonald, rapida, ['Cartago', 'Metro Centro'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([pizzahut, italiana, ['Cartago', 'Ruinas de Cartago'], 5, 'es obligatorio el uso de mascarilla']).
restaurante([tejarena, rapida, ['Cartago', 'Tejar del Guarco'], 10, 'es obligatorio el uso de mascarilla']).
restaurante([tacobell, mexicana, ['Cartago', '100mts norte de las Ruinas'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([yokohama, japonesa, ['Cartago', 'Plaza Boulevard, Blvd. el Molino, Provincia de Cartago, Cartago'], 20, 'es obligatorio el uso de mascarilla']).
restaurante([kfc, rapida,['Cartago', '50 mts al sur de las Ruinas'],20,'es obligatorio el uso de mascarilla']).
restaurante([linfei,china,['Cartago', '200 mts al este de la Basilica de Los Angeles'],20,'es obligatorio el uso de mascarilla']).

%Puntarenas
restaurante([mcdonald, rapida, ['Puntarenas', 'Plaza Centenario'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([rostipollos, rapida, ['Puntarenas', 'Hotel Alamar'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([pizzahut, italiana, ['Puntarenas', 'Ocean Mall'], 20, 'es obligatorio el uso de mascarilla']).
restaurante([fishy, mariscos, ['Puntarenas', 'Parque Mora'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([kfc, rapida,['Puntarenas', 'Puntarenas Down Town'],20,'es obligatorio el uso de mascarilla']).
restaurante([yokohama, japonesa, ['Puntarenas', 'Puntarenas Down Town'],20,'es obligatorio el uso de mascarilla']).
restaurante([tacobell, mexicana, ['Puntarenas', 'Plaza La Rioja'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([wingshun,china,['Puntarenas', 'Centro de la capital'],20,'es obligatorio el uso de mascarilla']).

%Heredia
restaurante([mcdonald, rapida, ['Heredia', 'Oxigeno Human Playground'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([pizzahut, italiana, ['Heredia', 'Mall Paseo de Las Flores'], 20, 'es obligatorio el uso de mascarilla']).
restaurante([mariscosymas, mariscos, ['Heredia', 'Entrada principal de la UNA 100 oeste y 25 Sur'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([kfc, rapida,['Heredia', 'Mall Paseo de Las Flores'],20,'es obligatorio el uso de mascarilla']).
restaurante([yokohama, japonesa, ['Heredia', 'Contiguo Al AYA De San Pablo 112, Heredia, San Pablo'],20,'es obligatorio el uso de mascarilla']).
restaurante([tacobell, mexicana, ['Heredia', 'Mall Paseo de Las Flores'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([lungfung,china,['Heredia', 'Limón, Heredia'],20,'es obligatorio el uso de mascarilla']).

%Alajuela
restaurante([mcdonald, rapida, ['Alajuela', 'Diagonal Bomba Provincia de Alajuela La Tropicana, Provincia de Alajuela'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([pizzahut, italiana, ['Alajuela', 'Centro de Alajuela'], 20, 'es obligatorio el uso de mascarilla']).
restaurante([limonta, mariscos, ['Alajuela', 'Avenida 6 y calle 5, casa esquinera mano derecha Alajuela'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([kfc, rapida,['Alajuela', 'Cruce calle 2 Obispo Tristán y avenida 10 Jesús Ocaña'],20,'es obligatorio el uso de mascarilla']).
restaurante([matsuri, japonesa, ['Alajuela', 'Tropicana, Provincia de Alajuela, Alajuela'],20,'es obligatorio el uso de mascarilla']).
restaurante([tacobell, mexicana, ['Alajuela', 'Centro de Alajuela, Alajuela'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([meywah,china,['Alajuela', 'Av. 3 Tomás Guardia, Provincia de Alajuela, Alajuela'],20,'es obligatorio el uso de mascarilla']).

%Guanacaste
restaurante([mcdonald, rapida, ['Guanacaste', 'Carretera Interamericana Esq, Av. Central, Provincia de Guanacaste, Liberia'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([pizzahut, italiana, ['Guanacaste', 'Provincia de Guanacaste, Santa Cruz'], 20, 'es obligatorio el uso de mascarilla']).
restaurante([reina, mariscos, ['Guanacaste', 'Provincia de Guanacaste, Curime'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([kfc, rapida,['Guanacaste', 'Diagonal a Plaza Santa Rosa, Liberia, Guanacaste, Provincia de Guanacaste, Liberia'],20,'es obligatorio el uso de mascarilla']).
restaurante([cos-ita, japonesa, ['Guanacaste', 'Parada de Buses Samara - San Jose, 160, Provincia de Guanacaste, Sámara'],20,'es obligatorio el uso de mascarilla']).
restaurante([tacobell, mexicana, ['Guanacaste', 'Limonal, Provincia de Guanacaste, La Palma'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([yongxin,china,['Guanacaste', 'Provincia de Guanacaste, Liberia'],20,'es obligatorio el uso de mascarilla']).

%Limon
restaurante([mcdonald, rapida, ['Limon', 'Calle 8, Limon'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([pizzahut, italiana, ['Limon', 'Av 2, Limón'], 20, 'es obligatorio el uso de mascarilla']).
restaurante([rancho, mariscos, ['Limon', 'Unnamed Road, Limon, Guapiles'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([kfc, rapida,['Limon', 'Centro de Limon'],20,'es obligatorio el uso de mascarilla']).
restaurante([tacobell, mexicana, ['Limon', 'Sobre carretera Braulio Carrillo, Guapiles, Limon'], 30, 'es obligatorio el uso de mascarilla']).
restaurante([china-garden,china,['Limon', 'Pocora, Limon, sobre ruta 32, 100 este de Almacenes El Colono de, Limon, Pocora'],20,'es obligatorio el uso de mascarilla']).


%******************************************************************************
%******************************************************************************
%******************************************************************************



%******************************************************************************
%**************************    Tipo de Menu    ********************************
%******************************************************************************

% Descripción		:	Tipos de menu disponibles en la base de datos
% Nombre de Hecho	:	menu([A,[C,D],[_])
% Formato           :   (tipoComida, [restaurantesDisponibles], [saboresDisponibles])


menu([hamburguesa, [mcdonald, tejarena, autogrill], [simple, queso, doble, vegana]]).
menu([hamburguesa, [autogrill], [artesanal]]).
menu([hamburguesas, [mcdonald, tejarena, autogrill], [simple, conQueso, doble, vegana]]).
menu([hamburguesas, [autogrill], [artesanal]]).

menu([arroz,[hongkong,linfei,wingshun,china-garden,yongxin,meywah,lungfung],[chino,pollo,cantones]]).

menu([pollo, [kfc,rostipollos], [crispy, frito]]).
menu([pollos, [kfc,rostipollos], [crispy, frito]]).

menu([pescado,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[fileteado, empanizado, crispy]]).
menu([pescados,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[fileteado, empanizado, crispy]]).

menu([ceviche,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[limon]]).
menu([ceviches,[fishy,marisqueando,mariscosymas,limonta,reina,rancho],[limon]]).

menu([taco, [tacobell, autogrill], [simple, doble]]).
menu([tacos, [tacobell, autogrill], [simple, doble]]).

menu([nacho, [tacobell, autogrill, tejarena], [simple, res, pollo, combinado]]).
menu([nachos, [tacobell, autogrill, tejarena], [simple, res, pollo, combinado]]).

menu([sushi, [yokohama,matsuri,cos-ita], [tempura, soba, domburi, niguiri]]).

menu([pizza, [pizzahut, woods], [hongos, pepperoni, jamon, hawaiana, brasilena, margarita]]).
menu([pizzas, [pizzahut, woods], [hongos, pepperoni, jamon, hawaiana, brasilena, margarita]]).

menu([calzone, [pizzahut, woods], [tomate, pina]]).
menu([calzones, [pizzahut, woods], [tomate, pina]]).

menu([papa, [mcdonald, tejarena], [grandes, pequenas]]).
menu([papas, [mcdonald, tejarena], [grandes, pequenas]]).


%******************************************************************************
%******************************************************************************
%******************************************************************************





%******************************************************************************
%**************************    Tipo de Bebida    ******************************
%******************************************************************************

% Descripción		:	Tipos de bebida disponibles en la base de datos
% Nombre de Hecho	:	bebida([A,[B,C])
% Formato           :   (tipoBebida, [restaurantesDisponibles]) 

bebida([cocacola, [tacobell, yokohama, woods, pizzahut, mcdonald, tejarena]]).
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

