%%%
% Autores:
%        Roberto Iglesias Castro
%        Alexandro Fortes Silva
%
%


% Definicion de predicados dinamicos
:- dynamic imposible/1.
:- dynamic mesas/1.
:- dynamic brazos/1.

% Definicion de operadores
:- op(200, xfx, /).
:- op(400, xfx, de).
:- op(300, xfx, en).
:- op(600, fy, el).
:- op(600, fy, coger).
:- op(700, xfx, coge).
:- op(700, xfx, deposita).
:- op(700, xfx, coge_en).
:- op(700, xfx, deposita_en).
:- op(800, xfx, hasta_encontrar).
:- op(800, xfx, sobre).
:- op(900, xf, cercano).
:- op(1000, fy, incompatible).

% Busqueda de un elemento X en una lista
% pertenece(b, [a,b,c]).
search(X, [X|_]) :-
	!.
search(X, [_|R]) :-
	search(X, R).

% Agregar un elemento X en la lista L
add(X, L, [X|L]).

% Buscar un elemento ELEMENTO en un monton MONTON de una mesa MESA en
% una lista de 3 niveles.
buscarMonton(0, 0, Elem, [Elem|_]).%(Numero Mesa,Numero Monton, Elemento a devolver, Lista de mesa.)
buscarMonton(0, 0, [], []).
buscarMonton(0, 1, Elem, [Cab|_]) :-
	buscarMonton(0, 0, Elem, Cab).
buscarMonton(0, Monton, Elem, [_|Res]) :-
	Monton > 1,
	Monton2 is Monton - 1,
	buscarMonton(0, Monton2, Elem, Res).
buscarMonton(1, Monton, Elem, [Cab|_]) :-
	buscarMonton(0, Monton, Elem, Cab),
	!.
buscarMonton(Mesa, Monton, Elem, [_|Res]) :-
	Mesa > 1,
	Mesa2 is Mesa - 1,
	buscarMonton(Mesa2, Monton, Elem, Res),
	!.

% Buscar un objecto X. Devuelve un array con las posiciones del elemento
% dentro del array de mesas. Ejemplo de salida
% [[[],[]] , [[PosMesa,PosMonton],[]]
searchObject([], _, _, _, []).
searchObject([X|_], X, TBL0, HP0, [TBL0, HP0]).
searchObject([[]|T], X, TBL0, _, Z) :-
	TBL is TBL0 + 1,
	HP is 0,
	searchObject(T, X, TBL, HP, Z).
searchObject([[H|T2]|T], X, TBL0, HP0, [A|Z]) :-
	HP is HP0 + 1,
	searchObject([T2|T], X, TBL0, HP, Z),
	(searchObject(H, X, TBL0, HP, A) ->
		true;
		A = [], !
	).

% Elimina el primer elemento del monton MONTON de la mesa MESA de una
% lista de 3 niveles
popMonton(0, 0, Elem, [Elem|Res], Res).
popMonton(0, 1, Elem, [Ca1|Res], [Ca2|Res]) :-
	popMonton(0, 0, Elem, Ca1, Ca2).
popMonton(0, Monton, Elem, [Ca|Res1], [Ca|Res2]) :-
	Monton > 1,
	Monton2 is Monton - 1,
	popMonton(0, Monton2, Elem, Res1, Res2).
popMonton(1, Monton, Elem, [Ca1|Res], [Ca2|Res]) :-
	popMonton(0, Monton, Elem, Ca1, Ca2),
	!.
popMonton(Mesa, Monton, Elem, [Ca|Res1], [Ca|Res2]) :-
	Mesa > 1,
	Mesa2 is Mesa - 1,
	popMonton(Mesa2, Monton, Elem, Res1, Res2),
	!.

% Agregar un elemento ELEM al monton MONTON de la mesa MESA de una lista
% de 3 niveles
pushMonton(0, 0, Elem, L, [Elem|L]).
pushMonton(0, 1, Elem, [Ca1|Res], [Ca2|Res]) :-
	pushMonton(0, 0, Elem, Ca1, Ca2).
pushMonton(0, Monton, Elem, [Ca|Res1], [Ca|Res2]) :-
	Monton2 is Monton - 1,
	pushMonton(0, Monton2, Elem, Res1, Res2).
pushMonton(1, Monton, Elem, [Ca1|Res], [Ca2|Res]) :-
	pushMonton(0, Monton, Elem, Ca1, Ca2),
	!.
pushMonton(Mesa, Monton, X, [Ca|Res1], [Ca|Res2]) :-
	Mesa2 is Mesa - 1,
	pushMonton(Mesa2, Monton, X, Res1, Res2),
	!.

% Substituir el valor numerico referente a la cercania al monton o la mesa
actualizarCercania(0, 1, Elem, [_|Res], [Elem|Res]).
actualizarCercania(0, Monton, Elem, [H|Res1], [H|Res2]) :-
	Monton2 is Monton - 1,
	actualizarCercania(0, Monton2, Elem, Res1, Res2).
actualizarCercania(1, Monton, Elem, [Ca1|Res], [Ca2|Res]) :-
	actualizarCercania(0, Monton, Elem, Ca1, Ca2),
	!.
actualizarCercania(Mesa, MN, Elem, [Ca|Res1], [Ca|Res2]) :-
	Mesa2 is Mesa - 1,
	actualizarCercania(Mesa2, MN, Elem, Res1, Res2),
	!.

% Configuracion inicial
conf(M, B, I) :-
	brazos(B),
	mesas(M),
	imposible(I).

% Lista de mesas
mesas([[/*M1*/  [cubo/azul, piramide/rojo],[esfera/blanco, cubo/blanco]],
	[/*M2*/  [piramide/azul, esfera/azul],[esfera/azul, piramide/blanco]] ]).

% Lista de brazos. Numero de brazo, Bolsa, Cercania a la mesa, Cercania
% al monton.
brazos([/*Brazo 1*/ [[], 1, 2 ],
     [/*Brazo 2*/  [], 2, 1 ]]
    ).

% Lista de incompatibles
imposible([]).

% Objeto
F/C :-
	forma(F),
	color(C).

% Colores
color(azul).
color(blanco).
color(rojo).

% Formas
forma(cubo).
forma(piramide).
forma(esfera).

% Buscar el primer par de elementos de cercania que coincidan en la mesa y
% monton
searchNearMesaMonton([[]], _, _, _) :- fail.
searchNearMesaMonton([[_, T, H]|Z], L, A0, RM, TBL, HP) :-
	A is A0 + 1,
	(search([T, H], L) ->
		RM is A,
		TBL is T,
		HP is H;
		searchNearMesaMonton(Z, L, A, RM, TBL, HP)
	).

% Buscar el primer par de elementos de cercania que coincidan en la mesa
searchNearMesa([[]], _, _, _) :- fail.
searchNearMesa([[_, T, _]|Z], L, A0, RM, TBL, HP) :-
	A is A0 + 1,
	(search([T, Y], L) ->
		RM is A,
		TBL is T,
		HP is Y;
		searchNearMesa(Z, L, A, RM, TBL, HP)
	).

% Buscar el primer par de elementos en la lista
searchNear([[TBL, HP]|_], RM, TBL, HP) :-
	RM is 1,
	!.
searchNear([[]|T], RM, TBL, HP) :-
	searchNear(T, RM, TBL, HP).

% Visualizar las listas principales
printImposible(L) :-
	nl,
	write('-------- INCOMPATIBILIDADES --------'), nl,
	nl,
	write(L),
	nl, nl.

printList([], _, _).
printList([Ca|Res], Nombre, Aux) :-
	N is Aux + 1,
	write(Nombre), write(N), write(': '),
	write(Ca), nl,
	printList(Res, Nombre, N).

printTable([], _, _).
printTable([Ca|Res], Nombre, N0) :-
	N is N0 + 1,
	write(Nombre),write(N),write(' -'),
	printList(Ca, '\tM', 0),
	printTable(Res, Nombre, N).

mostrarEntorno :-
	conf(LIST_TABLES, LIST_ARMS, LIST_IMPOSIBLE),
	printConf(LIST_TABLES, LIST_ARMS),
	printImposible(LIST_IMPOSIBLE),
	!.
printConf(LIST_TABLES, LIST_ARMS) :-
	nl,
	write('------- TABLERO --------'), nl,
	nl,
	printTable(LIST_TABLES, 'Mesa', 0),
	nl,
	write('------- ROBOT --------'), nl,
	nl,
	printList(LIST_ARMS, 'Brazo', 0),
	nl.

% Mesajes de errores
error(1, OBJECT_1, OBJECT_2) :-
	write('"'),
	write('ERROR: '),
	write(OBJECT_1),
	write('"'),
	write(' no insertado con '),
	write('"'),
	write(OBJECT_2),
	write('"'),
	write(' por que son elementos incompatibles'),
	nl, !, fail.

error(2, OBJECT) :-
	write('ERROR: '),
	write(OBJECT),
	write('" No se encuentra en la cima del montón'),
	nl, !, fail.

error(3, OBJECT) :-
	write('ERROR: '),
	write(OBJECT),
	write(' no existe'),
	write('"!'),
	nl, !, fail.

error(4, OBJECT) :-
	write('ERROR con: "'),
	write(OBJECT),
	write('",el montón está vacío'),
	nl, !, fail.

error(5, OBJECT) :-
	write('ERROR: '),
	write(OBJECT),
	write('" no depositado porque el brazo esta vacio'),
	nl, !, fail.

error(6, OBJECT) :-
	write('ERROR: '),
	write(OBJECT),
	write('"'),
	write(' no esta en la cima de la bolsa del brazo'),
	nl, !, fail.

error(7, OBJECT) :-
	write('ERROR: '),
	write(OBJECT),
	write(' no depositado porque la bolsa del brazo esta vacia'),
	nl, !, fail.

error(8, OBJECT) :-
	write('ERROR: '),
	write(OBJECT),
	write(' no esta en la cima de ningun monton'),
	nl, !, fail.

error(9, OBJECT) :-
	write('ERROR: '),
	write(OBJECT),
	write(' objeto con formato no válido (FORMA/COLOR)'),
	nl, !, fail.

error(10) :-
	write('ERROR: Esa incompatibilidad ya existe'),
	nl, !, fail.

% Actualizar listas principales (mesas, brazos e imposibles)
updateMesas(OLD_LIST, NEW_LIST) :-
	retract(mesas(OLD_LIST)),
	assert(mesas(NEW_LIST)).

updateBrazos(OLD_LIST, NEW_LIST) :-
	retract(brazos(OLD_LIST)),
	assert(brazos(NEW_LIST)).

updateImposible(OLD_LIST, NEW_LIST) :-
	retract(imposible(OLD_LIST)),
	assert(imposible(NEW_LIST)).

% Coge el objeto si esta en la cima del monton.
% Primero controla los errores, busca la cabeza
el BRAZO coge FORM/COLOR en MONTON de MESA :-
	(FORM/COLOR ->
		true;
		error(9, FORM/COLOR)/*Error de formato*/
	),
	conf(LISTA_MESAS, LISTA_BRAZOS, LISTA_INCOMPATIBLES),
	buscarMonton(BRAZO, 1, OBJ_BRAZO, LISTA_BRAZOS),
	(popMonton(MESA, MONTON, OBJ_MESA, LISTA_MESAS, NUEVA_LISTA_MESAS) ->
		true;
		error(4, FORM/COLOR)/*Error de monton vacio*/
	),
	(not(search([OBJ_MESA, OBJ_BRAZO], LISTA_INCOMPATIBLES)) ->
		true;
		error(1, OBJ_MESA, OBJ_BRAZO)/*Existe un incompatibilidad*/
	),
	(OBJ_MESA == FORM/COLOR ->
		true;
		error(2, FORM/COLOR)/*No se puede coger poque no esta en la cima*/
	),
	pushMonton(BRAZO, 1, FORM/COLOR, LISTA_BRAZOS, AUX_LISTA_BRAZOS_1),
	actualizarCercania(BRAZO, 2, MESA, AUX_LISTA_BRAZOS_1, AUX_LISTA_BRAZOS_2),
	actualizarCercania(BRAZO, 3, MONTON, AUX_LISTA_BRAZOS_2, NUEVA_LISTA_BRAZOS),
	updateMesas(LISTA_MESAS, NUEVA_LISTA_MESAS),
	updateBrazos(LISTA_BRAZOS, NUEVA_LISTA_BRAZOS),
	printConf(NUEVA_LISTA_MESAS, NUEVA_LISTA_BRAZOS).

% Depositar un objeto
el BRAZO deposita FORM/COLOR en MONTON de MESA :-
	(FORM/COLOR ->
		true;
		error(9, FORM/COLOR)
	),
	conf(LISTA_MESAS, LISTA_BRAZOS, LISTA_INCOMPATIBLES),
	buscarMonton(MESA, MONTON, OBJ_MESA, LISTA_MESAS),
	(popMonton(BRAZO, 1, OBJ_BRAZO, LISTA_BRAZOS, AUX_LISTA_BRAZOS_1) ->
		true;
		error(7, FORM/COLOR)
	),
	(not(search([OBJ_BRAZO, OBJ_MESA], LISTA_INCOMPATIBLES)) ->
		true;
		error(1, OBJ_BRAZO, OBJ_MESA)
	),
	(OBJ_BRAZO == FORM/COLOR ->
		true;
		error(6, FORM/COLOR)
	),
	pushMonton(MESA, MONTON, FORM/COLOR, LISTA_MESAS, NUEVA_LISTA_MESAS),
	actualizarCercania(BRAZO, 2, MESA, AUX_LISTA_BRAZOS_1, AUX_LISTA_BRAZOS_2),
	actualizarCercania(BRAZO, 3, MONTON, AUX_LISTA_BRAZOS_2, NUEVA_LISTA_BRAZOS),
	updateMesas(LISTA_MESAS, NUEVA_LISTA_MESAS),
	updateBrazos(LISTA_BRAZOS, NUEVA_LISTA_BRAZOS),
	printConf(NUEVA_LISTA_MESAS, NUEVA_LISTA_BRAZOS).


% Coger objetos en un monton HEAP de una mesa TABLE hasta encontrar
% uno en concreto e ir depositandolos en la pila del brazo ARM
el BRAZO coge_en MONTON de MESA hasta_encontrar FORM/COLOR :-
	(FORM/COLOR ->
		true;
		error(9, FORM/COLOR)
	),
	conf(LISTA_MESAS, LISTA_BRAZOS, LISTA_INCOMPATIBLES),
	buscarMonton(BRAZO, 1, OBJ_BRAZO, LISTA_BRAZOS),
	(popMonton(MESA, MONTON, OBJ_MESA, LISTA_MESAS, NUEVA_LISTA_MESAS) ->
		true;
		printConf(LISTA_MESAS, LISTA_BRAZOS),
		error(3, FORM/COLOR)
	),
	(not(search([OBJ_MESA, OBJ_BRAZO], LISTA_INCOMPATIBLES)) ->
		true;
		printConf(LISTA_MESAS, LISTA_BRAZOS),
		error(1, OBJ_MESA, OBJ_BRAZO)
	),
	pushMonton(BRAZO, 1, OBJ_MESA, LISTA_BRAZOS, AUX_LISTA_BRAZOS_1),
	actualizarCercania(BRAZO, 2, MESA, AUX_LISTA_BRAZOS_1, AUX_LISTA_BRAZOS_2),
	actualizarCercania(BRAZO, 3, MESA, AUX_LISTA_BRAZOS_2, NUEVA_LISTA_BRAZOS),
	updateMesas(LISTA_MESAS, NUEVA_LISTA_MESAS),
	updateBrazos(LISTA_BRAZOS, NUEVA_LISTA_BRAZOS),
	((OBJ_MESA \= FORM/COLOR ->
		el BRAZO coge_en MONTON de MESA hasta_encontrar FORM/COLOR);
		printConf(NUEVA_LISTA_MESAS, NUEVA_LISTA_BRAZOS), !
	).

% Depositar objetos en un monton MONTON de un mesa MESA hasta encontrar
% uno en concreto e ir cogiendolos de la pila del brazo BRAZO
el BRAZO deposita_en MONTON de MESA hasta_encontrar FORM/COLOR :-
	(FORM/COLOR ->
		true;
		error(9, FORM/COLOR)
	),
	conf(LISTA_MESAS, LISTA_BRAZOS, LISTA_INCOMPATIBLES),
	buscarMonton(MESA, MONTON, OBJ_MESA, LISTA_MESAS),
	(popMonton(BRAZO, 1, OBJ_BRAZO, LISTA_BRAZOS, AUX_LISTA_BRAZOS_1) ->
		true;
		printConf(LISTA_MESAS, LISTA_BRAZOS),
		error(3, FORM/COLOR)
	),
	(not(search([OBJ_BRAZO, OBJ_MESA], LISTA_INCOMPATIBLES)) ->
		true;
		printConf(LISTA_MESAS, LISTA_BRAZOS),
		error(1, OBJ_BRAZO, OBJ_MESA)
	),
	pushMonton(MESA, MONTON, OBJ_BRAZO, LISTA_MESAS, NUEVA_LISTA_MESAS),
	actualizarCercania(BRAZO, 2, MESA, AUX_LISTA_BRAZOS_1, AUX_LISTA_BRAZOS_2),
	actualizarCercania(BRAZO, 3, MONTON, AUX_LISTA_BRAZOS_2, NUEVA_LISTA_BRAZOS),
	updateMesas(LISTA_MESAS, NUEVA_LISTA_MESAS),
	updateBrazos(LISTA_BRAZOS, NUEVA_LISTA_BRAZOS),
	((OBJ_BRAZO \= FORM/COLOR ->
		el BRAZO deposita_en MONTON de MESA hasta_encontrar FORM/COLOR);
		printConf(NUEVA_LISTA_MESAS, NUEVA_LISTA_BRAZOS), !
	).


% Agrega una incompatibilidad de un objeto FORM1/COLOR1 sobre otro
% objeto FORM2/COLOR2
incompatible FORM1/COLOR1 sobre FORM2/COLOR2 :-
	(FORM1/COLOR1 ->
		true;
		error(9, FORM1/COLOR1)
	),
	(FORM2/COLOR2 ->
		true;
		error(9, FORM2/COLOR2)
	),
	imposible(LISTA_INCOMPATIBLES),
	(not(search([FORM1/COLOR1, FORM2/COLOR2], LISTA_INCOMPATIBLES)) ->
		true;
		error(10)
	),
	add([FORM1/COLOR1, FORM2/COLOR2], LISTA_INCOMPATIBLES, NUEVA_LISTA_INCOMPATIBLES),
	updateImposible(LISTA_INCOMPATIBLES, NUEVA_LISTA_INCOMPATIBLES),
	printImposible(NUEVA_LISTA_INCOMPATIBLES).



% Busca el brazo mas cercano al objeto y lo coge
coger FORM/COLOR cercano :-
	(FORM/COLOR ->
		true;
		error(9, FORM/COLOR)
	),
	conf(LISTA_MESAS, LISTA_BRAZOS, LISTA_INCOMPATIBLES),
	searchObject(LISTA_MESAS, FORM/COLOR, 1, 0, LISTA_MESAS_MONTONES),
	((searchNearMesaMonton(LISTA_BRAZOS, LISTA_MESAS_MONTONES, 0, BRAZO, MESA, MONTON);
	searchNearMesa(LISTA_BRAZOS, LISTA_MESAS_MONTONES, 0, BRAZO, MESA, MONTON);
	searchNear(LISTA_MESAS_MONTONES, BRAZO, MESA, MONTON)) ->
		true;
		error(8, FORM/COLOR)),
	popMonton(MESA, MONTON, OBJ_MESA, LISTA_MESAS, NUEVA_LISTA_MESAS),
	buscarMonton(BRAZO, 1, OBJ_BRAZO, LISTA_BRAZOS),
	not(search([FORM/COLOR, OBJ_BRAZO], LISTA_INCOMPATIBLES)),
	pushMonton(BRAZO, 1, OBJ_MESA, LISTA_BRAZOS, NUEVA_LISTA_BRAZOS),
	updateMesas(LISTA_MESAS, NUEVA_LISTA_MESAS),
	updateBrazos(LISTA_BRAZOS, NUEVA_LISTA_BRAZOS),
	printConf(NUEVA_LISTA_MESAS, NUEVA_LISTA_BRAZOS).
