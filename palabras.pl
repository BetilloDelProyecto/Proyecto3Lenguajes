:- module(palabras, [
    cargar_palabras/2,
    guardar_palabras/2,
    agregar_si_no_existe/3
]).

cargar_palabras(Ruta, Lista) :-
    open(Ruta, read, Flujo, [encoding(utf8)]),
    leer_lineas(Flujo, Lista),
    close(Flujo).

leer_lineas(Flujo, []) :-
    at_end_of_stream(Flujo), !.
leer_lineas(Flujo, [Linea|Resto]) :-
    read_line_to_string(Flujo, Linea),
    leer_lineas(Flujo, Resto).

agregar_si_no_existe(Palabra, Lista, Lista) :-
    member(Palabra, Lista), !.
agregar_si_no_existe(Palabra, Lista, [Palabra|Lista]).

guardar_palabras(Ruta, Lista) :-
    open(Ruta, write, Flujo, [encoding(utf8)]),
    escribir_lineas(Lista, Flujo),
    close(Flujo).

escribir_lineas([], _).
escribir_lineas([Palabra|Resto], Flujo) :-
    writeln(Flujo, Palabra),
    escribir_lineas(Resto, Flujo).