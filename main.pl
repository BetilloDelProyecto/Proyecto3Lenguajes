:- use_module(logica).
:- use_module(palabras). % Para agregar palabras nuevas
:- dynamic intentos_configurados/1.

% Valor por defecto para los intentos configurados
intentos_configurados(7).

:- initialization(main).

main :-
    mostrar_menu_principal.

% ------------------------
% Menú Principal
% ------------------------
mostrar_menu_principal :-
    nl,
    writeln("=== Juego Ahorcado ==="),
    writeln("1. Empezar a Jugar"),
    writeln("2. Configuracion"),
    writeln("3. Salir"),
    write("Escoge una opcion: "), flush_output(current_output),
    read_line_to_string(user_input, Opcion),
    manejar_opcion_principal(Opcion).

manejar_opcion_principal("1") :-
    writeln("\n=== Empezar a Jugar ===\n"),
    intentos_configurados(Intentos),
    logica:seleccionar_palabra_aleatoria(Palabra),
    logica:estado_inicial(Palabra, Intentos, Estado),
    logica:jugar(Estado),
    mostrar_menu_principal.
manejar_opcion_principal("2") :-
    mostrar_menu_configuracion.
manejar_opcion_principal("3") :-
    writeln("Gracias por jugar!").
manejar_opcion_principal(_) :-
    writeln("Opcion invalida."),
    mostrar_menu_principal.

% ------------------------
% Menú de Configuración
% ------------------------
mostrar_menu_configuracion :-
    nl,
    writeln("=== Configuracion ==="),
    writeln("1. Agregar Palabra"),
    writeln("2. Numero de Intentos"),
    writeln("3. Salir al Menu Principal"),
    write("Escoge una opcion: "), flush_output(current_output),
    read_line_to_string(user_input, Opcion),
    manejar_opcion_configuracion(Opcion).

manejar_opcion_configuracion("1") :-
    write("Ingresa la palabra que deseas agregar: "), flush_output(current_output),
    read_line_to_string(user_input, Palabra),
    string_lower(Palabra, PalabraMinus),
    cargar_palabras('palabras.txt', Lista),
    agregar_si_no_existe(PalabraMinus, Lista, NuevaLista),
    (Lista \= NuevaLista ->
        guardar_palabras('palabras.txt', NuevaLista),
        writeln("Palabra agregada exitosamente.")
    ;
        writeln("La palabra ya existe en la lista.")
    ),
    mostrar_menu_configuracion.
manejar_opcion_configuracion("2") :-
    write("Ingresa el nuevo numero de intentos: "), flush_output(current_output),
    read_line_to_string(user_input, Entrada),
    ( number_string(Numero, Entrada), Numero > 0 ->
        retractall(intentos_configurados(_)),
        asserta(intentos_configurados(Numero)),
        format("Numero de intentos configurado a ~w.~n", [Numero])
    ;   writeln("Entrada invalida. Debe ser un numero mayor que cero.")
    ),
    mostrar_menu_configuracion.
manejar_opcion_configuracion("3") :-
    mostrar_menu_principal.
manejar_opcion_configuracion(_) :-
    writeln("Opcion invalida."),
    mostrar_menu_configuracion.