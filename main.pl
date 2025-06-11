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
    writeln("1. Empezar a Jugar (1 Jugador)"),
    writeln("2. Jugar 2 Jugadores"),
    writeln("3. Configuracion"),
    writeln("4. Salir"),
    write("Escoge una opcion: "), flush_output(current_output),
    read_line_to_string(user_input, Opcion),
    manejar_opcion_principal(Opcion).

manejar_opcion_principal("1") :-
    writeln("\n=== Empezar a Jugar (1 Jugador) ===\n"),
    intentos_configurados(Intentos),
    logica:seleccionar_palabra_aleatoria(Palabra),
    logica:estado_inicial(Palabra, Intentos, Estado),
    logica:jugar(Estado),
    mostrar_menu_principal.
manejar_opcion_principal("2") :-
    writeln("\n=== Modo 2 Jugadores ===\n"),
    jugar_dos_jugadores,
    mostrar_menu_principal.
manejar_opcion_principal("3") :-
    mostrar_menu_configuracion.
manejar_opcion_principal("4") :-
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

% ------------------------
% Juego para 2 jugadores
% ------------------------

jugar_dos_jugadores :-
    intentos_configurados(Intentos),

    % Jugador 1 ingresa palabra
    repeat,
    write("Jugador 1, ingresa una palabra para que adivine el Jugador 2: "), flush_output(current_output),
    read_line_to_string(user_input, Palabra1),
    string_lower(Palabra1, P1Lower),
    string_chars(P1Lower, Chars1),
    (Chars1 = [] ->
        writeln("La palabra no puede estar vacia."), fail
    ; \+ forall(member(C, Chars1), char_type(C, alpha)) ->
        writeln("La palabra solo puede contener letras."), fail
    ; true),
    !,  % sale del repeat cuando se ingresa una palabra válida

    % Jugador 2 ingresa palabra
    repeat,
    write("Jugador 2, ingresa una palabra para que adivine el Jugador 1: "), flush_output(current_output),
    read_line_to_string(user_input, Palabra2),
    string_lower(Palabra2, P2Lower),
    string_chars(P2Lower, Chars2),
    (Chars2 = [] ->
        writeln("La palabra no puede estar vacía."), fail
    ; \+ forall(member(C, Chars2), char_type(C, alpha)) ->
        writeln("La palabra solo puede contener letras."), fail
    ; true),
    !,  % sale del repeat cuando se ingresa una palabra válida

    % Crear estados de juego
    logica:estado_inicial(P1Lower, Intentos, EstadoJ2),  % J2 adivina esta
    logica:estado_inicial(P2Lower, Intentos, EstadoJ1),  % J1 adivina esta

    jugar_turnos(jugador1, EstadoJ1, EstadoJ2).


jugar_turnos(_, estado(P1, Letras1, _), estado(P2, Letras2, _)) :-
    nonvar(P1), nonvar(P2),
    string_chars(P1, CharsP1),
    string_chars(P2, CharsP2),
    subset(CharsP1, Letras1),
    subset(CharsP2, Letras2),
    writeln("Empate: ambos adivinaron sus palabras al mismo tiempo!").

jugar_turnos(_, estado(P1, Letras1, _), _) :-
    nonvar(P1),
    string_chars(P1, CharsP1),
    subset(CharsP1, Letras1),
    format("Jugador 1 ha ganado! La palabra era: ~w~n", [P1]).

jugar_turnos(_, _, estado(P2, Letras2, _)) :-
    nonvar(P2),
    string_chars(P2, CharsP2),
    subset(CharsP2, Letras2),
    format("Jugador 2 ha ganado! La palabra era: ~w~n", [P2]).

jugar_turnos(_, estado(P1, _, 0), _) :-
    format("Jugador 1 ha perdido. La palabra era: ~w~n", [P1]),
    writeln("Jugador 2 gana!").

jugar_turnos(_, _, estado(P2, _, 0)) :-
    format("Jugador 2 ha perdido. La palabra era: ~w~n", [P2]),
    writeln("Jugador 1 gana!").

jugar_turnos(jugador1, EstadoJ1, EstadoJ2) :-
    writeln("\n=== Turno de Jugador 1 ==="),
    logica:jugar_turno(EstadoJ1, NuevoEstadoJ1, _),
    jugar_turnos(jugador2, NuevoEstadoJ1, EstadoJ2).

jugar_turnos(jugador2, EstadoJ1, EstadoJ2) :-
    writeln("\n=== Turno de Jugador 2 ==="),
    logica:jugar_turno(EstadoJ2, NuevoEstadoJ2, _),
    jugar_turnos(jugador1, EstadoJ1, NuevoEstadoJ2).
