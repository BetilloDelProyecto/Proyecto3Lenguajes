:- module(logica, [
    seleccionar_palabra_aleatoria/1,
    estado_inicial/2,
    estado_inicial/3,
    mostrar_palabra/3,
    adivinar_letra/4,
    verificar_estado/2,
    jugar/1,
    siguiente_turno/1
]).


:- use_module(palabras).  
:- use_module(library(random)).

seleccionar_palabra_aleatoria(Palabra) :-
    cargar_palabras('palabras.txt', Lista),
    random_member(Palabra, Lista).

% estado_inicial(+Palabra, -Estado) con intentos por defecto (7)
estado_inicial(Palabra, Estado) :-
    estado_inicial(Palabra, 7, Estado).

% estado_inicial(+Palabra, +Intentos, -Estado) configurable
estado_inicial(Palabra, Intentos, estado(Palabra, [], Intentos)).

% mostrar_palabra(+Palabra, +LetrasAdivinadas, -Visual)
mostrar_palabra(PalabraStr, Letras, VisualStr) :-
    string_chars(PalabraStr, Chars),
    revelar_letras(Chars, Letras, Resultado),
    atomic_list_concat(Resultado, ' ', VisualStr).

% revelar_letras(+Chars, +Adivinadas, -ResultadoConGuiones)
revelar_letras([], _, []).
revelar_letras([C|Resto], Letras, [C|RResultado]) :-
    member(C, Letras), !,
    revelar_letras(Resto, Letras, RResultado).
revelar_letras([_|Resto], Letras, ['_'|RResultado]) :-
    revelar_letras(Resto, Letras, RResultado).

% adivinar_letra(+Letra, +EstadoAnterior, -EstadoNuevo, -Resultado)
% Resultado es 'acierto' o 'error'

adivinar_letra(L, estado(Palabra, Letras, Intentos), NuevoEstado, Resultado) :-
    (   member(L, Letras)
    ->  % Ya estaba adivinada
        NuevoEstado = estado(Palabra, Letras, Intentos),
        Resultado = repetida
    ;   string_chars(Palabra, Chars),
        (   member(L, Chars)
        ->  Resultado = acierto,
            agregar_si_no_existe(L, Letras, LetrasActualizadas),
            NuevoEstado = estado(Palabra, LetrasActualizadas, Intentos)
        ;   Resultado = error,
            agregar_si_no_existe(L, Letras, LetrasActualizadas),
            I2 is Intentos - 1,
            NuevoEstado = estado(Palabra, LetrasActualizadas, I2)
        )
    ).

% verificar_estado(+Estado, -Resultado)
% Resultado puede ser: victoria | derrota | sigue

verificar_estado(estado(Palabra, Letras, _), victoria) :-
    string_chars(Palabra, Chars),
    subset(Chars, Letras), !.

verificar_estado(estado(_, _, 0), derrota) :- !.

verificar_estado(_, sigue).

jugar(Estado) :-
    Estado = estado(Palabra, Letras, Intentos),
    mostrar_palabra(Palabra, Letras, Visual),
    format("Palabra: ~w~n", [Visual]),
    format("Intentos restantes: ~w~n", [Intentos]),
    write("Ingresa una letra: "), flush_output(current_output),

    leer_letra(Letra),

    ( string_length(Letra, 1) ->
        adivinar_letra(Letra, Estado, NuevoEstado, Resultado),
        (   Resultado = repetida ->
            writeln("Letra ya usada\n"),
            jugar(NuevoEstado)
        ;   Resultado = acierto ->
            writeln("Bien, Letra correcta\n"),
            siguiente_turno(NuevoEstado)
        ;   Resultado = error ->
            writeln("Letra incorrecta\n"),
            siguiente_turno(NuevoEstado)
        )
    ;   writeln("Por favor, ingresa solo una letra\n"),
        jugar(Estado)
    ).

leer_letra(LetraChar) :-
    read_line_to_string(user_input, Input),
    string_lower(Input, Lower),
    string_chars(Lower, Chars),
    ( Chars = [LetraChar] ->
        true
    ;   writeln("Por favor, ingresa solo una letra."),
        leer_letra(LetraChar)
    ).

siguiente_turno(Estado) :-
    verificar_estado(Estado, Resultado),
    (   Resultado = victoria ->
        writeln("Ganaste, Adivinaste toda la palabra.")
    ;   Resultado = derrota ->
        Estado = estado(Palabra, _, _),
        format("Perdiste La palabra era: ~w~n", [Palabra])
    ;   jugar(Estado)
    ).

