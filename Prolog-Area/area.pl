% =================================================================
% ST0244 - Paradigmas de Programacion
% Practica I - Del pixel a la integral: area bajo la curva
% Parte logica (Prolog)
%
% Enfoque: en lugar de describir "que pasos ejecutar", se describen
% las RELACIONES que deben cumplirse entre una columna X, su altura
% f(X) y la imagen. Prolog se encarga de encontrar los valores que
% satisfacen esas relaciones (findall/3) y de acumularlos (sum_list/2).
%
% Pipeline conceptual:
%   relaciones -> valores que satisfacen f(x) -> M -> area
%
% Ejecutar:
%   swipl area.pl ../curva_binaria_P4.pbm
%   (si no se pasa argumento, usa por defecto curva_binaria_P4.pbm)
% =================================================================

:- initialization(main).

% -----------------------------------------------------------------
% 1. CARGA DEL ARCHIVO PBM P4
% -----------------------------------------------------------------
% leer_pbm(+Archivo, -Ancho, -Alto, -Datos, -BytesPorFila)
%   Datos es un termino compuesto bytes(B1,B2,...,Bn) construido a
%   partir de la lista de bytes binarios. Usar un termino compuesto
%   (en vez de una lista) permite acceso practicamente O(1) a
%   cualquier byte mediante arg/3, necesario para consultar pixeles
%   en cualquier orden.

leer_pbm(Archivo, Ancho, Alto, Datos, BytesPorFila) :-
    read_file_to_codes(Archivo, TodosLosBytes, [type(binary)]),
    parsear_cabecera(TodosLosBytes, Ancho, Alto, BytesImagen),
    BytesPorFila is (Ancho + 7) // 8,
    Datos =.. [bytes|BytesImagen].

% parsear_cabecera(+Bytes, -Ancho, -Alto, -RestoBinario)
parsear_cabecera(Bytes, Ancho, Alto, Resto) :-
    Bytes = [0'P, 0'4 | Bytes1],
    saltar_blancos_y_comentarios(Bytes1, Bytes2),
    leer_entero(Bytes2, Ancho, Bytes3),
    saltar_blancos_y_comentarios(Bytes3, Bytes4),
    leer_entero(Bytes4, Alto, Bytes5),
    Bytes5 = [_UnEspacio|Resto].  % PBM: un unico byte de espacio antes del binario

saltar_blancos_y_comentarios([0'#|Resto], BytesFinal) :-
    !,
    saltar_hasta_nl(Resto, Resto1),
    saltar_blancos_y_comentarios(Resto1, BytesFinal).
saltar_blancos_y_comentarios([C|Resto], BytesFinal) :-
    code_type(C, space),
    !,
    saltar_blancos_y_comentarios(Resto, BytesFinal).
saltar_blancos_y_comentarios(Bytes, Bytes).

saltar_hasta_nl([0'\n|Resto], Resto) :- !.
saltar_hasta_nl([_|Resto], Final) :-
    saltar_hasta_nl(Resto, Final).

leer_entero(Bytes, Numero, Resto) :-
    leer_digitos(Bytes, Digitos, Resto),
    Digitos \= [],
    number_codes(Numero, Digitos).

leer_digitos([C|Resto], [C|Digitos], Final) :-
    code_type(C, digit),
    !,
    leer_digitos(Resto, Digitos, Final).
leer_digitos(Bytes, [], Bytes).

% -----------------------------------------------------------------
% 2. ACCESO A PIXELES INDIVIDUALES
% -----------------------------------------------------------------
% pixel_negro(+X, +Y, +Datos, +BytesPorFila)
%   Verdadero si el pixel (X,Y) es negro (bit = 1).
%   Empaquetado: MSB primero, filas alineadas a byte (estandar PBM P4).

pixel_negro(X, Y, Datos, BytesPorFila) :-
    IndiceByte is Y * BytesPorFila + (X // 8) + 1,   % +1: arg/3 empieza en 1
    arg(IndiceByte, Datos, Byte),
    IndiceBit is 7 - (X mod 8),
    Bit is (Byte >> IndiceBit) /\ 1,
    Bit =:= 1.

% -----------------------------------------------------------------
% 3. LA FUNCION DISCRETA f(x) COMO RELACION
% -----------------------------------------------------------------
% f(+X, +Datos, +Alto, +BytesPorFila, -Altura)
%   Altura es el numero de pixeles negros consecutivos contados
%   desde la fila inferior de la imagen hacia arriba.

f(X, Datos, Alto, BytesPorFila, Altura) :-
    YInicial is Alto - 1,
    contar_negros(X, YInicial, Datos, BytesPorFila, 0, Altura).

contar_negros(X, Y, Datos, BytesPorFila, Acumulado, Altura) :-
    Y >= 0,
    pixel_negro(X, Y, Datos, BytesPorFila),
    !,
    Y1 is Y - 1,
    Acumulado1 is Acumulado + 1,
    contar_negros(X, Y1, Datos, BytesPorFila, Acumulado1, Altura).
contar_negros(_, _, _, _, Altura, Altura).

% -----------------------------------------------------------------
% 4. ESTRUCTURA DE ALTURAS M, CONSTRUIDA DE FORMA DECLARATIVA
%    M = [f(0), f(1), ..., f(n-1)]
% -----------------------------------------------------------------
alturas(Ancho, Datos, Alto, BytesPorFila, M) :-
    MaxX is Ancho - 1,
    findall(Altura,
            ( between(0, MaxX, X),
              f(X, Datos, Alto, BytesPorFila, Altura)
            ),
            M).

% -----------------------------------------------------------------
% 5. AREA MEDIANTE SUMA DE RIEMANN (Ax = 1 pixel)
% -----------------------------------------------------------------
area(M, Area) :- sum_list(M, Area).

% -----------------------------------------------------------------
% 6. VISUALIZACION DE LA CURVA EN CONSOLA (imagen escalada)
% -----------------------------------------------------------------
mostrar_curva(M, AnchoConsola, AltoConsola) :-
    length(M, N),
    TamGrupo is max(1, N // AnchoConsola),
    agrupar(M, TamGrupo, Grupos),
    maplist(promedio, Grupos, Muestras),
    max_list(Muestras, MaxAltura),
    numlist_desc(AltoConsola, 1, Filas),
    forall(member(Fila, Filas), mostrar_fila(Muestras, MaxAltura, AltoConsola, Fila)),
    length(Muestras, NCols),
    string_of_char(NCols, 0'-, Linea),
    writeln(Linea).

mostrar_fila(Muestras, MaxAltura, AltoConsola, Fila) :-
    maplist(caracter_para(MaxAltura, AltoConsola, Fila), Muestras, Caracteres),
    atom_codes(LineaAtom, Caracteres),
    writeln(LineaAtom).

caracter_para(MaxAltura, AltoConsola, Fila, Altura, Caracter) :-
    ( MaxAltura =:= 0
    -> AlturaEscalada = 0
    ;  AlturaEscalada is round(Altura / MaxAltura * AltoConsola)
    ),
    ( AlturaEscalada >= Fila -> Caracter = 0'# ; Caracter = 32 ).

% agrupar/3: divide una lista en sublistas de tamano TamGrupo
% (la ultima puede ser mas corta)
agrupar([], _, []) :- !.
agrupar(Lista, TamGrupo, [Grupo|Resto]) :-
    length(Grupo0, TamGrupo),
    ( append(Grupo0, Cola, Lista)
    -> Grupo = Grupo0, agrupar(Cola, TamGrupo, Resto)
    ;  Grupo = Lista, Resto = []
    ).

promedio(Lista, Promedio) :-
    sum_list(Lista, Suma),
    length(Lista, N),
    ( N =:= 0 -> Promedio = 0 ; Promedio is Suma // N ).

numlist_desc(Alto, Bajo, Lista) :-
    numlist(Bajo, Alto, Ascendente),
    reverse(Ascendente, Lista).

string_of_char(N, Codigo, Cadena) :-
    length(Codigos, N),
    maplist(=(Codigo), Codigos),
    atom_codes(Cadena, Codigos).

% -----------------------------------------------------------------
% 7. VISUALIZACION DE M[x] = f(x) CON BLOQUES UNICODE (sparkline)
% -----------------------------------------------------------------
mostrar_funcion_alturas(M, AnchoConsola) :-
    length(M, N),
    TamGrupo is max(1, N // AnchoConsola),
    agrupar(M, TamGrupo, Grupos),
    maplist(promedio, Grupos, Muestras),
    max_list(Muestras, MaxAltura),
    maplist(bloque_unicode(MaxAltura), Muestras, Bloques),
    atomic_list_concat(Bloques, Linea),
    writeln(Linea).

bloque_unicode(MaxAltura, Altura, Bloque) :-
    % Se usan puntos de codigo Unicode explicitos (0x2581.. = bloques de octavos)
    % en vez de caracteres literales, para evitar problemas de codificacion
    % del archivo fuente en distintos sistemas.
    CodigosNiveles = [0' , 0x2581, 0x2582, 0x2583, 0x2584, 0x2585, 0x2586, 0x2587, 0x2588],
    length(CodigosNiveles, NumNiveles),
    ( MaxAltura =:= 0
    -> Indice = 0
    ;  Indice is min(NumNiveles - 1, round(Altura / MaxAltura * (NumNiveles - 1)))
    ),
    nth0(Indice, CodigosNiveles, Codigo),
    char_code(Bloque, Codigo).

% -----------------------------------------------------------------
% 8. VALORES DE MUESTRA x_i -> f(x_i)
% -----------------------------------------------------------------
mostrar_muestras(M, Cantidad) :-
    length(M, N),
    Paso is max(1, N // Cantidad),
    MaxIndice is N - 1,
    findall(X, (between(0, MaxIndice, X), 0 is X mod Paso), IndicesTodos),
    length(Indices, Cantidad),
    append(Indices, _, IndicesTodos),
    forall(member(X, Indices),
           ( nth0(X, M, Altura),
             format("x_~w = ~w  ->  f(x_~w) = ~w pixeles~n", [X, X, X, Altura])
           )).

% -----------------------------------------------------------------
% PROGRAMA PRINCIPAL
% -----------------------------------------------------------------
main :-
    set_stream(user_output, encoding(utf8)),
    current_prolog_flag(argv, Argv),
    ( Argv = [Archivo|_] -> true ; Archivo = 'curva_binaria_P4.pbm' ),

    leer_pbm(Archivo, Ancho, Alto, Datos, BytesPorFila),
    format("Imagen: ~w x ~w pixeles~n", [Ancho, Alto]),

    alturas(Ancho, Datos, Alto, BytesPorFila, M),
    area(M, Area),

    nl, writeln('=== VISUALIZACION DE LA CURVA (imagen escalada) ==='),
    mostrar_curva(M, 100, 25),

    nl, writeln('=== FUNCION DE ALTURAS M[x] = f(x) (bloques unicode) ==='),
    mostrar_funcion_alturas(M, 100),

    nl, writeln('=== VALORES DE MUESTRA x_i -> f(x_i) ==='),
    mostrar_muestras(M, 10),

    format("~nCada columna tiene base = 1 pixel~n"),
    format("Area = suma de f(x_i)~n"),
    format("Area = ~w pixeles cuadrados~n", [Area]),
    halt.

main :-
    writeln('Error: no se pudo procesar el archivo PBM.'),
    halt(1).
