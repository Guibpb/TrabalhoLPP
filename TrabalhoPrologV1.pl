:- dynamic estado/2. %define estado como dinamico
:- dynamic tempo_isolado/2.
:- dynamic tempo_infec/2.

%estados iniciais de conexao entre as pessoas 
conecta(p1, p2).
conecta(p2, p3).
conecta(p2, p4).
conecta(p3, p4).
conecta(p4, p5).
conecta(p5, p6).
conecta(p6, p2).
conecta(p7, p5).
conecta(p7, p4).

conexao(A, B) :- conecta(A, B).
conexao(A, B) :- conecta(B, A).
%define conexao como bidirecional

taxa_infeccao(0.3).
taxa_cura(0.1).

iniciar :-
    retractall(estado(_,_)),
    retractall(tempo_isolado(_,_)),
    assertz(estado(p1, suscetivel)),
    assertz(estado(p2, infectado)),
    assertz(estado(p3, suscetivel)),
    assertz(estado(p5, suscetivel)),
    assertz(estado(p6, suscetivel)),
    assertz(estado(p7, infectado)),
    assertz(estado(p4, isolado)),
    assertz(tempo_isolado(p4, 3)), %tempo default
    assertz(tempo_infec(p2, 1)),
    assertz(tempo_infec(p7, 1)),
    writeln("Simulacao iniciada.").
    
run :-
    findall(P, estado(P, infectado), I1),
    format("Início do Dia - Infectados: ~w~n", [I1]),
    rodada_infeccao,
    rodada_cura,
    rodada_isolamento,
    att_temp_infec,
    findall(P, estado(P, infectado), I2),
    format("Final do Dia - Infectados: ~w~n", [I2]).

loop_dias(N) :-
    loop_dias(1, N).

loop_dias(Dias, Max) :-
    Dias =< Max,
    format("~nDia ~w - ~n", Dias),
    Temp is Dias + 1,
    run,
    loop_dias(Temp, Max).

loop_dias(Dias, Max) :-
    Dias > Max.

rodada_infeccao :-
    findall(Pessoa, estado(Pessoa, infectado), Infectados),
    forall(member(PessoaInfectada, Infectados), 
        infectar_conexoes(PessoaInfectada)
    ).

infectar_conexoes(Infectado) :-
    findall(Pessoa, conexao(Pessoa, Infectado), Pessoas),
    forall(member(Pessoa, Pessoas), 
        ignore(infectar(Pessoa))
    ).

infectar(Pessoa) :-
    estado(Pessoa, suscetivel),
    random(R),
    taxa_infeccao(T),
    R =< T,
    retract(estado(Pessoa, suscetivel)),
    assertz(estado(Pessoa, infectado)),
    assertz(tempo_infec(Pessoa, 0)),
    format("Pessoa ~w se infectou.~n", Pessoa).

rodada_cura :-
    findall(Pessoa, estado(Pessoa, infectado), Infectados),
    forall(member(Infectado, Infectados), 
        ignore(curar(Infectado))
    ).

curar(Infectado) :-
    tempo_infec(Infectado, X),
    taxa_cura(T),
    T1 is T * (0.6 + (X ** 2)*0.4), %aumenta a taxa de cura de acordo com dias da infeccao
    format("Taxa de Cura: ~3f - ", T1),
    format("Pessoa ~w~n", Infectado),
    random(R),
    R =< T1,
    retract(estado(Infectado, infectado)),
    retract(tempo_infec(Infectado, X)),
    assertz(estado(Infectado, suscetivel)),
    format("Pessoa ~w se curou.~n", Infectado).
curar(_).

rodada_isolamento :-
    findall(Pessoa, estado(Pessoa, isolado), Isolados),
    forall(member(Isolado, Isolados), ignore(verificar_tempo(Isolado))).

verificar_tempo(Pessoa) :-
    tempo_isolado(Pessoa, X),
    Y is X - 1,
    definir_isolamento(Pessoa, Y).

definir_isolamento(P, 0):-
    retract(tempo_isolado(Pessoa, 1)),
    assertz(tempo_isolado(Pessoa, 0)),
    retract(estado(P, isolado)),
    assertz(estado(P, suscetivel)),
    format("Pessoa ~w saiu de isolamento~n", P).

definir_isolamento(Pessoa, Y) :-
    Y > 0,
    retractall(tempo_isolado(Pessoa, _)),
    assertz(tempo_isolado(Pessoa, Y)),
    !.

mostrar_tempos_isolamento :-
    findall((P, Tempo), tempo_isolado(P, Tempo), Lista),
    format("~w~n", [Lista]),
    estado(p4, X),
    format("~w~n", X).

mostrar_estados :-
    findall((Pessoa, Estado), estado(Pessoa,Estado), ListaTotal),
    format("Estado atual: ~w~n", [ListaTotal]).

mostrar_conexoes :-
    findall((Pessoa1, Pessoa2), conecta(Pessoa1, Pessoa2), Lista),
    format("Todas as conexoes: ~w~n", [Lista]).

ler_dias :- 
    write("Insira o numero de dias: "),
    read(N),
    integer(N),
    loop_dias(N).

att_temp_infec :-
    findall((P,X), tempo_infec(P,X), ListaTempos0),

    forall(member((P,X), ListaTempos0), (
        retractall(tempo_infec(P,_)), 
        X1 is X + 1, 
        assertz(tempo_infec(P, X1))
    )),

    findall((Pessoa, Tempo), tempo_infec(Pessoa, Tempo), ListaTempos),
    format("Tempo de infeccao de todo mundo: ~w~n~n", [ListaTempos]).

%funcoes teste
 
quant_pessoas :-
    retractall(estado(_,_)),
    write("Insira o número de pessoas: "),
    read(Num),
    integer(Num),
    loop_pessoas(Num).
    
loop_pessoas(N) :-
    loop_pessoas(1, N).

loop_pessoas(I, N) :-
    I =< N,
    criar_pessoa(I),
    I2 is I + 1,
    loop_pessoas(I2, N).

loop_pessoas(I, N) :-
    I > N.

criar_pessoa(Seq) :-
    atomic_list_concat([p, Seq], S),
    format("~w~n", S),
    assertz(estado(S,suscetivel)).

isolar_random(N) :-
    random_between(1, N, X),
    isolar(X),
    format("Pessoa p~w se isolou aleatoriamente.~n", X).

isolar(Num):-
    atomic_list_concat([p, Num], S),
    retractall(estado(S, _)),
    assertz(estado(S, isolado)).