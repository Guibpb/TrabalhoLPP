:- dynamic estado/2. %define estado como dinamico
:- dynamic tempo_isolado/2.
:- dynamic tempo_infec/2.
:- dynamic conecta/2.
:- dynamic dias_cura/1.

%estados iniciais de conexao entre as pessoas 

conexao(A, B) :- conecta(A, B).
conexao(A, B) :- conecta(B, A).%define conexao como bidirecional

taxa_infeccao(0.3).
dias_cura(7).

iniciar :-
    retractall(estado(_,_)),
    retractall(tempo_infec(_,_)),
    retractall(tempo_isolado(_,_)),
    writeln("Simulacao iniciada."),
    quant_pessoas,
    mostrar_estados,
    ler_dias.
    
run :-
    findall(P, estado(P, infectado), I1),
    format("Início do Dia - Infectados: ~w~n~n", [I1]),
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
    dias_cura(T),
    T1 is X**2*(1/T**2), %aumenta a taxa de cura de acordo com dias da infeccao equacao y = (1/DiasCura²)x²
    %format("Taxa de Cura: ~3f - ", T1),
    %format("Pessoa ~w~n", Infectado),
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
    findall(Pessoa, estado(Pessoa, suscetivel), ListaP),
    findall(Pessoa, estado(Pessoa, infectado), ListaInf),
    findall((Pessoa, Tempo), tempo_isolado(Pessoa,Tempo), ListaIso),
    format("Pessoas suscetiveis: ~w~n", [ListaP]),
    format("Pessoas infectadas: ~w~n", [ListaInf]),
    format("Pessoas isoladas e tempo de isolamento: ~w~n", [ListaIso]).

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
    ))

    %findall((Pessoa, Tempo), tempo_infec(Pessoa, Tempo), ListaTempos),
    %format("Tempo de infeccao de todo mundo: ~w~n~n", [ListaTempos])
    .

%funcoes teste
 
quant_pessoas :-
    retractall(estado(_,_)),
    write("Insira o número de pessoas totais: "), read(NPessoa), integer(NPessoa), nl,
    write("Insira o número de pessoas infectadas: "), read(NInfec), integer(NInfec), nl,
    write("Insira o número de pessoas isoladas: "), read(NIsol), integer(NIsol), nl,
    NPessoa >= NInfec + NIsol,
    loop_pessoas(1, NPessoa),
    loop_infec(1, NInfec, NPessoa),
    loop_isol(1, NIsol, NPessoa).

loop_pessoas(I, NP) :-
    I =< NP,
    criar_pessoa(I, NP),
    I2 is I + 1,
    loop_pessoas(I2, NP).

loop_pessoas(I, N) :- I > N.

loop_infec(I, NInfec, Max) :-
    I =< NInfec,
    infectar_random(Max),
    I2 is I+1,
    loop_infec(I2, NInfec, Max).

loop_infec(I, NInfec, _) :- I > NInfec.

loop_isol(I, NIsol, Max) :-
    I =< NIsol,
    isolar_random(Max),
    I2 is I + 1,
    loop_isol(I2, NIsol, Max).

loop_isol(I, NIsol, _) :- I > NIsol.

criar_pessoa(NumPessoa, Max) :-
    atomic_list_concat([p, NumPessoa], Pessoa),
    assertz(estado(Pessoa,suscetivel)),
    retractall(conecta(Pessoa, _)),
    ignore(definir_conexoes(NumPessoa, Pessoa, Max)).

definir_conexoes(NumPessoa, Pessoa, Max) :-
    NumPessoa =:= Max,
    assertz(conecta(Pessoa, p1)).

definir_conexoes(NumPessoa, Pessoa, Max) :-
    NumPessoa < Max,
    NumContato2 is NumPessoa + 1,
    atomic_list_concat([p, NumContato2], Pessoa3),
    assertz(conecta(Pessoa, Pessoa3)).

isolar_random(N) :-
    repeat,
    random_between(1, N, X),
    atomic_list_concat([p,X], Pessoa),
    estado(Pessoa, suscetivel),
    isolar(Pessoa),
    !.

isolar(Pessoa):-
    retractall(estado(Pessoa, _)),
    assertz(estado(Pessoa, isolado)),
    random_between(1,4, X),
    assertz(tempo_isolado(Pessoa, X)).

infectar_random(N):-
    repeat,
    random_between(1, N, X),
    atomic_list_concat([p,X], Pessoa),
    estado(Pessoa, suscetivel),
    infec(Pessoa),
    !.

infec(Pessoa) :-
    retract(estado(Pessoa, suscetivel)),
    assertz(estado(Pessoa, infectado)),
    assertz(tempo_infec(Pessoa, 1)).

inserir_taxa_morte:-
    write("Insira a taxa de morte: "),
    read(Taxa),
    float(Taxa),
    Taxa =< 1,
    Taxa > 0,
    dias_cura(Dia),
    A is ((-4)*Taxa)/Dia**2,
    B is (4*Taxa)/Dia,
    calc_morte_dia(A, B, Dia).

calc_morte_dia(A, B) :-
    dias_cura(N),
    calc_morte_dia(1, N, A, B).

calc_morte_dia(I, Max, A, B) :-
    I =< Max,
    Taxa is (I**2*A + I*B),
    format("Taxa para o dia ~w", I),
    format(" - ~3f~n", Taxa),
    I2 is I+1,
    calc_morte_dia(I2, Max, A, B).

calc_morte_dia(I, Max, _, _) :-
    I > Max.