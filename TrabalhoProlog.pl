:- dynamic estado/2. %define estado como dinâmico
:- dynamic tempo_isolado/2.

%estados iniciais de conexão entre as pessoas 
conecta(p1, p2).
conecta(p2, p3).
conecta(p3, p4).
conecta(p4, p5).
conecta(p5, p6).
conecta(p6, p2).
conecta(p7, p4).

conexao(A, B) :- conecta(A, B).
conexao(A, B) :- conecta(B, A).
%define conexão como bidirecional

taxa_infeccao(0.3).
taxa_cura(0.2).

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
    assertz(tempo_isolado(p4, 1)), %tempo default
    writeln("Simulação iniciada.").
    
run :-
    ler_dias.

loop(0) :-
    !.

loop(Dias) :-
    Dias > 0,
    format("Dia ~w - ~n", Dias),
    Temp is Dias-1,
    rodada_infeccao,
    rodada_cura,
    loop(Temp).

rodada_infeccao :-
    findall(Pessoa, estado(Pessoa, infectado), Infectados),
    format("Infectados: ~w~n", [Infectados]),
    forall(member(PessoaInfectada, Infectados), infectar_conexoes(PessoaInfectada)).

infectar_conexoes(Infectado) :-
    findall(Pessoa, conexao(Pessoa, Infectado), Pessoas),
    forall(member(Pessoa, Pessoas), ignore(infectar(Pessoa))).

infectar(Pessoa) :-
    estado(Pessoa, suscetivel),
    random(R),
    taxa_infeccao(T),
    R =< T,
    retract(estado(Pessoa, suscetivel)),
    assertz(estado(Pessoa, infectado)),
    format("Pessoa ~w se infectou.~n", Pessoa).

rodada_cura :-
    findall(Pessoa, estado(Pessoa, infectado), Infectados),
    forall(member(Infectado, Infectados), ignore(curar(Infectado))).

curar(Infectado) :-
    taxa_cura(T),
    random(R),
    R =< T,
    retract(estado(Infectado, infectado)),
    assertz(estado(Infectado, recuperado)),
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
    assertz(estado(P, suscetivel)).

definir_isolamento(Pessoa, Y) :-
    Y > 0,
    X is Y + 1,
    retract(tempo_isolado(Pessoa, X)),
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
    format("Todas as conexões: ~w~n", [Lista]).

ler_dias :- 
    write("Insira o número de dias: "),
    read(N),
    integer(N),
    loop(N).
