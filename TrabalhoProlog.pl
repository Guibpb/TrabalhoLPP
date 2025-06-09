% - Definição de Predicados Dinâmicos 
:- dynamic estado/2. 
:- dynamic tempo_isolado/2.
:- dynamic tempo_infec/2.
:- dynamic conecta/2.
:- dynamic dias_cura/1.
:- dynamic taxa_infeccao/1.
:- dynamic populacao/1.
:- dynamic infectados/1.
:- dynamic isolados/1.
:- dynamic vacinados/1.

% - Definição do Predicado Conexão como sendo bidirecional ao Conecta
conexao(A, B) :- conecta(A, B).
conexao(A, B) :- conecta(B, A).

% - Predicado Principal
main :-
    retractall(tempo_infec(_,_)), retractall(tempo_isolado(_,_)),
    writeln("Menu Principal - Escolha uma opção abaixo."),
    writeln("1 - Inserir quantidade de pessoas"),
    writeln("2 - Inserir taxas de infecção/cura/morte."),
    writeln("3 - Inserir medidas de prevenção."),
    writeln("4 - Iniciar Simulação."),
    writeln("0 - Sair."),
    write("Opção: "), read(Op), integer(Op), Op >= 0, Op < 5,
    run(Op).

run(1) :-
    quant_pessoas.

/*run(2) :-
run(3) :-
run(4) :-
run(0) :-*/

/*Esse predicado lê a quantidade de pessoas e executa uma função de loop para 
adicionar pessoas individualmente*/
quant_pessoas :-
    retractall(estado(_,_)), retractall(conecta(_,_)),
    retractall(populacao(_)), retractall(infectados(_)), %apaga todas as informações de predicados dinâmicos

    write("Insira o número de pessoas totais: "), read(NPessoa), integer(NPessoa), nl, 
    assertz(populacao(NPessoa)),%insere as informações da população total

    write("Insira o número de pessoas infectadas: "),
    read(NInfec), integer(NInfec), NInfec < NPessoa, nl,%insere as informações de infectados
    assertz(infectados(0)),
    
    loop_pessoas,
    loop_infec(1, NInfec).


% - Predicados que realizam um loop de 1 até N e cria uma pessoa para cada iteração do loop
loop_pessoas:-
    populacao(N),
    loop_pessoas(1, N).

loop_pessoas(I, N) :-
    I =< N,
    criar_pessoa(I),
    J is I + 1,
    loop_pessoas(J, N).

loop_pessoas(I, N) :- I > N.

%Concatena um atómo pN e insere o valor em uma nova pessoa suscetivel
criar_pessoa(NPessoa) :-
    atomic_list_concat([p, NPessoa], Pessoa),
    assertz(estado(Pessoa, suscetivel)),
    retractall(conecta(Pessoa, _)),
    ignore(definir_conexoes(NPessoa, Pessoa)).

%Define a conexão da última pessoa da população à 1ª pessoa
definir_conexoes(NPessoa, Pessoa) :-
    populacao(Max),
    NPessoa =:= Max,
    assertz(conecta(Pessoa, p1)).

%Define a conexão sempre à próxima pessoa da lista 
definir_conexoes(NPessoa, Pessoa) :- 
    populacao(Max),
    NPessoa < Max,
    NContato is NPessoa + 1,
    atomic_list_concat([p, NContato], Pessoa2),
    assertz(conecta(Pessoa, Pessoa2)).

%Predicados que definem um loop de 1 até I, com I sendo o num de infectados
loop_infec(I, NInfec) :-
    I =< NInfec,
    infectar_random, %Executa o predicado para infectar uma pessoa aleatória na população
    I2 is I+1,
    loop_infec(I2, NInfec).

loop_infec(I, NInfec) :- I > NInfec.

/*Repete as instruções até achar um número aleatório na população que está suscetivel,
    concatena o atómo com o número certo e executa o predicado para infectar
*/
infectar_random :-
    populacao(N),
    repeat,

    random_between(1, N, X),
    atomic_list_concat([p,X], Pessoa),
    estado(Pessoa, suscetivel),
    infectar(Pessoa),
    !.

/*Define o estado como infectado, define o tempo de infecção para 0
    e aumenta o número de infectados
*/
infectar(Pessoa) :-
    retractall(estado(Pessoa, suscetivel)),
    assertz(estado(Pessoa, infectado)),
    assertz(tempo_infec(Pessoa, 0)),

    infectados(X), Y is X + 1,
    retractall(infectados(X)), assertz(infectados(Y)).




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

num_infectados :-
    infectados(N),
    format("Número de infectados: ~w~n", N).

num_saudaveis :-
    infectados(Y), populacao(X),
    N is X - Y,
    format("Número de pessoas saudáveis: ~w~n", N).