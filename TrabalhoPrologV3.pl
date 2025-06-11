/*Alunos:
Guilherme Borges de Pádua Barbosa - 15653045
Gabriel Hupfer Rhigi - 15508612
Tainara Lopes de Oliveira Terassaka - 13675501
*/

% - Definição de Predicados Dinâmicos 
:- dynamic estado/2. 
:- dynamic tempo_isolado/2.
:- dynamic tempo_infec/2.
:- dynamic dias_cura/1.
:- dynamic conecta/2.
:- dynamic taxa_infeccao/1.
:- dynamic taxa_morte/1.
:- dynamic populacao/1.
:- dynamic infectados/1.
:- dynamic isolados/1.
:- dynamic vacinados/1.
:- dynamic vacinado/1.

% - Definição do Predicado Conexão como sendo bidirecional ao Conecta
conexao(A, B) :- conecta(A, B).
conexao(A, B) :- conecta(B, A).

% - Predicado Principal
main :-
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

run(2) :-
    ler_taxas.
run(3) :-
    medidas_prev.

run(4) :-
    loop_dias.

run(0) :-
    halt.

/*Esse predicado lê a quantidade de pessoas e executa uma função de loop para 
adicionar pessoas individualmente*/
quant_pessoas :-
    retractall(estado(_,_)), retractall(conecta(_,_)), retractall(tempo_infec(_,_)),
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
    concatena o atómo com o número certo e executa o predicado para infectar,
    possui lógica de repeat para evitar duplicatas.
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
    retractall(estado(Pessoa, _)), %Retira o estado suscetível
    assertz(estado(Pessoa, infectado)), %Aplica o estado infectado
    assertz(tempo_infec(Pessoa, 0)), %Define o tempo

    infectados(X), Y is X + 1,
    retractall(infectados(X)), assertz(infectados(Y)),
    format("Pessoa ~w se infectou.~n", Pessoa).


ler_taxas :- %recebe as taxas e as define em predicados dinâmicos
    write("Insira a taxa de infecção: (em porcentagem)"), %Chance de uma pessoa infectada passar a doença para uma conexão
    read(I), number(I), I > 0, I =< 100,

    write("Insira o tempo de duração máximo da infecção (em dias): " ), %Tempo em que a chance de cura chega a 100%
    read(T), integer(T), T > 0,

    write("Insira a taxa de morte diária (em porcentagem): "), %Taxa de morte máxima na metade da infecção
    read(M), number(M), M > 0, M =< 100,

    Mp is M/100,
    Ip is I/100,

    retractall(taxa_infeccao(_)), assertz(taxa_infeccao(Ip)), %Remove os estados antigos e define novos
    retractall(taxa_morte(_)), assertz(taxa_morte(Mp)),
    retractall(dias_cura(_)), assertz(dias_cura(T)).

/* Define medidas preventivas aplicadas a indivíduos suscetíveis da população:
    isolamento (por 1 a 4 dias) e vacinação. Os números são fornecidos pelo usuário
    dentro do limite de pessoas não infectadas.
 */
medidas_prev :-
    populacao(X), infectados(Y), X > 0,
    Saudaveis is X - Y, %Define o número de pessoas saudáveis como o total - número de infectados

    writeln("1 - Pessoas isoladas não poderão ser infectadas durante o tempo de isolamento, de 1 a 4 dias."),
    format("Insira o número de pessoas isoladas (de 0 até ~w): ", Saudaveis), 

    read(Isol), integer(Isol), Isol >= 0, Isol =< Saudaveis, nl, %Verifica se o número é válido

    retractall(estado(_, isolado)), retractall(tempo_isolado(_, _)),%Apaga os estados anteriores
    retractall(isolados(_)), assertz(isolados(Isol)), %Define o número de isolados

    loop_isol(1, Isol, X),

    SaudaveisAtt is Saudaveis - Isol, %Retira os isolados da lista de Saudáveis
    writeln("2 - Pessoas vacinadas terão uma taxa de infecção e morte consideravelmente mais baixa."),
    format("Insira o número de pessoas vacinadas (de 0 até ~w): ", SaudaveisAtt), 

    read(Vac), integer(Vac), Vac >= 0, Vac =< SaudaveisAtt, nl, %Verifica se o número é válido
    retractall(vacinado(_)),
    retractall(vacinados(_)), assertz(vacinados(Vac)), %Define o número de vacinados
    loop_vac(1, Vac, X).

%Aplica isolamento a NIsol pessoas aleatórias suscetíveis, uma por iteração.
loop_isol(I, NIsol, Max) :-
    I =< NIsol,
    isolar_random(Max),
    I2 is I + 1,
    loop_isol(I2, NIsol, Max).

loop_isol(I, NIsol, _) :- I > NIsol.

%Usa lógica de repeat para não ter duplicatas
isolar_random(N) :-
    repeat,
    random_between(1, N, X),
    atomic_list_concat([p,X], Pessoa), %concatena o átomo como nome da pessoa 'pN'
    estado(Pessoa, suscetivel),
    isolar(Pessoa),
    !.

%Retira os estados antigos e define novos, define tempo de isolamento aleatoriamente
isolar(Pessoa):-
    assertz(estado(Pessoa, isolado)),

    random_between(1,4, X), %de 1 a 4 dias de isolamento
    assertz(tempo_isolado(Pessoa, X)).

%Realiza a vacinação para NVac pessoas aleatórias suscetíveis
loop_vac(I, NVac, Max) :-
    I =< NVac,
    vacinar_random(Max), 
    I2 is I + 1,
    loop_vac(I2, NVac, Max).

loop_vac(I, NVac, _) :- I > NVac.

%Usa lógica de repeat para evitar  duplicadas
vacinar_random(N) :-
    repeat,
    random_between(1, N, X),
    atomic_list_concat([p,X], Pessoa), %Concatena o átomo como nome da pessoa 'pN'
    \+ estado(Pessoa, infectado), %Não pode estar infectado
    \+ vacinado(Pessoa), %Não pode já ter sido vacinado
    vacinar(Pessoa),
    !.

%Retira estados antigos e define um estado vacinado
vacinar(Pessoa):-
    assertz(vacinado(Pessoa)).

/*Inicia a simulação por um número de dias informado pelo usuário.
    Limita a entrada entre 1 e 999 para evitar loops infinitos ou simulações longas demais.
 */
loop_dias :-
    write("Insira o número de dias: "),
    read(Dias), integer(Dias), Dias > 0, Dias < 1000,
    loop_dias(1, Dias).

/*Executa a simulação dia a dia. Para cada dia:
    - Imprime o número do dia
    - Chama o predicado 'simular', que representa as ações daquele dia
    - Avança para o próximo dia, até atingir o limite
 */
loop_dias(Dias, Max) :-
    Dias =< Max,
    format("~nDia ~w - ~n", Dias),
    Temp is Dias + 1,
    simular,
    loop_dias(Temp, Max).

loop_dias(I, N) :-
    I > N.

/*Escolha nº 4, por rodadas:
    1- executa a infecção de todas as conexões de pessoas infectadas,
    2- tenta curar todos os infectados (taxa de cura aumenta por dia até o dia máximo de infecção(taxa 100%))
    3- diminui o tempo de isolamento para todos os isolados
    4- aumenta o tempo de infecção para todos os infectados
*/  
simular :-
    findall((Pessoa, Estado), estado(Pessoa, Estado), Estados),
    format("Estados no dia: ~w~n", [Estados]),
    rodada_infeccao,
    rodada_cura,
    rodada_isolamento,
    rodada_morte,
    att_temp_infec.

%Tenta infectar as conexões de todos os infectados
rodada_infeccao :-
    findall(Pessoa, estado(Pessoa, infectado), Infectados), %Realiza a busca de infectados e coloca na lista 'Infectados'
    forall(member(Pessoa, Infectados), %Itera pela lista 'Infectados' e tenta realizar a infecção das conexões
        infectar_conexoes(Pessoa)
    ).

infectar_conexoes(Infectado) :-
    findall(Pessoa, conexao(Pessoa, Infectado), Conexoes), %Realiza a busca das conexões de infectados e coloca na lista 'Conexoes'
    forall(member(Pessoa, Conexoes), %Itera pela lista 'Conexões' e tenta infectar cada um
        ignore(tentar_infec(Pessoa)) %Ignore para não parar o código se der false
    ).

%Se a pessoa for vacinada, a taxa de infecção diminui
tentar_infec(Pessoa) :-
    estado(Pessoa, suscetivel),
    vacinado(Pessoa),
    taxa_infeccao(T), 
    Tvac is T*0.3, %Taxa de infecção diminui em 70%
    random(R), %Float aleatório entre 0 e 1
    R =< Tvac,
    infectar(Pessoa). 

%Pessoa não vacinada, taxa normal
tentar_infec(Pessoa) :-
    estado(Pessoa, suscetivel),
    \+ vacinado(Pessoa),
    taxa_infeccao(T),
    random(R),
    R =< T,
    infectar(Pessoa).

rodada_cura :-
    findall(Pessoa, estado(Pessoa, infectado), Infectados),
    forall(member(Infectado, Infectados),
        curar(Infectado)
    ).

/*Tenta curar uma pessoa infectada com base no tempo de infecção e em uma equação
de probabilidade crescente com o passar dos dias. A fórmula usada aumenta a chance
de cura de forma quadrática conforme o número de dias de infecção se aproxima de DiasCura.
 */
curar(Infectado) :-
    tempo_infec(Infectado, X),
    dias_cura(Dias),

    %Fórmula: y = (1 / Dias²) * X²
    %Essa equação faz a chance de cura crescer de forma quadrática.
    %Exemplo: se DiasCura = 10, a chance no dia 10 será 1 (100%), no dia 5 será 0.25(25%).
    T1 is (1/Dias**2)*(X**2),

    % Número aleatório entre 0 e 1
    random(R),
    R =< T1,

    %A pessoa é removida da lista de infectados e volta ao estado suscetível.
    retract(estado(Infectado, infectado)),
    retract(tempo_infec(Infectado, X)),
    assertz(estado(Infectado, suscetivel)),

    %Diminui por 1 o número de infectados
    infectados(NumInfec),
    NumAtt is NumInfec - 1,
    retract(infectados(_)), assertz(infectados(NumAtt)),

    format("Pessoa ~w se curou.~n", Infectado).
    
        
curar(_). %Se o predicado falhar, não retorna erro

/*Executa uma rodada de atualização do isolamento. Para cada pessoa isolada,
verifica quanto tempo de isolamento resta e atualiza seu estado.
Usa 'ignore' para que falhas não interrompam o loop.
 */
rodada_isolamento :-
    findall(Pessoa, estado(Pessoa, isolado), Isolados),
    forall(member(Isolado, Isolados), ignore(verificar_tempo(Isolado))).

%Reduz o tempo de isolamento da pessoa em 1 dia e redefine o estado, se for o último dia.
verificar_tempo(Pessoa) :-
    tempo_isolado(Pessoa, X),
    Y is X - 1,
    definir_isolamento(Pessoa, Y).

/*Se NovoTempo for 0, a pessoa sai do isolamento e volta a ser suscetível.
    Caso contrário, apenas atualiza o tempo restante.
 */
definir_isolamento(P, 0):-
    isolados(X), Y is X - 1, %Atualiza a lista de isolados

    %Remove o tempo restante
    retractall(tempo_isolado(P, _)),
    assertz(tempo_isolado(P,0)),

    %Retorna ao estado suscetível após fim do isolamento
    retractall(estado(P, _)),
    assertz(estado(P, suscetivel)),

    retractall(isolados(_)),
    assertz(isolados(Y)),
    format("Pessoa ~w saiu de isolamento~n", P).

definir_isolamento(Pessoa, Y) :-
    Y > 0,
    retractall(tempo_isolado(Pessoa, _)),
    assertz(tempo_isolado(Pessoa, Y)),
    !.

%Predicado para calcular a morte de pessoas infectadas
rodada_morte :-
    findall(Pessoa, estado(Pessoa, infectado), Infectados), %Procura todos os infectados
    format("Infectados: ~w~n", [Infectados]),
    forall(member(Infectado, Infectados), tentar_morte(Infectado)). %Para cada um, executa o predicado tentar_morte

%Predicado para calcular a taxa de morte se o infectado for vacinado
tentar_morte(Infectado) :-
    vacinado(Infectado),
    taxa_morte(T),
    NovaTaxa is T * 0.1, %Taxa de morte diminui em 90%
    format("Pessoa ~w é vacinada, portanto a taxa de morte diminui de: ~2f, para: ~2f~n", [Infectado, T, NovaTaxa]),
    calc_taxa_morte(Infectado, NovaTaxa). %Executa o predicado calc_taxa_morte/2

%Se o infectado não for vacinado, a taxa de morte continua a mesma
tentar_morte(Infectado) :-
    \+ vacinado(Infectado),
    taxa_morte(T),
    calc_taxa_morte(Infectado, T). 

/*Calcula a taxa de morte diária de acordo com a equação de uma parábola, dada por:
    f(x) = ((-4T)/D²)x² + ((4T)/D)x

    Sendo as raízes do polinômio = [0, D], ou seja, a taxa de morte é perto de zero
    no início e no final da infecção, e na metade ela atinge seu ápice, que sempre
    será igual à Taxa de morte inserida pelo usuário.*/
calc_taxa_morte(Infectado, Taxa):-
    dias_cura(Dia), %Pesquisa a quantidade de dias para a doença se curar completamente
    tempo_infec(Infectado, TempoInfec), %Descobre o tempo de infecção do doente

    A is ((-4)*Taxa)/Dia**2, %Faz o calculo de A e B como coeficientes da equação quadrática
    B is (4*Taxa)/Dia,

    NovaTaxa is TempoInfec**2*A + TempoInfec*B, %NovaTaxa = f(x) e TempoInfec = x
    random(R), 
    R =< NovaTaxa,

    
    morte(Infectado). %Mata o infectado

calc_taxa_morte(_, _).

%Predicado que "mata" a pessoa, retirando todos os seus estados
morte(Pessoa) :-
    retractall(estado(Pessoa,_)),    
    retractall(tempo_infec(Pessoa,_)),
    retractall(tempo_isolado(Pessoa,_)), retractall(vacinado(Pessoa)),

    populacao(Total), NovaPop is Total - 1, %Diminui a população
    retract(populacao(Total)), assertz(populacao(NovaPop)),

    infectados(Infec), NovoInfec is Infec - 1, %Diminui o número de infectados também
    retract(infectados(Infec)), assertz(infectados(NovoInfec)),

    format("A pessoa ~w morreu.~n", Pessoa). %Imprime na tela para mostrar quem "morreu"

%Atualiza o tempo de infecção para todos os infectados, aumenta por 1 sempre.
att_temp_infec :-
    findall((P,X), tempo_infec(P,X), ListaTempos0), %Procura todos os tempos
 
    forall(member((P,X), ListaTempos0), ( %Para cada tempo, retira o anterior e insere tempo + 1.
        retractall(tempo_infec(P,_)), 
        X1 is X + 1, 
        assertz(tempo_infec(P, X1))
    )).

%Predicado não usado no código em si, para mostrar os resultados entre simulações
mostrar_estados :-
    findall(Pessoa, estado(Pessoa, suscetivel), ListaP), %Acha todos os suscetiveis,
    findall(Pessoa, estado(Pessoa, infectado), ListaInf), %infectados,
    findall((Pessoa, Tempo), tempo_isolado(Pessoa,Tempo), ListaIso), %isolados,
    findall(Pessoa, vacinado(Pessoa), ListaVac), %e vacinados.

    format("Pessoas suscetiveis: ~w~n", [ListaP]), %Imprime todas as listas na tela.
    format("Pessoas infectadas: ~w~n", [ListaInf]),
    format("Pessoas isoladas e tempo de isolamento: ~w~n", [ListaIso]),
    format("Pessoas vacinadas: ~w~n", [ListaVac]).

%Predicado não usado no código em si, para mostrar as conexões em fila.
mostrar_conexoes :-
    findall((Pessoa1, Pessoa2), conecta(Pessoa1, Pessoa2), Lista),
    format("Todas as conexoes: ~w~n", [Lista]).

%Predicado não usado no código em si, para mostrar a quantidade de pessoas em cada tipo
estatisticas:-
    infectados(Infec), populacao(Total), isolados(Isol), vacinados(Vac),
    Saudaveis is Total - Infec,
    writeln("Estatísticas: "),
    format("Saudáveis: ~w~n", Saudaveis),
    format("Infectados: ~w~n", Infec),
    format("Isolados: ~w~n", Isol),
    format("Vacinados: ~w~n", Vac).