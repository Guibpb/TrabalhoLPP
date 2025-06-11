#|
*Alunos:
Guilherme Borges de Pádua Barbosa - 15653045
Gabriel Hupfer Rhigi - 15508612
Tainara Lopes de Oliveira Terassaka - 13675501
|#

;; Variáveis globais
(defparameter *taxa-infeccao* 0.3) ;; Taxa de infecção.
(defparameter *taxa-recuperacao* 0.05) ;; Taxa de recuperação.
(defparameter *taxa-morte* 0.03) ;; Taxa de morte.
(defparameter *conexoes* nil) ;; Vetor de conexões entre indivíduos.
(defparameter *qtd-conexoes* 0) ;; Quantidade de conexões inicializadas em zero.
(defparameter *n* 0) ;; Tamanho da população inicializado em zero.
(defparameter *populacao* nil) ;; Vetor que armazena estados dos indivíduos.
(defparameter *recuperados-por-dia* '()) ;; Lista dos recuperados por dia.
(defparameter *limite-doenca* 12) ;; Tempo máximo de duração da doença em dias.
(defparameter *dias-infectado* nil) ;; Vetor que armazena quantos dias cada indivíduo está infectado.
(defparameter *explosao-infectados* nil) ;; Quantidade de infectados que define uma "explosão de casos".
#| 
Define o limite de conexões que um indivíduo pode ter em uma população de tamanho n, 
com base em uma escala logarítmica (não linear). Retorna o valor arredondado de 2·log(n), 
mas nunca inferior a 1 — garantindo ao menos uma conexão e evitando valores nulos.
|#
(defun qtd-conexoes (n)
    (max 1 (round (* 2 (log n))))
)
#|
Recebe o input do usuário para definir o tamanho da população então:
- Verifica se a entrada do usuário é um número inteiro positivo;
- Caso o usuário digite uma entrada inválida receberá um aviso até digitar algo válido.
|#
(defun tamanho-populacao ()
    (format t "Digite o tamanho da populacao: ")
    (finish-output)
    (let ((tam (read)))
        (if (and (numberp tam) (integerp tam) (> tam 0))
        tam
            (progn
                (format t "Entrada invalida. Tente novamente.~%")
                (tamanho-populacao)
            )
        )
    )
)
#|
Gera uma rede social representada por um vetor, onde cada índice corresponde a um indivíduo
e armazena uma lista dos indivíduos conectados a ele.
Para cada indivíduo:
- Define um número aleatório de conexões entre 0 e aproximadamente 2 vezes qtd-conexoes
- Gera conexões únicas, evitando auto-conexão e duplicatas;
- Armazena a lista de conexões no vetor global *conexoes*.
|#
(defun gerar-conexoes ()
    (loop for i from 0 below *n* do
        (let ((num-conexoes (random (+ 1 (* 2 *qtd-conexoes*)))) 
            (conexoes '())
            )
            (loop repeat num-conexoes do
                (let ((contato (random *n*)))
                    (unless (or (= contato i) (member contato conexoes))
                    (setf conexoes (append conexoes (list contato)))
                    )
                )
            )
        (setf (aref *conexoes* i) conexoes)
        )
    )
)
#|
Simula a propagação de uma infecção em uma população:
- Percorre todos os indivíduos da população;
- Para cada indivíduo infectado:
    - Verifica seus contatos na rede de conexões;
    - Para cada contato suscetível ("S"), infecta com probabilidade definida em *taxa-infeccao*;
- Retorna a lista dos novos indivíduos infectados, sem duplicatas.
|#
(defun infecta ()
    (let ((novos-infectados '()))
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "I")
                (dolist (contato (aref *conexoes* i))
                    (when (and (string= (aref *populacao* contato) "S") (< (random 1.0) *taxa-infeccao*))
                        (setf novos-infectados (append novos-infectados (list contato)))
                    )
                )
            )
        )
    (remove-duplicates novos-infectados)
    )
)
#|
Simula a recuperação de indivíduos infectados na população:
- Percorre todos os indivíduos da população;
- Para cada indivíduo infectado:
  - Verifica há quantos dias está infectado;
  - Se passou do limite máximo de dias (*limite-doenca*), ou
    com probabilidade *taxa-recuperacao* após ao menos 1 dia infectado,
    marca o indivíduo para recuperação;
- Retorna a lista dos novos indivíduos recuperados, sem duplicatas.
|#
(defun recupera ()
    (let ((novos-recuperados '()))
        (loop for i from 0 below *n* do
            (when (or (string= (aref *populacao* i) "I") (string= (aref *populacao* i) "IS"))
                (let ((dias (aref *dias-infectado* i)))
                    (when (or (> dias *limite-doenca*) (and (> dias 0) (< (random 1.0) *taxa-recuperacao*)))
                    (setf novos-recuperados (append novos-recuperados (list i)))
                    )
                )
            )
        )
    (remove-duplicates novos-recuperados)
    )
)
#|
Simula a perda de imunidade e reinfecção de indivíduos recuperados:
- Percorre todos os indivíduos da população;
- Para cada indivíduo recuperado:
  - Aplica uma chance fixa (10%) de perder imunidade e voltar a ser suscetível ("S");
  - Se ocorrer perda de imunidade, atualiza o estado e registra o indivíduo como reinfectado;
- Retorna a lista dos indivíduos que perderam imunidade, sem duplicatas.
|#
(defun reinfecta ()
    (let ((novos-reinfectados '()))
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "R") 
                (when (< (random 1.0) 0.1)
                    (setf (aref *populacao* i) "S")
                    (setf novos-reinfectados (append novos-reinfectados (list i)))
                )
            )
        )
(remove-duplicates novos-reinfectados))
)
#|
Simula a morte de indivíduos infectados na população:
- Percorre todos os indivíduos;
- Para cada infectado ("I" ou "IS") com pelo menos 5 dias de infecção:
  - Aplica uma chance de morte definida por *taxa-morte*;
  - Se ocorrer, atualiza o estado para "M" (morto) e registra o indivíduo;
- Retorna a lista dos novos mortos, sem duplicatas.
|#
(defun morte ()
    (let ((novos-mortos '()))
        (loop for i from 0 below *n* do
            (when (or (string= (aref *populacao* i) "I") (string= (aref *populacao* i) "IS"))
                (let ((dias (aref *dias-infectado* i)))
                    (when (and (>= dias 5) (< (random 1.0) *taxa-morte*))
                        (setf (aref *populacao* i) "M")
                        (setf novos-mortos (append novos-mortos (list i))))
                )
            )
        )
    (remove-duplicates novos-mortos))
)
#|
Incrementa em 1 o contador de dias infectado para todos os indivíduos atualmente infectados.
- Percorre todos os indivíduos;
- Para cada infectado ("I" ou "IS"), incrementa o valor correspondente em *dias-infectado*.
|#
(defun conta-dias-infectado ()
    (loop for i from 0 below *n* do
        (when (or (string= (aref *populacao* i) "I") (string= (aref *populacao* i) "IS"))
        (incf (aref *dias-infectado* i))
        )
    )
)
#|
Conta o número de indivíduos atualmente infectados na população.
- Percorre todos os indivíduos;
- Incrementa um contador para cada indivíduo no estado "I" e "IS";
- Retorna o total de infectados.
|#
(defun contar-infectados ()
    (let ((nro-infectados 0))
        (loop for i from 0 below *n* do
            (when (or (string= (aref *populacao* i) "I") (string= (aref *populacao* i) "IS"))
            (incf nro-infectados)
            )
        )
    nro-infectados
    )
)
#|
Aplica métodos de prevenção na população com base no número atual de infectados:
- Para cada indivíduo:
  - Infectados ("I") têm 20% de chance de se isolarem, mudando para "IS";
  - Após atingir o limite *explosao-infectados*:
    - Recuperados ("R") têm 10% de chance de se vacinarem ("V");
    - Suscetíveis ("S") têm 5% de chance de se vacinarem ("V");
- Atualiza os estados na população e registra os índices dos vacinados e isolados;
- Retorna dois valores: a lista de vacinados e a lista de isolados.
|#
(defun metodos-prevencao (infectados)
    (let ((isolados '()) (vacinados '()))
        (loop for i from 0 below *n* do
            (let ((estado (aref *populacao* i)))
                (cond
                    ((string= estado "I")
                        (when (< (random 1.0) 0.2)
                            (setf (aref *populacao* i) "IS")
                            (setf isolados (append isolados (list i)))
                        )
                    )
                    ((and (>= infectados *explosao-infectados*) (string= estado "R"))
                        (when (< (random 1.0) 0.1)
                            (setf (aref *populacao* i) "V")
                            (setf vacinados (append vacinados (list i)))
                        )
                    )
                    ((and (>= infectados *explosao-infectados*) (string= estado "S"))
                        (when (< (random 1.0) 0.05)
                            (setf (aref *populacao* i) "V")
                            (setf vacinados (append vacinados (list i)))
                        )
                    )
                )
            )
        )
    (values vacinados isolados)
    )
)
#|
Remove conexões associadas aos indivíduos isolados da rede social:
- Para cada indivíduo isolado ("IS"):
  - Remove sua lista de conexões (seta para nil);
  - Remove sua referência da lista de conexões de todos os outros indivíduos.
|#
(defun remover-isolados ()
    (loop for i from 0 below *n* do
        (when (string= (aref *populacao* i) "IS")
            (setf (aref *conexoes* i) nil)
            (loop for j from 0 below *n* do
                (let ((conexoes-atuais (aref *conexoes* j)) (nova-lista '()))
                    (dolist (novo conexoes-atuais)
                        (unless (= novo i)
                        (setf nova-lista (append nova-lista (list novo)))
                        )
                    )
                (setf (aref *conexoes* j) nova-lista)
                )
            )
        )
    )
)
#|
Atualiza os estados dos indivíduos na população com base nas listas recebidas:
- Para cada índice em "novos-infectados", atualiza o estado para "I" (infectado);
- Para cada índice em "novos-recuperados", atualiza o estado para "R" (recuperado);
- Registra a lista diária de recuperados adicionando "novos-recuperados" ao vetor global *recuperados-por-dia*.
|#
(defun atualizar-populacao (novos-infectados novos-recuperados isolados vacinados) ;; Listas passadas como parâmetro
    (dolist (i novos-infectados)
        (setf (aref *populacao* i) "I")
    )
    (dolist (i novos-recuperados)
        (setf (aref *populacao* i) "R")
    )
    (setf *recuperados-por-dia* (append *recuperados-por-dia* (list novos-recuperados)))
)
#|
Exibe o resumo diário dos estados da população, mostrando as listas e quantidades
de infectados, recuperados, mortos, reinfectados, isolados e vacinados.
Obs.: Uso do "collect" é para acumular automaticamente em uma lista os índices dos infectados,
evitando manipulações manuais de listas.
|#
(defun mostrar-resultados (novos-recuperados novos-mortos isolados vacinados novos-reinfectados) ;; Listas passadas como parâmetro
    (let ((infectados (loop for i from 0 below *n*
                       when (string= (aref *populacao* i) "I")
                       collect i)))
        (format t "Infectados hoje (~A): ~A~%" (length infectados) infectados)
        (format t "Recuperados hoje (~A): ~A~%" (length novos-recuperados) novos-recuperados)
        (format t "Falecidos hoje (~A): ~A~%" (length novos-mortos) novos-mortos)
        (format t "Reinfectados hoje (~A): ~A~%" (length novos-reinfectados) novos-reinfectados)
        (format t "Isolados hoje (~A): ~A~%" (length isolados) isolados)
        (format t "Vacinados hoje (~A): ~A~%" (length vacinados) vacinados)
    )
)
#|
Executa uma rodada da simulação da doença:
- Calcula novos infectados, recuperados, mortos e reinfectados;
- Conta infectados atuais;
- Aplica métodos de prevenção (vacinados e isolados);
- Atualiza população e remove conexões de isolados;
- Mostra resultados do dia;
- Incrementa dias de infecção.
Obs.: O uso de multiple-value-list captura os múltiplos valores retornados
por metodos-prevencao e os converte em uma lista. Em seguida, first e second
extraem, respectivamente, o primeiro e o segundo elemento dessa lista, atribuindo-os a variáveis.
|#
(defun simular ()
    (let* ((novos-infectados (infecta))
           (novos-recuperados (recupera))
           (novos-mortos (morte))
           (infectados (contar-infectados))
           (valores (multiple-value-list(metodos-prevencao infectados))) 
           (vacinados (first valores))
           (isolados (second valores))
           (novos-reinfectados (reinfecta)))
    (atualizar-populacao novos-infectados novos-recuperados isolados vacinados)
    (remover-isolados)
    (mostrar-resultados novos-recuperados novos-mortos isolados vacinados novos-reinfectados)
    (conta-dias-infectado)
    )
)
#|
Executa a simulação principal:
- Inicializa parâmetros, população e conexões sociais;
- Define paciente zero;
- Roda um loop diário que simula a propagação enquanto houver infectados;
- Finaliza quando não há mais infectados ativos.
|#
(defun main ()
    (setf *random-state* (make-random-state t))
    (setf *n* (tamanho-populacao))
    (setf *explosao-infectados* (/ *n* 5))
    (setf *qtd-conexoes* (qtd-conexoes *n*))
    (setf *conexoes* (make-array *n* :initial-element nil))
    (setf *populacao* (make-array *n* :initial-element "S"))
    (setf *dias-infectado* (make-array *n* :initial-element 0))
    (setf *recuperados-por-dia* '())
    (setf (aref *populacao* 0) "I")
    (gerar-conexoes)
    (let ((dia 1))
        (loop while (some (lambda (estado) 
                                  (or (string= estado "I") (string= estado "IS"))) 
                                  (coerce *populacao* 'list))
            do
            (format t "DIA ~A~%" dia)
            (simular)
            (format t "~%")
            (incf dia)
        )
    )
)
(main)