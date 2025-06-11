#|
*Alunos:
Guilherme Borges de Pádua Barbosa - 15653045
Gabriel Hupfer Rhigi - 15508612
Tainara Lopes de Oliveira Terassaka - 13675501
|#

;; Variáveis globais
(defparameter *taxa-infeccao* 0.3) ;; Taxa de infecção.
(defparameter *taxa-recuperacao* 0.6) ;; Taxa de recuperação.
(defparameter *taxa-morte* 0.03) ;; Taxa de morte.
(defparameter *taxa-reinfeccao* 0.005) ;; Taxa de reinfecção.
(defparameter *conexoes* nil) ;; Vetor de conexões entre indivíduos.
(defparameter *qtd-conexoes* 0) ;; Quantidade de conexões inicializadas em zero.
(defparameter *n* 0) ;; Tamanho da população inicializado em zero.
(defparameter *populacao* nil) ;; Vetor que armazena estados dos indivíduos.
(defparameter *recuperados-por-dia* '()) ;; Lista dos recuperados por dia.
(defparameter *limite-doenca* 10) ;; Tempo máximo de duração da doença em dias.
(defparameter *dias-infectado* nil) ;; Vetor que armazena quantos dias cada indivíduo está infectado.
(defparameter *explosao-infectados* nil) ;; Quantidade de infectados que define uma "explosão de casos".
#|
Recebe e valida o tamanho da população como um número inteiro positivo, repetindo até obter valor válido.
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
Define o limite de conexões que um indivíduo pode ter com base em uma escala logarítmica,
mínimo 1 e no máximo aproximadamente 2·log(n).
|#
(defun qtd-conexoes (n)
    (max 1 (round (* 2 (log n))))
)
#|
Cria uma rede social onde cada indivíduo tem uma lista única de conexões aleatórias, 
evitando auto-conexão e duplicatas, armazenada em conexoes.
|#
(defun gerar-conexoes ()
    (loop for i from 0 below *n* do
        (let ((num-conexoes (+ 1 (random (* 2 *qtd-conexoes*))))
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
Simula a infecção propagando-se de infectados para contatos suscetíveis com uma certa probabilidade, 
retornando os novos infectados sem duplicatas.
|#
(defun infecta ()
    (let ((novos-infectados '()))
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "I")
                (dolist (contato (aref *conexoes* i))
                    (when (and contato (string= (aref *populacao* contato) "S") (< (random 1.0) *taxa-infeccao*))
                        (setf novos-infectados (append novos-infectados (list contato)))
                    )
                )
            )
        )
    (remove-duplicates novos-infectados)
    )
)
#|
Simula a recuperação de infectados com base no tempo de infecção e numa probabilidade, 
retornando os recém-recuperados sem duplicatas.
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
Simula a perda de imunidade com chance fixa, reinfectando indivíduos recuperados 
e retornando os casos sem duplicatas.
|#
(defun reinfecta ()
    (let ((novos-reinfectados '()))
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "R") 
                (when (< (random 1.0) *taxa-reinfeccao*)
                    (setf (aref *populacao* i) "I")
                    (setf novos-reinfectados (append novos-reinfectados (list i)))
                )
            )
        )
(remove-duplicates novos-reinfectados))
)
#|
Simula mortes entre infectados com ≥5 dias de infecção, 
aplicando uma chance fixa e retornando os novos casos fatais sem duplicatas.
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
|#
(defun conta-dias-infectado ()
    (loop for i from 0 below *n* do
        (when (or (string= (aref *populacao* i) "I") (string= (aref *populacao* i) "IS"))
        (incf (aref *dias-infectado* i))
        )
    )
)
#|
Retorna o total de indivíduos nos estados "I" ou "IS", contando todos os atualmente infectados.
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
Aplica medidas preventivas com base no número de infectados:
- Infectados podem entrar em isolamento ("IS");
- Se os infectados superam explosao-infectados, suscetíveis e recuperados podem se vacinar ("V");
- Retorna dois valores, a listas de vacinados e  a lista de isolados.
|#
(defun metodos-prevencao (infectados)
    (let ((isolados '()) (vacinados '()))
        (loop for i from 0 below *n* do
            (let ((estado (aref *populacao* i)))
                (cond
                    ((and (string= estado "I") (not (= i 0)))
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
Remove conexões associadas aos indivíduos isolados da rede social.
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
Atualiza os estados dos indivíduos conforme os eventos do dia (infecção, recuperação, morte etc.) 
e registra os recuperados.
|#
(defun atualizar-populacao (novos-infectados novos-recuperados isolados vacinados novos-reinfectados novos-mortos) ;; Listas passadas como parâmetro
    (dolist (i novos-infectados)
        (setf (aref *populacao* i) "I")
    )
    (dolist (i novos-recuperados)
        (setf (aref *populacao* i) "R")
    )
      (dolist (i novos-mortos)
        (setf (aref *populacao* i) "M")
    )
    (dolist (i vacinados)
        (setf (aref *populacao* i) "V")
    )
    (dolist (i isolados)
        (setf (aref *populacao* i) "IS")
    )
    (setf *recuperados-por-dia* (append *recuperados-por-dia* (list novos-recuperados)))
)

#|
Exibe o resumo diário dos estados da população, mostrando as listas e quantidades
de infectados, recuperados, mortos, reinfectados, isolados e vacinados.
Obs.: Uso do "collect" é para acumular automaticamente em uma lista os índices dos infectados,
evitando manipulações manuais de listas. Uso de "length" é para obter o tamanho dessas listas,
permitindo exibir a quantidade de indivíduos em cada estado.
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
Executa um ciclo da simulação: processa infecções, recuperações, mortes, reinfecções, 
prevenção, atualiza estados e conexões, exibe resultados e avança o tempo da infecção.
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
    (atualizar-populacao novos-infectados novos-recuperados isolados vacinados novos-reinfectados novos-mortos)
    (remover-isolados)
    (mostrar-resultados novos-recuperados novos-mortos isolados vacinados novos-reinfectados)
    (conta-dias-infectado)
    )
)
#|
Verifica se ainda existem indivíduos infectados na população para saber se a epidemia acabou.
Retorna T se encontrar ao menos um infectado.
|#
(defun tem-infectados ()
    (loop for i from 0 below *n*
        when (or (string= (aref *populacao* i) "I")
                 (string= (aref *populacao* i) "IS"))
        do (return t)
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
        (let ((condicao t))
            (loop
                do
                (format t "DIA ~A~%" dia)
                (simular)
                (format t "~%")
                (incf dia)
                (setf condicao (tem-infectados))
                while condicao
            )
        )
    )
    (format t "Paciente zero conexoes: ~A~%" (aref *conexoes* 0))
    (format t "Estado paciente zero: ~A~%" (aref *populacao* 0))
)
(main)