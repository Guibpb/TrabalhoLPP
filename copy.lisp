;; Variáveis globais
(defparameter *taxa-infeccao* 0.3) ;; Taxa de infecção
(defparameter *taxa-recuperacao* 0.1) ;; Taxa de recuperação
(defparameter *taxa-morte* 0.1) ;; Taxa de morte
(defparameter *conexoes* nil) ;; Vetor de conexões entre indivíduos
(defparameter *qtd-conexoes* 0) ;; Quantidade de conexões inicializada em zero
(defparameter *n* 0) ;; Tamanho da população inicializado em zero
(defparameter *populacao* nil) ;; Vetor que armazena estados os infectado (I), suscetível (S) e recuperdado (R)
(defparameter *recuperados-por-dia* '()) ;; Lista dos recuperados por dia
(defparameter *limite-doenca* 10) ;; Tempo máximo de duração da doença
(defparameter *dias-infectado* nil) ;; Vetor que armazena os dias que um indvíduo está infectado
;; Rede social de mundo pequeno: 
;; evita grau 0, e cresce devagar
(defun qtd-conexoes (n)
    (max 1 (round (* 2 (log n))))
)
;; Recebe o input do usuário para definir o tamanho da população
(defun tamanho-populacao ()
    (format t "Digite o tamanho da populacao: ")
    (finish-output)
    (let ((tam (read)))
    ;; Verifica se a entrada do usuário é um número inteiro positivo
      (if (and (numberp tam) (integerp tam) (> tam 0))
          tam
          ;; Caso o usuário digite uma entrada inválida receberá um aviso até digitar algo válido.
          (progn
            (format t "Entrada invalida. Tente novamente.~%")
            (tamanho-populacao))))
)
;; Gera uma rede de conexões sociais armazenada como listas dentro de um vetor
(defun gerar-conexoes ()
  (loop for i from 0 below *n* do
    (let ((num-conexoes (random (+ 1 (* 2 *qtd-conexoes*)))) ;; variação +/- média
          (conexoes '()))
      (loop repeat num-conexoes do
        (let ((contato (random *n*))) ;; evita auto-conexao e duplicatas
          (unless (or (= contato i) (member contato conexoes))
            (setf conexoes (append conexoes (list contato))))))
      (setf (aref *conexoes* i) conexoes)))
)

(defun metodos-prevencao ()
    (let  ((isolados '()) (vacinados '()))
        ;; Infectados são escolhidos aleatoriamente para se isolarem (40% de chance)
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "I")
            (when (< (random 1.0) 0.4)
            (setf (aref *populacao* i) "IS")
            (setf isolados (append isolados (list i)))))
        )
        ;; Recuperados são escolhidos aleatoriamente para se vacinarem (30% de chance)
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "R")
            (when (< (random 1.0) 0.3) ;; 30% de chance de vacinar
            (setf (aref *populacao* i) "V")
            (setf vacinados (append vacinados (list i)))))
        )
        ;; Suscetíveis são escolhidos aleatoriamente para se vacinarem (20% de chance)
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "S")
            (when (< (random 1.0) 0.3)
            (setf (aref *populacao* i) "V")
            (setf vacinados (append vacinados (list i)))))
        )
    (values vacinados isolados))
)

(defun conta-dias-infectado ()
    (loop for i from 0 below *n* do
        (when (string= (aref *populacao* i) "I")
        (incf (aref *dias-infectado* i))))
)

(defun infecta ()
    (let ((novos-infectados '()))
        (loop for i from 0 below *n* do
            (when (string= (aref *populacao* i) "I")
                (dolist (contato (aref *conexoes* i))
                    (when (and (string= (aref *populacao* contato) "S")
                            (< (random 1.0) *taxa-infeccao*)
                          )
                        (setf novos-infectados (append novos-infectados (list contato)))))))
    (remove-duplicates novos-infectados))
)

(defun recupera ()
    (let ((novos-recuperados '()))
        (loop for i from 0 below *n* do
        (when (string= (aref *populacao* i) "I")
        (let ((dias (aref *dias-infectado* i)))
            (when (or (> dias *limite-doenca*) (< (random 1.0) *taxa-recuperacao*))
            (setf novos-recuperados (append novos-recuperados (list i)))))))
    (remove-duplicates novos-recuperados))
)

(defun reinfecta ()
    (let ((novos-reinfectados '()))
        (loop for i from 0 below *n* do
        (when (string= (aref *populacao* i) "R") 
        ;; Chance de perder imunidade e voltar a ser suscetível (5%)
        (when (< (random 1.0) 0.05)
            (setf (aref *populacao* i) "S")
            (setf novos-reinfectados (append novos-reinfectados (list i))))))
(remove-duplicates novos-reinfectados))
)

(defun morte ()
    (let ((novas-mortos '()))
        (loop for i from 0 below *n* do
    (when (string= (aref *populacao* i) "I")
    (when (< (random 1.0) *taxa-morte*)
          (setf (aref *populacao* i) "M")
    (setf novas-mortos (append novas-mortos (list i))))))
(remove-duplicates novas-mortos))
)

(defun atualizar-populacao (novos-infectados novos-recuperados isolados vacinados) ;; Listas passadas como parâmetro
    (dolist (i novos-infectados)
        (setf (aref *populacao* i) "I"))
    (dolist (i novos-recuperados)
        (setf (aref *populacao* i) "R"))
    (setf *recuperados-por-dia* (append *recuperados-por-dia* (list novos-recuperados)))
)

(defun mostrar-resultados (novos-recuperados novos-mortos isolados vacinados)
    (let ((infectados (loop for i from 0 below *n*
                       when (string= (aref *populacao* i) "I")
                       collect i)))
    (format t "Infectados hoje (~A): ~A~%" (length infectados) infectados)
    (format t "Recuperados hoje (~A): ~A~%" (length novos-recuperados) novos-recuperados)
    (format t "Falecidos hoje (~A): ~A~%" (length novos-mortos) novos-mortos)
    (format t "Isolados hoje (~A): ~A~%" (length isolados) isolados)
    (format t "Vacinados hoje (~A): ~A~%" (length vacinados) vacinados))
)

(defun simular ()
    (conta-dias-infectado)
    ;; Os valores retornados por metodos-prevencao são passados como lista para serem acessados.
    (let* ((valores (multiple-value-list(metodos-prevencao))) 
        (novos-infectados (infecta))
        (novos-recuperados (recupera))
        (novos-mortos (morte))
        (vacinados (first valores)) ;; Seleciona o primeiro elemento da lista e o nomeia.
        (isolados (second valores))) ;; Seleciona o segundo elemento da lista e o nomeia.
    (atualizar-populacao novos-infectados novos-recuperados isolados vacinados)
    (mostrar-resultados novos-recuperados novos-mortos isolados vacinados)
    )
)

(defun main ()

    (setf *random-state* (make-random-state t))
    (setf *n* (tamanho-populacao))
    (setf *qtd-conexoes* (qtd-conexoes *n*))
    (setf *conexoes* (make-array *n* :initial-element nil))
    (setf *populacao* (make-array *n* :initial-element "S"))
    (setf *dias-infectado* (make-array *n* :initial-element 0))
    (setf *recuperados-por-dia* '())
  ;; Estabelece o paciente zero.
    (setf (aref *populacao* 0) "I")
    (gerar-conexoes)
    (let ((dia 1))
        (loop while (some (lambda (estado) (string= estado "I")) (coerce *populacao* 'list)) do
        (format t "DIA ~A~%" dia)
        (simular)
        (format t "~%")
        (incf dia)))
)
(main)