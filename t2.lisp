(defparameter *tamanho-populacao* 10)
(defparameter *taxa-infeccao* 0.3)
(defparameter *taxa-recuperacao* 0.3)

(defstruct individuo'
  id
  estado) ;; :S, :I, :R

(defun criar-populacao ()
  (let ((populacao (make-array *tamanho-populacao*)))
       (dotimes (i *tamanho-populacao*)
       (setf (aref populacao i)
             (make-individuo :id (format nil "p~D" i)
                             :estado (if (= i 0) :I :S))))
  populacao))

(defun infecta (populacao)
  (dotimes (i *tamanho-populacao*)
    (let ((ind (aref populacao i)))
      (when (eq (individuo-estado ind) :I)
        (dotimes (j 3)
          (let* ((alvo-index (random *tamanho-populacao*))
                 (alvo (aref populacao alvo-index)))
            (when (and (eq (individuo-estado alvo) :S)
                       (< (random 1.0) *taxa-infeccao*))
              (setf (individuo-estado alvo) :I))))))))

(defun recupera (populacao)
  (let ((curados '()))
    (dotimes (i *tamanho-populacao*)
      (let ((ind (aref populacao i)))
        (when (eq (individuo-estado ind) :I)
          (when (< (random 1.0) *taxa-recuperacao*)
            (setf (individuo-estado ind) :R)
            (push (individuo-id ind) curados)))))
    (nreverse curados)))

(defun lista-infectados (populacao)
  (let ((lista '()))
    (dotimes (i *tamanho-populacao*)
      (let ((ind (aref populacao i)))
        (when (eq (individuo-estado ind) :I)
          (push (individuo-id ind) lista))))
    (nreverse lista)))

(defun simular-sir (dias)
  (let ((populacao (criar-populacao)))
    (loop for dia from 1 to dias do
      (format t "~%Dia ~D -~%" dia)
      (let ((infectados (lista-infectados populacao)))
        (format t "Infectados: ~A~%" infectados))

      (infecta populacao)

      (let ((curados (recupera populacao)))
        (dolist (c curados)
          (format t "Pessoa ~A se curou.~%" c)))

      (format t "[")
      (let ((primeiro t))
        (dolist (i (lista-infectados populacao))
          (unless primeiro (format t ", "))
          (format t "(~A,0)" i)
          (setf primeiro nil)))
      (format t "]~%")

      (let ((infectados (lista-infectados populacao)))
        (if (equal infectados nil)
            (progn
              (format t "Epidemia terminou no dia ~D.~%" dia)
              (return)))))

    (format t "~%Simulação finalizada.~%")))

(defun mostrar-parametros ()
  (format t "~%Parâmetros atuais:~%")
  (format t "Tamanho da população: ~D~%" *tamanho-populacao*)
  (format t "Taxa de infecção: ~,2f~%" *taxa-infeccao*)
  (format t "Taxa de recuperação: ~,2f~%" *taxa-recuperacao*)
  (format t "~%"))

(defun ler-dias ()
  (format t "Digite a quantidade de dias para simular: ")
  (finish-output)
  (loop
     for linha = (read-line)
     for dias = (ignore-errors (parse-integer linha :junk-allowed t))
     when (and dias (> dias 0))
       do (return dias)
     do (format t "Número inválido! Por favor, digite um número inteiro positivo: ")
        (finish-output)))

(defun comando ()
  (format t "~%Digite um comando ('start', 'info', 'exit'): ")
  (finish-output)
  (let ((input (string-downcase (read-line))))
    (cond
            ((string= input "exit")
       (format t "~%Saindo...~%")
       (finish-output)
       #+sbcl (sb-ext:quit)
       #+lispworks (lw:quit))
      ((string= input "info")
       (mostrar-parametros)
       (comando))
      ((string= input "start")
       (let ((dias (ler-dias)))
         (simular-sir dias))
       (comando))
      (t
       (format t "Comando inválido. Tente novamente.~%")
       (comando)))))

(defun main ()
  (format t "~%+-----------------------------------------+~%")    
    (format t "| Bem vindo ao Simulador de Epidemias     |~%")
    (format t "+-----------------------------------------+~%")
    (format t "| Digite 'start' para iniciar uma sessao  |~%")
    (format t "| Digite 'info' para mostrar parâmetros   |~%")
    (format t "| Digite 'exit' para sair do simulador    |~%")
    (format t "+-----------------------------------------+~%")
    (comando))

(main)