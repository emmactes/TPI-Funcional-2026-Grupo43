;; Iteracion 2. 
;; Extension 1
;; Carga de libreria externa mediante Quicklisp
(ql:quickload "local-time")
;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura 
;; ESTRATEGIA: Seleccion Condicional Multiple 
;; IMPACTO: No destructiva 
;; ========================================================
;; Requerimiento 1: Estados de Transicion
(defun transicion (color-actual color-siguiente)
  (cond
    ((and (eq color-actual 'en-rojo) (eq color-siguiente 'rojo-intermitente))
      (list color-actual "cambiar-a-rojo-intermitente"))
    ((and (eq color-actual 'rojo-intermitente) (eq color-siguiente 'en-verde))
      (list color-actual "cambiar-a-verde"))
    ((and (eq color-actual 'en-verde) (eq color-siguiente 'verde-intermitente))
      (list color-actual "cambiar-a-verde-intermitente"))
    ((and (eq color-actual 'verde-intermitente) (eq color-siguiente 'en-amarillo))
      (list color-actual "cambiar-a-amarillo"))
    ((and (eq color-actual 'en-amarillo) (eq color-siguiente 'amarillo-intermitente))
      (list color-actual "cambiar-a-amarillo-intermitente"))
    ((and (eq color-actual 'amarillo-intermitente) (eq color-siguiente 'en-rojo))
      (list color-actual "cambiar-a-rojo"))
    (T (list color-actual 'accion-por-defecto))
  )
)

;; ========================================================
;; FUNCION: temporizador
;; NATURALEZA: Pura (Dado un mismo tiempo, siempre retorna el mismo átomo)
;; ESTRATEGIA: Matematica y Seleccion Condicional (Uso de mod y cond)
;; IMPACTO: No destructiva
;; Requerimiento 2: Temporizador Automatico
(defun temporizador (epoch)
  (let ((segundo-ciclo (mod epoch 225))) ;; El ciclo es de 225s
    (cond
      ((< segundo-ciclo 90) 'en-rojo)
      ((< segundo-ciclo 93) 'rojo-intermitente)
      ((< segundo-ciclo 213) 'en-verde)      ;; 93 + 120
      ((< segundo-ciclo 216) 'verde-intermitente) ;; 213 + 3
      ((< segundo-ciclo 222) 'en-amarillo)   ;; 216 + 6
      (T 'amarillo-intermitente)             ;;
    )
  )
)

;; ========================================================
;; FUNCION: auditoria
;; NATURALEZA: Impura (Genera un efecto secundario: I/O en terminal)
;; ESTRATEGIA: Salida por pantalla y formateo de tiempo (Uso de let)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 3: Sistema de Auditoria
(defun auditoria (tiempo-unix)
  (let ((estado-actual (temporizador tiempo-unix))
        (fecha-formateada (local-time:format-timestring nil 
                            (local-time:unix-to-timestamp tiempo-unix)
                            :format '("[" :year "-" (:month 2) "-" (:day 2) " " 
                                      (:hour 2) ":" (:min 2) ":" (:sec 2) "]"))))
(cond
      ((eq estado-actual 'en-rojo) 
       (format t "Tiempo ~a: cambio de AMARILLO-INTERMITENTE a ROJO~%" fecha-formateada))
      ((eq estado-actual 'rojo-intermitente) 
       (format t "Tiempo ~a: cambio de ROJO a ROJO-INTERMITENTE~%" fecha-formateada))
      ((eq estado-actual 'en-verde) 
       (format t "Tiempo ~a: cambio de ROJO-INTERMITENTE a VERDE~%" fecha-formateada))
      ((eq estado-actual 'verde-intermitente) 
       (format t "Tiempo ~a: cambio de VERDE a VERDE-INTERMITENTE~%" fecha-formateada))
      ((eq estado-actual 'en-amarillo) 
       (format t "Tiempo ~a: cambio de VERDE-INTERMITENTE a AMARILLO~%" fecha-formateada))
      ((eq estado-actual 'amarillo-intermitente) 
       (format t "Tiempo ~a: cambio de AMARILLO a AMARILLO-INTERMITENTE~%" fecha-formateada))
    )
  )
)

;; ========================================================
;; FUNCION: duracion-ciclo
;; NATURALEZA: Pura (Dado un mismo tiempo-ciclo retorna un valor booleano)
;; ESTRATEGIA: Logica condicional (Implementada mediante cond)
;; IMPACTO: No destructiva (devuelve un booleano)
;; ========================================================
;; Requerimiento 4.a: Análisis de Ciclos Semafóricos
(defun duracion-ciclo (tiempo-ciclo)
    (cond
        ((and (> tiempo-ciclo 35) (< tiempo-ciclo 150)) T)
        (t nil)
    )
)

;; ========================================================
;; FUNCION: recomendacion-ciclo
;; NATURALEZA: Pura (Retorna una cadena de texto, no genera efectos secundarios)
;; ESTRATEGIA: Condicional (Evaluacion de multiples casos mediante cond)
;; IMPACTO: No destructiva 
;; ========================================================
;; Requerimiento 4.b: Analisis de Ciclos Semaforicos
(defun recomendacion-ciclo (tiempo)
   (cond
      ((< tiempo 35) "Tiempo de espera muy corto, se recomienda extenderlo")
      ((> tiempo 150) "Tiempo de espera muy largo, se recomienda reducirlo")
      (t "Tiempo de espera optimo")
   )
)

;; ========================================================
;; FUNCION: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Matematica/Composicion (Uso de let y truncate)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 5: Planificacion Temporal
(defun ciclos-por-tiempo (minutos)
    ;; Actualizado al ciclo de 225 segundos
    (let ((ciclos-completos (truncate (/ (* minutos 60) 225))))
        (format nil "Numero de ciclos completados ~a" ciclos-completos)
    )
)

;; ========================================================
;; FUNCION: informe-dist-temp
;; NATURALEZA: Pura
;; ESTRATEGIA: Matematica/Composicion (Uso de let y truncate)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 6: Informe de Distribución Temporal
(defun informe-dist-temp ()
  ;; Se actualizan los porcentajes con el divisor 225 y los nuevos estados
  (let ((calculo-rojo (truncate (/ (* 90 100.0) 225)))
        (calculo-rojo-intermitente (truncate (/ (* 3 100.0) 225)))
        (calculo-verde (truncate (/ (* 120 100.0) 225)))
        (calculo-verde-intermitente (truncate (/ (* 3 100.0) 225)))
        (calculo-amarillo (truncate (/ (* 6 100.0) 225)))
        (calculo-amarillo-intermitente (truncate (/ (* 3 100.0) 225))))
    (format nil "Rojo: ~a% (Int: ~a%) | Verde: ~a% (Int: ~a%) | Amarillo: ~a% (Int: ~a%)"
            calculo-rojo calculo-rojo-intermitente calculo-verde calculo-verde-intermitente 
            calculo-amarillo calculo-amarillo-intermitente)
  )
)

;; Extension 2
(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt"
                          :direction :output
                          :if-exists :supersede 
                          :if-does-not-exist :create)
    (format stream "Informe de Ejecucion del Sistema Semaforico~%")
    (format stream "=========================================~%")
    (mapcar #'(lambda (tiempo-unix)
               (let ((fecha-legible (local-time:format-timestring nil 
                                    (local-time:unix-to-timestamp tiempo-unix)
                            :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)))
                      (color-actual (temporizador tiempo-unix))
                      (color-anterior
                      (cond
                        ((equal (temporizador tiempo-unix) 'rojo) 'verde)
                        ((equal (temporizador tiempo-unix) 'amarillo) 'rojo)
                        ((equal (temporizador tiempo-unix) 'verde) 'amarillo))))
               (format stream "~a - Transicion: ~a -> ~a~%"
                          fecha-legible
                          color-anterior
                          color-actual)))
              datos)
    (format stream "~% --- Fin del Informe ---")))