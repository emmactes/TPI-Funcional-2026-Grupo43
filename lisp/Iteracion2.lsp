;; Iteracion 2. 
;; Extension 1
;; Carga de librerías externas mediante Quicklisp
(ql:quickload "local-time")
;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Dados el mismo estado y color, siempre retorna la misma lista)
;; ESTRATEGIA: Seleccion Condicional Multiple (Implementada mediante cond y eq)
;; IMPACTO: No destructiva (Construye una nueva lista como salida)
;; ========================================================
;; Requerimiento 1: Estados de Transición
(defun transicion (color-actual color-siguiente)
  (cond
    ((and (eq color-actual 'en-rojo)
          (eq color-siguiente 'verde))
      (list color-actual "cambiar-a-verde"))

    ((and (eq color-actual 'en-verde)
          (eq color-siguiente 'amarillo))
      (list color-actual "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo)
          (eq color-siguiente 'rojo))
      (list color-actual "cambiar-a-rojo"))

    (T (list color-actual 'accion-por-defecto))
  )
)

;; ========================================================
;; FUNCION: temporizador
;; NATURALEZA: Pura (Dado un mismo tiempo, siempre retorna el mismo átomo)
;; ESTRATEGIA: Matemática y Selección Condicional (Uso de let, mod y cond)
;; IMPACTO: No destructiva
;; Requerimiento 2: Temporizador Automatico
(defun temporizador (epoch)
  (cond
    ((< (mod epoch 216) 90) 'en-rojo)
    ((< (mod epoch 216) 210) 'en-verde)
    (T 'en-amarillo)
  )
)

;; ========================================================
;; FUNCION: auditoria
;; NATURALEZA: Impura (Genera un efecto secundario: I/O en terminal)
;; ESTRATEGIA: Salida por pantalla y formateo de tiempo (Uso de let)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 3 - Integración Fase 2 (Quicklisp)
(defun auditoria (tiempo-unix)
  (let ((estado-actual (temporizador tiempo-unix))
        ;; Convertimos el tiempo unix y lo formateamos directamente como pide el TP
        (fecha-formateada (local-time:format-timestring nil 
                            (local-time:unix-to-timestamp tiempo-unix)
                            :format '("[" :year "-" (:month 2) "-" (:day 2) " " 
                                      (:hour 2) ":" (:min 2) ":" (:sec 2) "]"))))
    (cond
      ((eq estado-actual 'en-rojo) 
       (format t "Tiempo ~a: la luz ha cambiado de amarillo a rojo~%" fecha-formateada))
      ((eq estado-actual 'en-verde) 
       (format t "Tiempo ~a: la luz ha cambiado de rojo a verde~%" fecha-formateada))
      ((eq estado-actual 'en-amarillo) 
       (format t "Tiempo ~a: la luz ha cambiado de verde a amarillo~%" fecha-formateada))
    )
  )
)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Dado un mismo tiempo-ciclo retorna un valor booleano)
;; ESTRATEGIA: Lógica condicional (Implementada mediante cond)
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
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (Retorna una cadena de texto, no genera efectos secundarios)
;; ESTRATEGIA: Condicional (Evaluación de múltiples casos mediante cond)
;; IMPACTO: No destructiva 
;; ========================================================
;; Requerimiento 4.b: Análisis de Ciclos Semafóricos
(defun recomendacion-ciclo (tiempo)
   (cond
      ((< tiempo 35) "Tiempo de espera muy corto, se recomienda extenderlo")
      ((> tiempo 150) "Tiempo de espera muy largo, se recomienda reducirlo")
      (t "Tiempo de espera óptimo")
   )
)

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Matemática/Composición (Uso de let y truncate)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 5: Planificacion Temporal
(defun ciclos-por-tiempo (minutos)
    (let ((ciclos-completos (truncate (/ (* minutos 60) 216))))
        (format nil "Numero de ciclos completados ~a" ciclos-completos)
    )
)

;; ========================================================
;; FUNCIÓN: informe-dist-temp
;; NATURALEZA: Pura
;; ESTRATEGIA: Matemática/Composición (Uso de let y truncate)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 6: Informe de Distribución Temporal
(defun informe-dist-temp ()
  (let ((calculo-rojo (truncate (/ (* 90 100.0) 216)))
        (calculo-amarillo (truncate (/ (* 6 100.0) 216)))
        (calculo-verde (truncate (/ (* 120 100.0) 216))))
    (format nil "rojo: ~a%  amarillo: ~a%  verde: ~a%" calculo-rojo calculo-amarillo calculo-verde)
  )
)

;; Extension 2
(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt" 
                          :direction :output 
                          :if-exists :supersede       ;si ya existia lo reemplaza
                          :if-does-not-exist :create) ;si no existe crea uno nuevo
    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================~%")
    (mapcar #'(lambda (tiempo-unix)
               (let ((fecha-humana (local-time:format-timestring nil (local-time:unix-to-timestamp tiempo-unix)
                           :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)))
                      (color-actual (temporizador tiempo-unix))
                      (color-anterior (cond 
                        ((equal (temporizador tiempo-unix) 'rojo) 'verde)
                        ((equal (temporizador tiempo-unix) 'amarillo) 'rojo)
                        ((equal (temporizador tiempo-unix) 'verde) 'amarillo))))
               (format stream "~a - Transición: ~a -> ~a~%" 
                          fecha-humana 
                          color-anterior 
                          color-actual)))
              datos)
    (format stream "~% --- Fin del Informe ---")))