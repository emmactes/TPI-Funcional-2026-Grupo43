;; Fase 2
;; Carga de libreria externa mediante Quicklisp
(ql:quickload "local-time")
;; ========================================================
;; FUNCION: temporizador
;; NATURALEZA: Pura (Dado un mismo tiempo, siempre retorna el mismo átomo)
;; ESTRATEGIA: Matematica y Seleccion Condicional (Uso de let, mod y cond)
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
;; Requerimiento 3 - Integracion Fase 2 (Quicklisp)
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