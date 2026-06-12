;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERSION CON ERRORES
;; BITACORA DE FALLOS NUESTROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;;
;; MI ERROR:
;; Inicialmente utilice cadenas de texto ("en-rojo",
;; "verde") para realizar las comparaciones. Luego
;; descubri que el sistema trabaja con simbolos
;; ('en-rojo, 'verde), por lo que las condiciones
;; nunca se cumplian y siempre se ejecutaba la
;; accion por defecto.
;; ========================================================

(defun transicion (color-actual cambiar-a)

  (cond

    ((and (eq color-actual 'en-rojo)
          (eq cambiar-a 'verde))
      (list color-actual "cambiar-a-verde"))

    ((and (eq color-actual 'en-verde)
          (eq cambiar-a 'amarillo))
      (list color-actual "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo)
          (eq cambiar-a 'rojo))
      (list color-actual "cambiar-a-rojo"))

    (T
      (list color-actual 'accion-por-defecto))
  )
)



;; ========================================================
;; FUNCION: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;;
;; MI ERROR:
;; Inicialmente asumi que la secuencia de estados era
;; ROJO -> AMARILLO -> VERDE.
;; Luego descubri que el ciclo correcto era:
;; ROJO -> VERDE -> AMARILLO -> ROJO.
;;
;; Tambien utilice <= para delimitar los rangos
;; temporales, generando errores en los cambios
;; exactos de estado.
;; ========================================================

(defun timer (epoch)

  (cond

    ((< (mod epoch 216) 90)
      'en-rojo)

    ((< (mod epoch 216) 210)
      'en-verde)

    (T
      'en-amarillo)
  )
)



;; ========================================================
;; FUNCION: registrar-cambio
;; NATURALEZA: Impura
;; ESTRATEGIA: Salida por pantalla
;; IMPACTO: No destructiva
;;
;; MI ERROR:
;; Inicialmente devolvia una lista con la informacion
;; del cambio. Luego observe que el requerimiento
;; solicitaba imprimir el mensaje en la terminal.
;; ========================================================

(defun registrar-cambio
       (tiempo color-anterior color-nuevo)

  (format t
          "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          tiempo
          color-anterior
          color-nuevo)
)

