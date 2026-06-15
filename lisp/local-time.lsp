
;; 1. Carga de librerías externas mediante Quicklisp
(ql:quickload "local-time")
;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 1
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

    (T (list color-actual 'accion-por-defecto))
  )
)

;; ========================================================
;; FUNCION: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 2
(defun timer (epoch)
  (cond
    ((< (mod epoch 216) 90) 'en-rojo)
    ((< (mod epoch 216) 210) 'en-verde)
    (T 'en-amarillo)
  )
)

;; ========================================================
;; FUNCION: registrar-cambio
;; NATURALEZA: Impura
;; ESTRATEGIA: Salida por pantalla
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 3
(defun registrar-cambio (tiempo color-anterior color-nuevo)
  (format t
          "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          tiempo
          color-anterior
          color-nuevo)
)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Dado un mismo tiempo-ciclo retorna un valor booleano)
;; ESTRATEGIA: Lógica condicional (Implementada mediante cond)
;; IMPACTO: No destructiva (devuelve un booleano)
;; ========================================================
;Requerimiento 4.a
(defun duracion-ciclo(tiempo-ciclo)
    (cond
        ((and(>= tiempo-ciclo 35) (<= tiempo-ciclo 150)) T)
        (t 'nil)
    )
)

;; ========================================================
;; FUNCIÓN: ciclo-recomendado
;; NATURALEZA: Pura (Retorna una cadena de texto, no genera efectos secundarios)
;; ESTRATEGIA: Condicional (Evaluación de múltiples casos mediante cond)
;; IMPACTO: No destructiva 
;; ========================================================
;; Requerimiento 4.b
(defun ciclo-recomendado (tiempo)
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
;; Requerimiento 5
(defun ciclos-por-tiempo (minutos)
    (let ((ciclos-completos (truncate (/ (* minutos 60) 216))))
        (format nil "Numero de ciclos completados ~a" ciclos-completos)
    )
)

;; ========================================================
;; FUNCIÓN: informe-dis-tem
;; NATURALEZA: Pura
;; ESTRATEGIA: Matemática/Composición (Uso de let y truncate)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 6
(defun informe-dis-tem (tiempo-total)
  ;; El porcentaje es fijo según la duración de cada color en el ciclo de 216s
  (let ((calculo-rojo (truncate (/ (* 90 100.0) 216)))
        (calculo-amarillo (truncate (/ (* 6 100.0) 216)))
        (calculo-verde (truncate (/ (* 120 100.0) 216))))
    (format nil "rojo: ~a%  amarillo: ~a%  verde: ~a%" calculo-rojo calculo-amarillo calculo-verde)
  )
)