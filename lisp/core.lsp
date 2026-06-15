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
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;;
;; NUESTRO ERROR:
;; Inicialmente asumimos que la secuencia de estados era
;; ROJO -> AMARILLO -> VERDE.
;; Luego descubri que el ciclo correcto era:
;; ROJO -> VERDE -> AMARILLO -> ROJO.
;;
;; Tambien utilizamos <= y < para delimitar los rangos
;; temporales, generando errores en los cambios
;; exactos de estado.
;; ========================================================
;; Requerimiento 2
(defun temporizador(tiempo_actual)
    (cond
		((and (>= (mod tiempo_actual 216) 0) (< (mod tiempo_actual 216) 90)) 'rojo)
		((and (>= (mod tiempo_actual 216) 90) (< (mod tiempo_actual 216) 96)) 'amarillo)
		((and (>= (mod tiempo_actual 216) 96) (< (mod tiempo_actual 216) 216)) 'verde)
	)
)

;; ========================================================
;; FUNCION: auditoria
;; NATURALEZA: Impura
;; ESTRATEGIA: Salida por pantalla
;; IMPACTO: No destructiva
;;
;; NUESTRO ERROR:
;; Habia un problema con el ciclo del semaforo, lo que generaba mensajes incorrectos.
;; Por ejemplo, cuando el semaforo cambiaba a rojo, el mensaje indicaba que habia cambiado 
;; de verde a rojo, cuando en realidad el cambio era de amarillo a rojo.
;; ========================================================
;; Requerimiento 3
(defun auditoria (tiempo-unix)
  (let ((estado-actual (temporizador tiempo-unix)))
    (cond
      ((eq estado-actual 'rojo) 
       (format t "Tiempo ~a: la luz ha cambiado de amarillo a rojo~%" tiempo-unix))
      ((eq estado-actual 'verde) 
       (format t "Tiempo ~a: la luz ha cambiado de rojo a verde~%" tiempo-unix))
      ((eq estado-actual 'amarillo) 
       (format t "Tiempo ~a: la luz ha cambiado de verde a amarillo~%" tiempo-unix))
    )
  )
)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Dado un mismo tiempo-ciclo retorna un valor booleano)
;; ESTRATEGIA: Lógica condicional (Implementada mediante cond)
;; IMPACTO: No destructiva (devuelve un booleano)
;; ========================================================
;; Requerimiento 4.a
(defun duracion-ciclo (tiempo-ciclo)
    (cond
        ((and (> tiempo-ciclo 35) (< tiempo-ciclo 150)) T)
        (t nil)
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
