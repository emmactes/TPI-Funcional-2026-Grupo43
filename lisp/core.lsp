;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Dados el mismo estado y color, siempre retorna la misma lista)
;; ESTRATEGIA: Seleccion Condicional Multiple (Implementada mediante cond y equal)
;; IMPACTO: No destructiva (Construye una nueva lista como salida)
;; ========================================================
; Requerimiento 1: Estados de Transición
(defun transicion (color-actual color-siguiente)
  (cond 
     ((and (eq color-actual 'en-rojo) (eq color-siguiente 'verde))
        (list color-actual (format nil "cambiar-a-~a" color-siguiente)))
        ((and (eq color-actual 'en-rojo) (eq color-siguiente 'amarillo))
        (list color-actual (format nil "cambiar-a-~a" color-siguiente)))
        ((and (eq color-actual 'en-amarillo) (eq color-siguiente 'rojo))
        (list color-actual (format nil "cambiar-a-~a" color-siguiente)))
        ((and (eq color-actual 'en-amarillo) (eq color-siguiente 'verde))
        (list color-actual (format nil "cambiar-a-~a" color-siguiente)))
        ((and (eq color-actual 'en-verde) (eq color-siguiente 'rojo))
        (list color-actual (format nil "cambiar-a-~a" color-siguiente)))
        ((and (eq color-actual 'en-verde) (eq color-siguiente 'amarillo))
        (list color-actual (format nil "cambiar-a-~a" color-siguiente)))
     (t (list color-actual 'accion-por-defecto))))

;; ========================================================
;; FUNCIÓN: temporizador
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Selección Condicional (Implementada mediante cond y operadores matemáticos)
;; IMPACTO: No destructiva
;; ========================================================
; Requerimiento 2: Temporizador Automatico
(defun temporizador (tiempo-unix)
    (cond
    ((<(mod tiempo-unix 216) 90) 'rojo)
        ((and (> (mod tiempo-unix 216) 89) (< (mod tiempo-unix 216) 96)) 'amarillo)
        ((> (mod tiempo-unix 216) 95) 'verde))
    )

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura (Imprime texto en pantalla de acuerdo al valor agregado)
;; ESTRATEGIA: Condicional (evaluación de múltiples casos mediante cond y llamadas a la función temporizador)
;; IMPACTO: No destructiva 
;; ========================================================
;3
(defun auditoria(tiempo-unix)
    (cond
        ((equal (temporizador tiempo-unix) 'rojo) (format t "Tiempo ~a: la luz ha cambiado de ~a a ~a" tiempo-unix 'verde 'rojo ))
        ((equal (temporizador tiempo-unix) 'amarillo) (format t "Tiempo ~a: la luz ha cambiado de ~a a ~a" tiempo-unix 'rojo 'amarillo ))
        ((equal (temporizador tiempo-unix) 'verde) (format t "Tiempo ~a: la luz ha cambiado de ~a a ~a" tiempo-unix 'amarillo 'verde ))
    )
)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Dado un mismo tiempo-ciclo retorna un valor booleano)
;; ESTRATEGIA: Lógica condicional (Implementada mediante cond)
;; IMPACTO: No destructiva (devuelve un booleano)
;; ========================================================
;4.a
(defun duracion-ciclo(tiempo-ciclo)
    (cond
        ((and(>= tiempo-ciclo 35) (<= tiempo-ciclo 150)) T)
        (t nil)
    )
)

;; ========================================================
;; FUNCIÓN: ciclo-recomendado
;; NATURALEZA: Impura (imprime texto en pantalla de acuerdo al parametro ingresado)
;; ESTRATEGIA: Condicional (Evaluación de múltiples casos mediante cond)
;; IMPACTO: No destructiva 
;; ========================================================
;4.b
(defun ciclo-recomendado(tiempo)
   (cond
      ((< tiempo 35) "Tiempo de espera muy corto, se recomienda extenderlo")
      ((> tiempo 150) "Tiempo de espera muy largo, se recomienda reducirlo")
      (t "Tiempo de espera óptimo")
   )
)
;5 
(defun ciclos-por-tiempo (minutos)
	(let ((ciclos-completos (truncate (/ (* minutos 60) 216))))
		(format nil "Numero de ciclos completados ~a" ciclos-completos)
	)
)

;6 
(defun informe-dis-tem (tiempo-total)
  (let ((calculo-rojo (truncate (* tiempo-total (/ 90 216))))
        (calculo-amarillo (truncate (* tiempo-total (/ 6 216))))
        (calculo-verde (truncate (* tiempo-total (/ 120 216)))))
    (format nil "rojo: ~a segundos  amarillo: ~a segundos  verde: ~a segundos" calculo-rojo calculo-amarillo calculo-verde)
  )
)