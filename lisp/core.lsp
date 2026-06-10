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