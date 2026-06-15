;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Dados el mismo estado y color, siempre retorna la misma lista)
;; ESTRATEGIA: Seleccion Condicional Multiple (Implementada mediante cond y eq)
;; IMPACTO: No destructiva (Construye una nueva lista como salida)
;; ========================================================
;; Requerimiento 1: Estados de Transicion

(defun transicion (color-actual color-siguiente)

  (cond

    ((and (eq color-actual 'en-rojo)
          (eq color-siguiente 'verde))
      (list color-actual
            (format nil "cambiar-a-~a" color-siguiente)))

    ((and (eq color-actual 'en-verde)
          (eq color-siguiente 'amarillo))
      (list color-actual
            (format nil "cambiar-a-~a" color-siguiente)))

    ((and (eq color-actual 'en-amarillo)
          (eq color-siguiente 'rojo))
      (list color-actual
            (format nil "cambiar-a-~a" color-siguiente)))

    (t
      (list color-actual 'accion-por-defecto))
  )
)


;; ========================================================
;; FUNCION: temporizador
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Seleccion Condicional
;; IMPACTO: No destructiva
;; ========================================================

;; Requerimiento 2: Temporizador Automatico

(defun temporizador (tiempo-unix)

  (cond

    ((< (mod tiempo-unix 216) 90)
      'rojo)

    ((< (mod tiempo-unix 216) 210)
      'verde)

    (t
      'amarillo)
  )
)

;; ========================================================
;; FUNCION: auditoria
;; NATURALEZA: Impura (Imprime texto en pantalla)
;; ESTRATEGIA: Condicional
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 3: Sistema de Auditoria

(defun auditoria (tiempo-unix)

  (cond

    ((equal (temporizador tiempo-unix) 'rojo)

      (format t
              "Tiempo ~a: la luz ha cambiado de amarillo a rojo"
              tiempo-unix))

    ((equal (temporizador tiempo-unix) 'verde)

      (format t
              "Tiempo ~a: la luz ha cambiado de rojo a verde"
              tiempo-unix))

    ((equal (temporizador tiempo-unix) 'amarillo)

      (format t
              "Tiempo ~a: la luz ha cambiado de verde a amarillo"
              tiempo-unix))
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
;; FUNCION: ciclo-recomendado
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional (Evaluacion de multiples casos mediante cond)
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
;; 5
;; ========================================================
;; FUNCION: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Composicion funcional
;; IMPACTO: No destructiva
;;
;; Recibe una cantidad de minutos de funcionamiento
;; y calcula cuantos ciclos completos del semaforo
;; se realizaron.

(defun ciclos-por-tiempo (minutos)

  (let ((ciclos-completos
          (truncate (/ (* minutos 60) 216))))

    (format nil
            "Numero de ciclos completados ~a"
            ciclos-completos)
  )
)

;;6
;; ========================================================
;; FUNCION: informe-dis-tem
;; NATURALEZA: Pura
;; ESTRATEGIA: Calculo mediante funciones aritmeticas
;; IMPACTO: No destructiva
;;
;; Recibe un tiempo total de funcionamiento y calcula
;; cuantos segundos correspondieron a cada color del
;; semaforo segun las proporciones del ciclo:
;; ROJO -> VERDE -> AMARILLO
;; ========================================================

(defun informe-dis-tem (tiempo-total)

  (let ((calculo-rojo (truncate (* tiempo-total (/ 90 216))))
        (calculo-verde (truncate (* tiempo-total (/ 120 216))))
        (calculo-amarillo (truncate (* tiempo-total (/ 6 216)))))

    (format nil
            "rojo: ~a segundos  verde: ~a segundos  amarillo: ~a segundos"
            calculo-rojo
            calculo-verde
            calculo-amarillo)
  )
)