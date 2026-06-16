;; Fase 1
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
;; NATURALEZA: Pura (Dado un mismo tiempo, siempre retorna el mismo atomo)
;; ESTRATEGIA: Matematica y Seleccion Condicional (mod y cond)
;; IMPACTO: No destructiva
;;
;; NUESTRO ERROR:
;; Inicialmente asumimos que la secuencia de estados era
;; ROJO -> AMARILLO -> VERDE.
;; Luego descubrimos que el ciclo correcto era:
;; ROJO -> VERDE -> AMARILLO -> ROJO.
;; Tambien utilizabamos < repitiendo la funcion 'mod' 
;; para delimitar los rangos, generando código ineficiente. 
;; ========================================================
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
;; NATURALEZA: Impura (Genera efectos secundarios al imprimir en consola)
;; ESTRATEGIA: Salida por Pantalla y Seleccion Condicional
;; IMPACTO: No destructiva
;;
;; NUESTRO ERROR:
;; Habia un problema con el ciclo del semaforo, lo que generaba mensajes incorrectos.
;; Por ejemplo, cuando el semaforo cambiaba a rojo, el mensaje indicaba que habia cambiado 
;; de verde a rojo, cuando en realidad el cambio era de amarillo a rojo.
;; ========================================================
;; Requerimiento 3: Sistema de Auditoria
(defun auditoria (tiempo-unix)
  (let ((estado-actual (temporizador tiempo-unix)))
    (cond
      ((eq estado-actual 'en-rojo) 
       (format t "Tiempo ~a: la luz ha cambiado de amarillo a rojo~%" tiempo-unix))
      ((eq estado-actual 'en-verde) 
       (format t "Tiempo ~a: la luz ha cambiado de rojo a verde~%" tiempo-unix))
      ((eq estado-actual 'en-amarillo) 
       (format t "Tiempo ~a: la luz ha cambiado de verde a amarillo~%" tiempo-unix))
    )
  )
)

;; ========================================================
;; FUNCION: duracion-ciclo
;; NATURALEZA: Pura (Dado un mismo tiempo-ciclo retorna un valor booleano)
;; ESTRATEGIA: Logica condicional (Implementada mediante cond)
;; IMPACTO: No destructiva (devuelve un booleano)
;; ========================================================
;; Requerimiento 4.a: Analisis de Ciclos Semaforicos
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
    (let ((ciclos-completos (truncate (/ (* minutos 60) 216))))
        (format nil "Numero de ciclos completados ~a" ciclos-completos)
    )
)

;; ========================================================
;; FUNCION: informe-dist-temp
;; NATURALEZA: Pura
;; ESTRATEGIA: Matematica/Composicion (Uso de let y truncate)
;; IMPACTO: No destructiva
;; ========================================================
;; Requerimiento 6: Informe de Distribucion Temporal
(defun informe-dist-temp ()
  (let ((calculo-rojo (truncate (/ (* 90 100.0) 216)))
        (calculo-amarillo (truncate (/ (* 6 100.0) 216)))
        (calculo-verde (truncate (/ (* 120 100.0) 216))))
    (format nil "rojo: ~a%  amarillo: ~a%  verde: ~a%" calculo-rojo calculo-amarillo calculo-verde)
  )
)

;; ========================================================
;; REQUERIMIENTO 7: ASEGURAMIENTO DE LA CALIDAD 
;; Guia de ejecucion de pruebas para validacion manual.
;; Las lineas con ">" representan el ingreso del operador.
;; ========================================================

;; --- Pruebas Requerimiento 1: transicion ---
;; Normal (Camino esperado):
; > (transicion 'en-rojo 'verde)
; (EN-ROJO "cambiar-a-verde")
;
;; Alternativo (Accion por defecto):
; > (transicion 'en-rojo 'amarillo)
; (EN-ROJO ACCION-POR-DEFECTO)
;

;; --- Pruebas Requerimiento 2: temporizador ---
;; Normal (Evaluacion de los 3 colores del ciclo):
; > (temporizador 45)
; ROJO
; > (temporizador 90)
; AMARILLO
; > (temporizador 212)
; VERDE
;
;; Alternativo (Tiempos mayores al primer ciclo de 216s):
; > (temporizador 300) 
; ROJO ; (300 mod 216 = 84, cae en rango rojo)
;

;; --- Pruebas Requerimiento 3: auditoria ---
;; Normal (Impresion por pantalla segun el tiempo ingresado):
; > (auditoria 212)
; Tiempo 212: la luz ha cambiado de rojo a verde
; NIL
;
;; Alternativo (Impresion superando el primer ciclo):
; > (auditoria 230)
; Tiempo 230: la luz ha cambiado de amarillo a rojo
; NIL

;; --- Pruebas Requerimiento 4.a: duracion-ciclo ---
;; Normal (Tiempo dentro del rango optimo):
; > (duracion-ciclo 100)
; T
;
;; Alternativo (Tiempo fuera del rango optimo):
; > (duracion-ciclo 20)
; NIL
;

;; --- Pruebas Requerimiento 4.b: ciclo-recomendado ---
;; Normal (Tiempo optimo):
; > (ciclo-recomendado 100)
; "Tiempo de espera optimo"
;
;; Alternativos (Tiempos cortos y largos):
; > (ciclo-recomendado 20)
; "Tiempo de espera muy corto, se recomienda extenderlo"
; > (ciclo-recomendado 200)
; "Tiempo de espera muy largo, se recomienda reducirlo"
;

;; --- Pruebas Requerimiento 5: ciclos-por-tiempo ---
;; Normal (Evaluacion de multiples ciclos):
; > (ciclos-por-tiempo 10)
; "Numero de ciclos completados 2"
;
;; Alternativo (Tiempo insuficiente para completar 1 ciclo):
; > (ciclos-por-tiempo 3)
; "Numero de ciclos completados 0"
;

;; --- Pruebas Requerimiento 6: informe-dist-temp ---
;; Normal (Retorno de porcentajes fijos en formato texto):
; > (informe-dist-temp)
; "rojo: 41%  amarillo: 2%  verde: 55%"