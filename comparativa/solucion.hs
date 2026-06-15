
-- Funciones aplicadas a Haskell

-- ========================================================
-- FUNCION: transicion
-- NATURALEZA: Pura
-- ESTRATEGIA: Pattern Matching
-- IMPACTO: No destructiva
-- ========================================================

data Estado = EnRojo | EnVerde | EnAmarillo
    deriving (Show, Eq)

data Destino = Rojo | Verde | Amarillo
    deriving (Show, Eq)

transicion :: Estado -> Destino -> (Estado, String)

transicion EnRojo Verde =
    (EnRojo, "cambiar-a-verde")

transicion EnVerde Amarillo =
    (EnVerde, "cambiar-a-amarillo")

transicion EnAmarillo Rojo =
    (EnAmarillo, "cambiar-a-rojo")

transicion estado _ =
(estado, "accion-por-defecto")



-- ========================================================
-- FUNCION: timer
-- NATURALEZA: Pura
-- ESTRATEGIA: Condicional
-- IMPACTO: No destructiva
-- ========================================================

timer :: Int -> Estado

timer epoch
    | t < 90   = EnRojo
    | t < 210  = EnVerde
    | otherwise = EnAmarillo
  where
    t = mod epoch 216

