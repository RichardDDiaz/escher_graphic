module Basica.Escher where
import Dibujo
import Interp
import Graphics.Gloss

--Cambiamos el type y creamos 
data Escher = Vacio | Triangulo

fish2 :: Fig a -> Fig a
fish2 p = Espejar (Rot45 p)

-- El dibujo u.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU = encimar4 . fish2

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = p ^^^ fish2 p ^^^ fish3 p
            where fish3  = r270 
    --ver tamaño de los dibujos, en el henderson dice que se reduce la figura por sqrt(2)

-- Esquina con nivel de detalle en base a la figura p.
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 _ = pureDib Vacio
esquina n p = cuarteto (esquina (n-1) p) (l p) (Rotar (l p)) (dibujoU p)
              where l = lado (n-1)

-- Lado con nivel de detalle.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 _ = pureDib Vacio
lado n p = cuarteto (l p) (l p) (Rotar (dibujoU p)) (dibujoU p)
    where l = lado (n-1)


noneto p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p (Juntar 1 1 q r)) 
                           (Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u))
                           (Juntar 1 2 v (Juntar 1 1 w x)))
                           

-- El dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n t = noneto 
            (esquina n p) 
            (lado n p ) 
            (r270 (esquina n p))
            (Rotar (lado n p))
            p
            (r270 (lado n p))
            (Rotar (esquina n p))
            (r180 (lado n p))
            (r180 (esquina n p))
            where p = pureDib t


interpEscher :: Output Escher 
interpEscher Triangulo = trian2
interpEscher Vacio = simple blank 