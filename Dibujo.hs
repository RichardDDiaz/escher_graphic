module Dibujo where

-- Definir el lenguaje.
data Fig a = Basica a | Rotar (Fig a) | Espejar (Fig a) | Rot45 (Fig a)
    | Apilar Int Int (Fig a) (Fig a)
    | Juntar Int Int (Fig a) (Fig a)
    | Encimar (Fig a) (Fig a)
    deriving(Eq,Show)


type Dibujo a = Fig a

comp :: (a ->a) -> Int -> a -> a
comp f 1 d = f d
comp f n d = comp f (n-1) (f d)


-- Con currificacion quedaria mas elegante creo, no recuerdo como era eso
-- Aqui use la comp, queda mejor
r180 :: Dibujo a -> Dibujo a
r180= comp Rotar 2

r270 :: Dibujo a -> Dibujo a
r270 = comp Rotar 3

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (d1 /// d2) .-. (d3 /// d4)
--
-- Mi idea es unir dos dibujos abajo y a esos dibujos apilarle arriba los otros

encimar4 :: Dibujo a -> Dibujo a
encimar4 d =  (d ^^^ Rotar d) ^^^ (r180 d ^^^ r270 d)



-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (Rotar d) (r180 d) (r270 d)


-- esquemas para la manipulación de figuras básicas.

-- Ver un a como una figura.
-- Tiene que ver con MONADAS, como los maybe
pureDib :: a -> Dibujo a
pureDib = Basica

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica n) = Basica (f n)
mapDib f (Rotar n) = Rotar (mapDib f n)
mapDib f (Espejar n) = Espejar (mapDib f n)
mapDib f (Rot45 n) = Rot45 (mapDib f n)
mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de intro a la lógica
--comentario: le da un significado a cada constructor, varias funciones que queramos
--            para cada uno de los construtores. Logicos, aritmeticos, los que definamos
sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Int -> Int -> b -> b -> b) ->
       (Int -> Int -> b -> b -> b) ->
       (b -> b -> b) ->
       Dibujo a -> b
sem ba ro es r45 ap ju en (Basica a) = ba a
sem ba ro es r45 ap ju en (Rotar n) = ro (sem ba ro es r45 ap ju en n)
sem ba ro es r45 ap ju en (Espejar n) = es (sem ba ro es r45 ap ju en n)
sem ba ro es r45 ap ju en (Rot45 n) = r45 (sem ba ro es r45 ap ju en n)
sem ba ro es r45 ap ju en (Apilar x y d1 d2) = ap x y (sem ba ro es r45 ap ju en d1) (sem ba ro es r45 ap ju en d2)
sem ba ro es r45 ap ju en (Juntar x y d1 d2) = ju x y (sem ba ro es r45 ap ju en d1) (sem ba ro es r45 ap ju en d2)
sem ba ro es r45 ap ju en (Encimar d1 d2) = en (sem ba ro es r45 ap ju en d1) (sem ba ro es r45 ap ju en d2)


type Pred a = a -> Bool


-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
-- comentario: use expresiones lamdba, podes pasarle el predicado sin ningun problema
--             solo toma un argumento y escupe el resultado asi que podemos usarlo
--             en mapDib 
cambiar :: Pred a -> a -> Dibujo a -> Dibujo a
cambiar p a = mapDib (\ x -> if p x then a else x)


-- Alguna básica satisface el predicado.
-- Comentario: Como se dijo en la funcion sem, aqui le dimos varias funciones que
-- le dan un significado a cada constructor, para nuestra caso particular (Booleanos)
-- El id es la funcion identidad
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = sem p id id id (\ x y d1 d2 -> d1 || d2) (\ x y d1 d2 -> d1 || d2) (||)


-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = sem p id id id (\ x y d1 d2 -> d1 && d2) (\ x y d1 d2 -> d1 && d2) (&&)


-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a -- (a -> Bool -> a -> Bool) -> a -> Bool
andP p q a  = p a && q a


-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP p q a = p a || q a

-- Describe la figura. Ejemplos: 
--   desc (const "b") (Basica b) = "b"
--   desc (const "b") (Rotar (Basica b)) = "rot (b)"
--   desc (const "b") (Apilar n m (Basica b) (Basica b)) = "api n m (b) (b)"
-- La descripción de cada constructor son sus tres primeros
-- símbolos en minúscula, excepto `Rot45` al que se le agrega el `45`.
desc :: (a -> String) -> Dibujo a -> String
desc f = sem f (\ x -> "rot "++ x ++"")
                 (\ x -> "esp "++ x ++"")
                 (\ x -> "rot45 "++ x ++"")
                 (\ x y d1 d2 -> "api  "++ show x ++" "++ show y ++" "++ d1 ++" "++ d2 ++"")
                 (\ x y d1 d2 -> "jun  "++ show x ++" "++ show y ++" "++ d1 ++" "++ d2 ++"")
                 (\ d1 d2 -> "enc "++ d1 ++" "++ d2 ++"")

-- Junta todas las figuras básicas de un dibujo.
basicas :: Dibujo a -> [a]
basicas = sem (\ x -> [x])
                id
                id
                id
                (\ x y d1 d2 -> d1++d2)
                (\ x y d1 d2 -> d1++d2)
                (\ d1 d2 -> d1++d2)


sigEsp :: Dibujo a -> Bool
sigEsp (Basica _) = False
sigEsp (Rotar _) = False
sigEsp (Espejar _) = True
sigEsp (Rot45 _) = False
sigEsp Apilar {} = False
sigEsp Juntar {} = False
sigEsp (Encimar _ _) = False


-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a) -- Dibujo a -> Bool
esFlip2 (Basica a) = False
esFlip2 (Rotar d) = esFlip2 d
esFlip2 (Espejar d) = sigEsp d || esFlip2 d -- vs code recomienda otra forma, no aplicarla
esFlip2 (Rot45 d) = esFlip2 d
esFlip2 (Apilar x y d1 d2) = esFlip2 d1 || esFlip2 d2
esFlip2 (Juntar x y d1 d2) = esFlip2 d1 || esFlip2 d2
esFlip2 (Encimar d1 d2) = esFlip2 d1 || esFlip2 d2



sigRot :: Dibujo a -> Int -> Bool
sigRot (Basica _) n = False
sigRot (Rotar d) n = n == 0 || sigRot d (n-1)
sigRot (Espejar _) n = False
sigRot (Rot45 _) n = False  -- si uso rot45 8 veces seguidas me da 360, que onda?
sigRot Apilar {} n = False
sigRot Juntar {} n = False
sigRot (Encimar _ _) n = False

-- Hay 4 rotaciones seguidas
esRot360 :: Pred (Dibujo a)
esRot360 (Basica a) = False
esRot360 (Rotar d) = sigRot d 2 || esRot360 d
esRot360 (Espejar d) = esRot360 d
esRot360 (Rot45 d) = esRot360 d
esRot360 (Apilar x y d1 d2) = esRot360 d1 || esRot360 d2
esRot360 (Juntar x y d1 d2) = esRot360 d1 || esRot360 d2
esRot360 (Encimar d1 d2) = esRot360 d1 || esRot360 d2

data Superfluo = RotacionSuperflua | FlipSuperfluo
                deriving(Eq,Show)

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
--check :: Dibujo a -> Either [Superfluo] (Dibujo a)

check :: Dibujo a -> Either [Superfluo] (Dibujo a)
check d 
    | esFlip2 d = if esRot360 d then Left [FlipSuperfluo,RotacionSuperflua] else Left [FlipSuperfluo]
    | esRot360 d = if esFlip2 d then Left [FlipSuperfluo,RotacionSuperflua] else Left [RotacionSuperflua]
    | otherwise = Right d

    

