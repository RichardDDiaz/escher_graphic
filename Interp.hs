module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
-- por la libreria tenemos que type Point = (Float, Float) y type Vector = Point
-- Picture 
--
--
--
--
type FloatingPic = Vector -> Vector -> Vector -> Picture
type Output a = a -> FloatingPic

-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

-- comprender esta función es un buen ejericio.
-- line :: Path -> Picture, donde Path = [Point] = [Vector]
-- Probando la funcion genera una linea de puntos
-- que al hacer zoom no son puntos sino que son lineas pequeñas
-- supungo que mag debe de ser el largo de esas lineas y sep
-- la separacion entre cada una, aunque la logica de la funcion
-- no termino de entenderla
hlines :: Vector -> Float -> Float -> [Picture]
hlines (x,y) mag sep = map (hline . (*sep)) [0..]
  where hline h = line [(x,y+h),(x+mag,y+h)]

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
-- Comentario: retorna pictures [picture linea n horizontales, picture igual pero rotado 09 y trasladado mas arriba]
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
  where ls = pictures $ take (n+1) $ hlines v sep l

-- figuras adaptables comunes
trian1 :: FloatingPic
trian1 a b c = line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 a b c = Color red $  line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD a b c = line $ map (a V.+) [c, half b , b V.+ c , c]

rectan :: FloatingPic
rectan a b c = line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

--simple :: Picture -> Vector -> Vector -> Vector -> Picture
simple :: Picture -> FloatingPic
simple p _ _ _ = p

fShape :: FloatingPic
fShape a b c = line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]
  where p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs,ys) a b c  = translate (fst a') (snd a') .
                             scale (magV b/xs) (magV c/ys) .
                             rotate ang $ f d (xs,ys)
  where ang = radToDeg $ argV b
        a' = a V.+ half (b V.+ c)


--type FloatingPic = Vector -> Vector -> Vector -> Picture
--type Output a = a -> FloatingPic

--_rotar(f)(x, w, h)_        | _f(x+w, h, -w)_
_rotar :: FloatingPic -> FloatingPic
_rotar f x w h =  f (x V.+ w) h (V.negate w)

--_rot45(f)(x, w, h)_        | _f(x+(w+h)/2, (w+h)/2, (h-w)/2)_
_rot45 :: FloatingPic -> FloatingPic
_rot45  f x w h = f (x V.+ half (w V.+ h)) (half (w V.+ h)) (half (h V.+ (V.negate w)))

--_espejar(f)(x, w, h)_      | _f(x+w, -w, h)_
_espejar :: FloatingPic -> FloatingPic
_espejar f x w  = f (x V.+ w) (V.negate w) 

--_encimar(f,g)(x, w, h)_    | _f(x, w, h) ∪ g(x, w, h)_ 
-- pictures :: [Pictures] -> Picture, por lo que se cumple que retorne Picture
_encimar :: FloatingPic -> FloatingPic -> FloatingPic
_encimar f g x w h = pictures [f x w h, g x w h]

opFloat :: Int -> Int -> Float
opFloat n m = fromIntegral m / fromIntegral (m+n)

opFloat2 :: Int -> Int -> Float
opFloat2 n m = fromIntegral n / fromIntegral (m+n)

--juntar(n, m, f, g)(x, w, h)  ---------> 
--f(x, w', h) ∪ g(x+w', r'w, h) con r'=n/(m+n), r=m/(m+n), w'=rw
_juntar :: Int -> Int -> FloatingPic -> FloatingPic -> FloatingPic
_juntar n m f g x w h =  pictures [f x w' h, g (x V.+ w') (r' V.* w) h ]
  where r = opFloat2 n m
        r' = opFloat n m
        w' = r V.* w

--apilar(n, m, f, g)(x, w, h)  ---------> 
--f(x + h', w, rh) ∪ g(x, w, h') con r' = n/(m+n), r=m/(m+n), h'=r'h
_apilar :: Int -> Int -> FloatingPic ->  FloatingPic -> FloatingPic
_apilar n m f g x w h = pictures [f (x V.+ h') w (r V.* h), g x w h']
  where r' = opFloat n m
        r = opFloat2 n m
        h' = r' V.* h

vacio :: FloatingPic
vacio = simple blank 

interp :: Output a -> Output (Dibujo a)
interp f = sem f _rotar _espejar _rot45 _apilar _juntar _encimar

