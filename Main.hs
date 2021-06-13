module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Basica.Escher as E

import Text.Read

data Conf a = Conf {
    basic :: Output a
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  }


ej :: Maybe Int -> Conf E.Escher
ej Nothing = error "Parametros no validos"
ej (Just n) = Conf {
                basic = E.interpEscher
              , fig = E.escher n E.Triangulo
              , width = 100
              , height = 100
              }

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf E.Escher) -> IO ()
initial cf = cf >>= \cfg ->
                  let x  = width cfg
                      y  = height cfg
                                                          --Output () -> Output (Dibujo ())
                                                          --Output () -> Dibujo () -> FloatingPic
                                                          --Output () -> Dibujo () -> Vector -> Vector -> Vector -> Picture
                  in display win white . withGrid $ interp (basic cfg) (fig cfg) (0,0) (x,0) (0,y)
        -- aqui empezamos a formar la figura final
  where withGrid p = pictures [p, color grey $ grid 10 (0,0) 100 10]
        grey = makeColorI 135 135 135 135

win = InWindow "Nice Window" (200, 200) (0, 0)
--main = initial $ return (ej 100 100 1)
main = do
        putStrLn "Ingrese nivel de detalle mayor igual a 0:"
        n <- getLine
        initial $ return $ ej (readMaybe n) 