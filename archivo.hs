import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white cuadrado

cuadrado ::  Picture 
cuadrado = Line [(1.5,1.2),(3.0,11.4),(5.2,6.9),(4.8,5.3)]