module Basica.Ejemplo where
import Dibujo
import Interp

type Basica = ()
ejemplo :: Dibujo Basica
ejemplo = ()

interpBas :: Output Basica
interpBas () = trian1
