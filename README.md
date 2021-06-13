# Lab de Programación Funcional

## Respuestas a las preguntas:

## 1.a) ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.


-- Por un lado, en **Dibujo.hs** creamos la sintaxis del lenguaje. Esto fue, definir los constructores y operadores de manera polimórfica, a los que luego dariamos significado con el módulo Interp. 

-- En **Interp.hs** definimos una semántica para el lenguaje, utilizando a la función *sem* como nexo entre éste módulo y el módulo Dibujo. Asi, utilizando tipos de la libreria Gloss (como Vector, Picture),

damos un significado geométrico a los constructores del tipo **Fig a**. También, éste módulo define las figuras adaptables comunes que utilizamos en el tercer módulo **Escher.hs**.

-- Por último, en el módulo **Escher.hs**, definimos las funciones especificas del paper de Henderson (Dibujo T, Dibujo U, noneto, escher, etc.) y una funcion que, recibiendo un argumento del tipo Escher

definido en este módulo, realice el dibujo de Escher.

La razon por la que estos módulos estan separados es que la sintaxis definida en **Dibujo.hs**, al ser polimórfica, podria ser usada con otra intepretación distinta; es decir, dandole otro significado (nada obliga a que

**Interp.hs** use tipos geométricos, esta es la intepretación que nosotros necesitabamos para realizar el dibujo). De la misma manera, el módulo **Escher.hs** solo define un tipo de dibujo que podríamos realizar con esta 

interpretación, pero nada impediría crear otro distinto que pueda utilizar al módulo **Interp.hs** para realizar otro dibujo.



## b) ¿Harían alguna modificación a la partición en módulos dada? Justificar. Advertencia: Si hacen una buena partición, se las chorearemos para el año que viene.

Un cambio que facilitaría la comprensión de **Interp.hs** seria añadir un cuarto módulo en el que definiriamos a las figuras adaptables comunes, las funciones grid, hlines, y al vector nulo. Consideramos a estas funciones 

como "constantes" para el uso que les damos en el proyecto, y principalmente, que son funciones que no hacen al significado del lenguaje.

## 2) ¿Por qué las funciones básicas no están incluidas en la definición del lenguaje, y en vez es un parámetro del tipo?

Porque estas funciones están implementadas para una semántica en particular, que es la definida en **Interp.hs**. Luego utilizamos esas funciones como parámetros para la función **sem**, y son esas funciones las que

*interpretarán* a nuestro tipo Dibujo, ya que éste tipo es polimórfico, y existe mas de una sola interpretación.

## 3) Explique cómo hace Interp.grid para construir la grilla, explicando cada pedazo de código que interviene.

La función grid toma 4 argumentos: un entero n, un vector v, la separación entre lineas sep y la longitud de estas l. Luego usa estos parámetros para construir una Picture mediante la función pictures, de la siguiente 

manera: toma una lista de n+1 lineas obtenidas con (hlines v sep l) y genera una Picture con esta lista, asignandola a una variable ls.  Luego, usando pictures nuevamente, toma como argumentos a ls, y a ls rotado

90 grados, y trasladado. De esta manera, devuelve una Picture en forma de grilla.
