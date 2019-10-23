module Conteo where
{--
Comenzaremos por escribir una función agregar01 que nos permite, dada una lista de enteros,
retornar una nueva lista de listas de enteros que contenga la lista que se obtiene de la dada
agregando el 0 y la lista que se obtiene agregando el 1.
Ejemplo agregar01 [0,1,0,0] = [[0,0,1,0,0],[1,0,1,0,0]]
--}
agregar01::[Int]->[[Int]]
agregar01 [] = [[0],[1]]
agregar01 l = [(0:l),(1:l)]
{--
En un siguiente paso, implementaremos una función agregarBit que, dada una lista de lista de
enteros, retorne el resultado de agregar 0 y 1 a cada una de las listas.
Ejemplo agregarBit [[0,0,1,0,0],[1,0,1,0,0]] = [[0,0,0,1,0,0],[1,0,0,1,0,0],[0,1,0,1,0,0],[1,1,0,1,0,0]]
--}
agregarBit::[[Int]]->[[[Int]]]
agregarBit [] = []
agregarBit (x:xs) = (agregar01 x):agregarBit xs;
{--
Por último implementaremos una función posiblesNBits que reciba un entero n y retorne todos
los posibles listas binarias de largo n. Es importante tener en cuenta que si bien hay que
considerar el caso en el que la cantidad de bits es 0, el caso base que utilizaremos será el de la
cantidad de bits sea 1, el cual se resuelve simplemente utilizando la función agregar01
aplicada a la lista vacía.
Ejemplo posiblesNBits 2 = [[0,0],[0,1],[1,0],[1,1]]
--} 
posiblesNBits::Int->[[Int]]
posiblesNBits = undefined