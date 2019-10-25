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
agregarBit::[[Int]]->[[Int]]
agregarBit [] = []
agregarBit (x:xs) = (agregar01 x) ++ agregarBit xs;
{--
Por último implementaremos una función posiblesNBits que reciba un entero n y retorne todos
los posibles listas binarias de largo n. Es importante tener en cuenta que si bien hay que
considerar el caso en el que la cantidad de bits es 0, el caso base que utilizaremos será el de la
cantidad de bits sea 1, el cual se resuelve simplemente utilizando la función agregar01
aplicada a la lista vacía.
Ejemplo posiblesNBits 2 = [[0,0],[0,1],[1,0],[1,1]]
--} 
posiblesNBits::Int->[[Int]]
posiblesNBits 0 = []
posiblesNBits 1 = agregar01 []
posiblesNBits n = agregarBit (posiblesNBits (n-1));
-- Recibe el numero de bits,retorna la cantidad de numeros que se pueden hacer con esa cantidad de bits.
cantidadCasos::Int->Int
cantidadCasos 0 = 0
cantidadCasos n = length (posiblesNBits n);
{--
Empezaremos entonces por implementar numEntre, que recibirá 2 enteros m y n y generará la
lista con todos los números entre m y n, incluyendo a éstos.
Ejemplo numEntre 2 5 = [2,3,4,5]
--}
numEntre::Int->Int->[Int]
numEntre 0 0 = []
numEntre a b 
    | a <= b = a:numEntre (a+1) b
    | a > b = [];
--Ejemplo agregarTodos [1,2,3] [4,7] = [[1,4,7],[2,4,7],[3,4,7]]
agregarTodos::[Int]->[Int]->[[Int]]
agregarTodos [] l2 = []
agregarTodos (x:xs)  l2 = (x:l2):agregarTodos xs l2;

{--
Empezamos por implementar una función que, dada una lista de enteros, genere una lista de
listas donde se generen todas las listas con los digitos del 0 al 6 al inicio.
: agregar06 [1]= [[0,1],[1,1],[2,1],[3,1],[4,1],[5,1],[6,1]
--}
agregar06::[Int]->[[Int]]
agregar06 l = agregarTodos (numEntre 0 6) l;
{--
En un siguiente paso implementaremos agregarDigito, que dada una lista de listas, retorna una
nueva lista de listas, agregando a cada lista miembro todos los posibles dígitos entre 0 y 6.
agregarDigito [[1]] = [[0,1],[1,1],[2,1],[3,1],[4,1],[5,1],[6,1]]
--}
agregarDigito::[[Int]]->[[Int]]
agregarDigito [] = []
agregarDigito (x:xs) = agregar06 x ++ agregarDigito xs;
{--
Por último implementaremos posiblesNDigitos, que recibe un entero que determina la cantidad
de dígitos que tendrán todos los posibles números que queremos representar.
Ejemplo: posiblesNDigitos 1 = [[0],[1],[2],[3],[4],[5],[6]]
--}
posiblesNDigitos::Int->[[Int]]
posiblesNDigitos 0 = []
posiblesNDigitos 1 = agregar06 []
posiblesNDigitos n = agregarDigito(posiblesNDigitos (n-1));
{--
Siguiendo el mismo orden que el último ejercicio comenzaremos por implementar
agregarTodosFiltrando, que funciona de la misma manera que agregar todos, con la diferencia
que, antes de agregar un elemento de la primer lista a la segunda, verifica que el elemento no
esté en la segunda lista
agregarTodosFiltrando [1,2,3] [3] = [[1,3],[2,3]]
--}
contiene::Int->[Int]->Bool
contiene e [] = False
contiene e (x:xs) 
    | x == e = True
    | otherwise = contiene e xs;

agregarTodosFiltrando::[Int]->[Int]->[[Int]]
agregarTodosFiltrando [] l2 = [l2]
agregarTodosFiltrando (x:xs) l2
    | contiene x l2 = agregarTodosFiltrando xs l2
    | otherwise = x:l2:agregarTodosFiltrando xs l2;