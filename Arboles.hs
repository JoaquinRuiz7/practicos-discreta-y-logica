module Arboles where 
data BinTree a = Empty|Node a (BinTree a) (BinTree a)
    deriving (Show)
{--
   a
b        NULL
NULL NULL
Programar este arbol
arbol::Bin
arbol = Nodo 'a' (Nodo 'b' (NULL) (NULL) ) (NULL)
--}
--Cantidad de nodos
cantidadNodos::BinTree a->Int
cantidadNodos Empty = 0
cantidadNodos (Node a t1 t2) = 1+cantidadNodos t1+cantidadNodos t2;
--Suma de todos los nodos de un binTree de tipo int
sumaNodos::BinTree Int->Int
sumaNodos Empty = 0
sumaNodos (Node n t1 t2) = n+sumaNodos t1+sumaNodos t2;
--Ej 3 Dado un arbol y un predicado 
-- a retornar si todos los nodos cumplen el predicado
todosNodosCumplen::BinTree a ->(a->Bool)->Bool
todosNodosCumplen Empty p = True
todosNodosCumplen (Node a t1 t2) p = (p a) && todosNodosCumplen t1 p && todosNodosCumplen t2 p;
-- b retornar si existe un nodo que cumpla el predicado
algunoQueCumpla::BinTree a-> (a->Bool)->Bool
algunoQueCumpla Empty p = False
algunoQueCumpla (Node a t1 t2) p = (p a) || algunoQueCumpla t1 p || algunoQueCumpla t2 p;
arbolPrueba::BinTree Int
arbolPrueba = Node 1 (Node 2 (Empty) (Empty)) (Node 3 (Empty) (Empty));
--Ej 4 linealice un arbol en pasar a lista
--a PreOrden -> Medio izquierdo derecho -> [1,2,3]\
aListaPreOrden::BinTree a ->[a]
aListaPreOrden Empty = []
aListaPreOrden (Node a t1 t2 ) = a:[]++aListaPreOrden t1 ++ aListaPreOrden t2;
-- idem pero inorden izquierda medio derecha 2,1,3
aListaInorden::BinTree a ->[a]
aListaInorden Empty = []
aListaInorden ( Node a t1 t2 ) = aListaInorden t1 ++ a:[] ++ aListaInorden t2;
-- idem pero izq der medio 2,3,1
aListaOrden::BinTree a -> [a]
aListaOrden Empty = []
aListaOrden (Node a t1 t2 ) = aListaOrden t1 ++ aListaOrden t2 ++ a:[];
-- Cambiamos a arbol expresion aritmetica ExpArit
-- 1- Programar una funcion que retorne la cantidad de nodes binarios

data ExpArit = K Int| N ExpArit | Mas ExpArit ExpArit| Por ExpArit ExpArit
    deriving (Show)

arbPrueba::ExpArit 
arbPrueba= Mas ( (K 7)) ((N(K 3)));
cantNodosBinarios::ExpArit->Int
cantNodosBinarios (K n) = 0
cantNodosBinarios (N t1) = cantNodosBinarios t1
cantNodosBinarios (Mas t1 t2 ) = 1+cantNodosBinarios t1+cantNodosBinarios t2
cantNodosBinarios  (Por t1 t2) = 1 + cantNodosBinarios t1 + cantNodosBinarios t2;
--Programar una funcion que evalue un ExpArit
evaluarExpArit::ExpArit->Int
evaluarExpArit ( K n ) = n
evaluarExpArit ( N t1) = - (evaluarExpArit t1)
evaluarExpArit ( Mas t1 t2) = evaluarExpArit t1 + evaluarExpArit t2
evaluarExpArit ( Por t1 t2) = evaluarExpArit t1 * evaluarExpArit t2;
-- Programar una funcion que elimine la doble negacion
eliminarDobleNegacion::ExpArit->ExpArit
eliminarDobleNegacion (K n) = ( K n)
eliminarDobleNegacion (N (N (K n)) ) = (K n)
eliminarDobleNegacion (N (N t1)) = t1
eliminarDobleNegacion (N t1) = ( N t1)
eliminarDobleNegacion ( Mas t1 t2) = Mas (eliminarDobleNegacion t1) ( eliminarDobleNegacion t2)
eliminarDobleNegacion ( Por t1 t2 ) = Por ( eliminarDobleNegacion t1) ( eliminarDobleNegacion t2);



--Repartido ejercicios arboles
data AB a = Hoja a | Nodo (AB a) (AB a)
    deriving(Show)
arbol::AB Bool
arbol = Nodo ( Nodo (Hoja True) (Nodo (Hoja False) (Hoja False))) (Hoja True);
-- que calcula la cantidad de hojas que tiene un ´arbol binario
cantHojas::AB a -> Int
cantHojas (Hoja a) = 1
cantHojas (Nodo t1 t2) = cantHojas t1 + cantHojas t2;
--que calcula la cantidad de nodos internos que tiene un ´arbol binario
cantNodos::AB a -> Int
cantNodos (Hoja a )= 0
cantNodos (Nodo t1 t2) = 1+ cantNodos t1 + cantNodos t2;
-- hojas::AB a -> [a], que devuelve una lista con las hojas de un ´arbol binario
hojas::AB a -> [a]
hojas (Hoja a) = a:[]
hojas (Nodo t1 t2) = hojas t1 ++ hojas t2;
--que devuelve el ´arbol espejo de un ´arbol binario (o sea, otro con los
--mismos elementos, pero con todos los sub´arboles transpuestos)
espejo::AB a -> AB a
espejo (Hoja a) = (Hoja a)
espejo (Nodo t1 t2 ) = Nodo ( espejo t2 ) (espejo t1);

